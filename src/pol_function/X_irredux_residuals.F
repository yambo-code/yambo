!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS,AM,AF,IM
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
!> @callgraph
!> @callergraph
subroutine X_irredux_residuals(Xen,Xk,X,Dip,i_cg,iq,Xo_res,Xo_scatt)
 !
 ! This subroutine must be kept thread-safe, since it is used
 ! inside an external omp loop.
 ! This means mostly that variables from modules should be used as
 ! intent(IN) and not modified.
 !
 use pars,          ONLY:SP,cZERO,cONE
 use wrapper,       ONLY:V_by_V_plus_V
 use DIPOLES,       ONLY:DIPOLE_t,DIP_rotated
 use X_m,           ONLY:X_t,X_poles_tab,l_X_terminator,X_cols,X_rows,drude_n_states
 use X_m,           ONLY:rhotw_save_d, rhotw_save2_d
 use electrons,     ONLY:levels,spin_occ
 use frequency,     ONLY:bare_grid_N,coarse_grid_pt
 use collision_el,  ONLY:elemental_collision
 use D_lattice,     ONLY:nsym,DL_vol,i_time_rev,sop_inv
 use R_lattice,     ONLY:g_rot,qindx_X,bz_samp,G_m_G,q0_def_norm
 use R_lattice,     ONLY:g_rot_d,G_m_G_d
 use vec_operate,   ONLY:v_norm
 use collision_el,  ONLY:elemental_collision
 use deviceXlib_m
 !
#include<dev_defs.h>
 !
 implicit none
 !
 type(elemental_collision), target :: Xo_scatt
 type(levels), intent(in) :: Xen
 type(bz_samp),intent(in) :: Xk
 type(X_t),    intent(in) :: X
 type(DIPOLE_t),intent(in):: Dip
 integer,      intent(in) :: i_cg,iq
 complex(SP),  intent(out) DEV_ATTRIBUTE :: Xo_res(X_rows(1):X_rows(2),X_cols(1):X_cols(2))
 !
 ! Work sapce
 !
 ! AF: rhotw_save defined as automatic goes to stack and can harm 
 !     (stack overflow, causing random crashes of the code)
 !     it would be best to define it as allocatable (less efficient if too
 !     many calls are done, though)
 !
 complex(SP)        :: rhotw_save(Xo_scatt%ngrho),rhotw_save2(Xo_scatt%ngrho),Z_
#ifdef _CUDA
 complex(SP), pointer, device :: Xo_scatt_rhotw_d(:)
 integer            :: sop_inv_l,X_ng
#endif
 real(SP)           :: Z_eh_occ
 logical            :: l_X_term_vv
 integer            :: ngrho
 integer            :: ig1,ig2,ik,is,ikp,ikbz,ikpbz,i_spin,&
&                      isp,iv,ic,isave(4),n_poles,i_bg,ig_start,ROWS(2)
 !
 ! DIPOLES
 !
 complex(SP)        :: DIP_projected,field_dir(3)
 !
 field_dir=Dip%q0/v_norm(Dip%q0)*q0_def_norm
 !
 isave     = 0
 n_poles   = sum(bare_grid_N(1:i_cg-1))
 Xo_res    = cZERO
 Z_        = cONE
 !
 ngrho     = Xo_scatt%ngrho
 !
 loop_bare_grid: do i_bg = 1,bare_grid_N(i_cg)
   !
   n_poles=n_poles+1
   !
   ! Scattering geometry
   !---------------------
   !
   ikbz   = X_poles_tab(n_poles,1)
   iv     = X_poles_tab(n_poles,2)
   ic     = X_poles_tab(n_poles,3)
   i_spin = X_poles_tab(n_poles,4)
   !
   ikpbz  = qindx_X(iq,ikbz,1)
   !
   ik = Xk%sstar(ikbz,1)
   is = Xk%sstar(ikbz,2)
   !
   ikp= Xk%sstar(ikpbz,1)
   isp= Xk%sstar(ikpbz,2)
   !
   l_X_term_vv = (l_X_terminator.and.ic>=X%ib(1).and.ic<=Xen%nbm)
   !
   ! take into account the cancellation of terms
   ! occurring when dealing with the wings
   ig_start=X_rows(1)
   if (iq==1) ig_start= max(2,ig_start)
   !
#ifdef _CUDA
   X_ng=X%ng
#endif
   !
   ! Note the renormalization of the Z_eh_occ=f(1-f)*Z factor
   !
   if (allocated(Xen%Z))      Z_=Xen%Z(ic,ik,i_spin)*Xen%Z(iv,ikp,i_spin)
   !
   ! GPL_EXCLUDE_START
   !
   if (allocated(Xen%GreenF)) Z_=cONE
   !
   ! GPL_EXCLUDE_END
   !
   Z_eh_occ = Xen%f(iv,ikp,i_spin)*(spin_occ-Xen%f(ic,ik,i_spin))/spin_occ/real(Xk%nbz,SP)/DL_vol*real(Z_)
   !
   if (iq==1.and.abs(coarse_grid_Pt(i_cg))<Dip%Energy_treshold) Z_eh_occ=1._SP
   !
   if (l_X_term_vv) Z_eh_occ = Xen%f(iv,ikp,i_spin)*Xen%f(ic,ik,i_spin)/spin_occ/real(Xk%nbz,SP)/DL_vol*real(Z_)
   !
   ! Scattering CALL
   !-----------------
   !
#ifdef _CUDA
   Xo_scatt_rhotw_d => Xo_scatt%rhotw_d
#endif
   !
   if (iq==1) then
     !
     ! iq=1: Symmetries are applyed later and
     !       iG=1 case is computed separately 
     !
     Xo_scatt%is = (/ic,ik,1,i_spin/)
     Xo_scatt%os = (/iv,ik,1,i_spin/)
     Xo_scatt%qs = (/1,1,1/)
     if (.not. X%ng==1) then
       if ( any((/isave(1)/=iv,isave(2)/=ic,isave(3)/=ik,isave(4)/=i_spin/)) ) then
         !
         call DEV_SUBNAME(scatter_Bamp)(Xo_scatt)
         !
         ! rhotw_save=Xo_scatt%rhotw
         call dev_memcpy(DEV_VARNAME(rhotw_save),DEV_VARNAME(Xo_scatt%rhotw))
         !
         isave=(/iv,ic,ik,i_spin/)
         !
       endif
       !
#ifdef _CUDA
       sop_inv_l=sop_inv(is)
       !
       !$cuf kernel do(1) <<<*,*>>>
       do ig1=2,X_ng
         ig2=g_rot_d(ig1,sop_inv_l)
         Xo_scatt_rhotw_d(ig1)=rhotw_save_d(ig2)
       enddo
#else
       do ig1=2,X%ng
         ig2=g_rot(ig1,sop_inv(is))
         Xo_scatt%rhotw(ig1)=rhotw_save(ig2)
       enddo
#endif
       !
       if (is>nsym/(i_time_rev+1)) then
         call dev_conjg( DEV_VARNAME(Xo_scatt%rhotw) )
       endif
       !
     endif
     !
     ! iG=1 case
     DIP_projected=dot_product(field_dir,DIP_rotated(ic,iv,ikbz,i_spin,"DIP_iR",Xk))
     if(iv/=ic) DEV_VARNAME(Xo_scatt%rhotw)(1)=    -conjg(DIP_projected)
     if(iv==ic) DEV_VARNAME(Xo_scatt%rhotw)(1)=cONE-conjg(DIP_projected)
     !
     if (l_X_term_vv.and.iv==ic) DEV_VARNAME(Xo_scatt%rhotw)(1)=0.0_SP
     !
   else
     !
     ! iq>1: direct computation
     !
     Xo_scatt%is=(/ic,ik,is,i_spin/)
     Xo_scatt%os=(/iv,ikp,isp,i_spin/)
     Xo_scatt%qs=(/qindx_X(iq,ikbz,2),iq,1/)
     !
     call DEV_SUBNAME(scatter_Bamp)(Xo_scatt)
     !
   endif
   !
   ! Filling the upper triangular part of the residual here ! 
   !-------------^^^^^---------------------------------------
#ifdef _CUDA
   !$cuf kernel do(2) <<<*,*>>>
   do ig2=X_cols(1),X_cols(2)
     do ig1=X_rows(1),X_rows(2)
       if (ig1 <= ig2) then
         Xo_res(ig1,ig2) = Xo_res(ig1,ig2) + Z_eh_occ*Xo_scatt_rhotw_d(ig2) &
&                                          * conjg(Xo_scatt_rhotw_d(ig1))
       endif
     enddo
   enddo
#else
   !$omp parallel do default(shared), private(ig2)
   do ig2=X_cols(1),X_cols(2)
     ROWS=(/X_rows(1),min(ig2,X_rows(2))/)
     call V_by_V_plus_V(ROWS(2)-ROWS(1)+1,Z_eh_occ*Xo_scatt%rhotw(ig2),&
&                       conjg(Xo_scatt%rhotw(ROWS(1):ROWS(2))),Xo_res(ROWS(1):ROWS(2),ig2))
   enddo
   !$omp end parallel do
#endif
   !
   ! add terminator specific corrections
   ! (ic is running in valence)
   !
   if (l_X_term_vv.and.iv==ic) then 
     !
     Xo_scatt%is = (/iv,ik,1,i_spin/)
     Xo_scatt%os = (/iv,ik,1,i_spin/)
     Xo_scatt%qs = (/1,1,1/)
     !
     call dev_memcpy(DEV_VARNAME(rhotw_save2),DEV_VARNAME(Xo_scatt%rhotw))
     !
     if (X%ng==1) then
       DEV_VARNAME(Xo_scatt%rhotw)(1)=cONE 
     else
       call DEV_SUBNAME(scatter_Bamp)(Xo_scatt)
     endif
     !
     ! symm
     ! rhotw_save=Xo_scatt%rhotw
     call dev_memcpy(DEV_VARNAME(rhotw_save),DEV_VARNAME(Xo_scatt%rhotw))
     !
#ifdef _CUDA
     sop_inv_l=sop_inv(is)
     !
     !$cuf kernel do(1) <<<*,*>>>
     do ig1=1,X_ng
       ig2=g_rot_d(ig1,sop_inv_l)
       Xo_scatt_rhotw_d(ig1)=rhotw_save_d(ig2)
     enddo
#else
     !$omp parallel do default(shared), private(ig1,ig2)
     do ig1=1,X%ng
       ig2=g_rot(ig1,sop_inv(is))
       Xo_scatt%rhotw(ig1)=rhotw_save(ig2)
     enddo
     !$omp end parallel do
#endif
     !
     if (is>nsym/(i_time_rev+1)) then
       call dev_conjg(DEV_VARNAME(Xo_scatt%rhotw))
     endif
     !
#ifdef _CUDA
     !$cuf kernel do(2) <<<*,*>>>
     do ig2=X_cols(1),X_cols(2)
       do ig1=ig_start,X_rows(2)
         if (ig1 > ig2) cycle
         Xo_res(ig1,ig2)=Xo_res(ig1,ig2)-Z_eh_occ*Xo_scatt_rhotw_d(G_m_G_d(ig2,ig1))
       enddo
     enddo
#else
     !$omp parallel do default(shared), private(ig1,ig2)
     do ig2=X_cols(1),X_cols(2)
       do ig1=ig_start,min(ig2,X_rows(2))
         Xo_res(ig1,ig2)=Xo_res(ig1,ig2)-Z_eh_occ*Xo_scatt%rhotw(G_m_G(ig2,ig1))
       enddo
     enddo
     !$omp end parallel do 
#endif
     !
     call dev_memcpy(DEV_VARNAME(Xo_scatt%rhotw),DEV_VARNAME(rhotw_save2))
     !
   endif
   !
 enddo loop_bare_grid
 !
 if (iq==1.and.abs(coarse_grid_Pt(i_cg))<Dip%Energy_treshold) then
#ifdef _CUDA
   !$cuf kernel do(2) <<<*,*>>>
#else
   !$omp parallel do default(shared), private(ig1,ig2)
#endif
   do ig2=X_cols(1),X_cols(2)
     do ig1=ig_start,X_rows(2)
       if (ig1 > ig2) cycle
       Xo_res(ig1,ig2)=Xo_res(ig1,ig2)/real(drude_n_states,SP)
     enddo
   enddo
#ifndef _CUDA
   !$omp end parallel do 
#endif
 endif
 !
end subroutine X_irredux_residuals

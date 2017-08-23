!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine X_irredux_residuals(Xen,Xk,X,i_cg,iq,Xo_res,Xo_scatt)
 !
 ! This subroutine must be kept thread-safe, since it is used
 ! inside an external omp loop.
 ! This means mostly that variables from modules should be used as
 ! intent(IN) and not modified.
 !
 use pars,          ONLY:SP,cZERO,cONE
 use wrapper,       ONLY:V_by_V_plus_V
 use X_m,           ONLY:X_t,DIP_projected,X_poles_tab,l_X_terminator 
 use parallel_m,    ONLY:PAR_Xk_bz_index
 use electrons,     ONLY:levels,spin_occ
 use frequency,     ONLY:bare_grid_N
 use D_lattice,     ONLY:nsym,DL_vol,i_time_rev,sop_inv
 use R_lattice,     ONLY:g_rot,qindx_X,bz_samp,G_m_G
 use collision_el,  ONLY:elemental_collision
 !
 implicit none
 !
 type(elemental_collision), intent(inout) :: Xo_scatt
 type(levels), intent(in) :: Xen
 type(bz_samp),intent(in) :: Xk
 type(X_t),    intent(in) :: X
 integer,      intent(in) :: i_cg,iq
 complex(SP),  intent(out):: Xo_res(X%ng,X%ng)
 !
 ! Work sapce
 !
 ! AF: rhotw_save defined as automatic goes to stack and can harm 
 !     (stack overflow, causing random crashes of the code)
 !     it would be best to define it as allocatable (less efficient if too
 !     many calls are done, though)
 complex(SP)              :: rhotw_save(Xo_scatt%ngrho),rhotw_save2(Xo_scatt%ngrho),Z_
 real(SP)                 :: Z_eh_occ
 logical                  :: l_X_term_vv
 integer                  :: ig1,ig2,ik,is,ikp,ikbz,ikpbz,i_spin,ik_bz_mem,&
&                            isp,iv,ic,isave(4),n_poles,i_bg,ig_start
 !
 isave=0
 !
 n_poles = sum(bare_grid_N(1:i_cg-1))
 !
 Xo_res(:,:) = cZERO
 !
 Z_=cONE
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
   !
   Z_eh_occ = Xen%f(iv,ikp,i_spin)*(spin_occ-Xen%f(ic,ik,i_spin))/&
&             spin_occ/real(Xk%nbz,SP)/DL_vol*real(Z_)
   !
   if (l_X_term_vv) then
     Z_eh_occ = Xen%f(iv,ikp,i_spin)*Xen%f(ic,ik,i_spin)/ &
&               spin_occ/real(Xk%nbz,SP)/DL_vol*real(Z_)
   endif  
   !
   ! Scattering CALL
   !-----------------
   !
   if (iq==1) then
     Xo_scatt%is = (/ic,ik,1,i_spin/)
     Xo_scatt%os = (/iv,ik,1,i_spin/)
     Xo_scatt%qs = (/1,1,1/)
     ik_bz_mem=PAR_Xk_bz_index(ikbz)
     if (.not. X%ng==1) then
       if ( any((/isave(1)/=iv,isave(2)/=ic,isave(3)/=ik,isave(4)/=i_spin/)) ) then
         call scatter_Bamp(Xo_scatt)
         rhotw_save=Xo_scatt%rhotw
         isave=(/iv,ic,ik,i_spin/)
       endif
       do ig1=1,X%ng
         ig2=g_rot(sop_inv(is),ig1)
         Xo_scatt%rhotw(ig1)=rhotw_save(ig2)
       enddo
       if (is>nsym/(i_time_rev+1)) then
         do ig1=1,X%ng
           Xo_scatt%rhotw(ig1)=conjg(Xo_scatt%rhotw(ig1))
         enddo
       endif
     endif
     !
     ! the iv==ic case (XTerm) is dropped because of a
     ! cancellation with another XTerm contribution to wings and head
     ! (that, in fact, is not included below, see ig_start=2)
     !
     Xo_scatt%rhotw(1)=-conjg(DIP_projected(ic,iv,ik_bz_mem,i_spin))
     if (l_X_term_vv.and.iv==ic) Xo_scatt%rhotw(1)=0.0_SP
     !
   else
     Xo_scatt%is=(/ic,ik,is,i_spin/)
     Xo_scatt%os=(/iv,ikp,isp,i_spin/)
     Xo_scatt%qs=(/qindx_X(iq,ikbz,2),iq,1/)
     call scatter_Bamp(Xo_scatt)
   endif
   !
   ! Filling the upper triangular part of the residual here ! 
   !-------------^^^^^---------------------------------------
#if defined _OPENMP
   !$omp parallel do default(shared), private(ig2)
#endif
   do ig2=1,X%ng
     call V_by_V_plus_V(ig2,Z_eh_occ*Xo_scatt%rhotw(ig2),conjg(Xo_scatt%rhotw(:ig2)),Xo_res(:ig2,ig2))
   enddo
#if defined _OPENMP
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
     rhotw_save2 = Xo_scatt%rhotw
     !
     if (X%ng==1) then
       Xo_scatt%rhotw(1)=cONE 
     else
       call scatter_Bamp(Xo_scatt)
     endif
     !
     ! symm
     rhotw_save=Xo_scatt%rhotw
     !
#if defined _OPENMP
     !$omp parallel do default(shared), private(ig1,ig2)
#endif
     do ig1=1,X%ng
       ig2=g_rot(sop_inv(is),ig1)
       Xo_scatt%rhotw(ig1)=rhotw_save(ig2)
     enddo
#if defined _OPENMP
     !$omp end parallel do
#endif
     !
     if (is>nsym/(i_time_rev+1)) then
       do ig1=1,X%ng
         Xo_scatt%rhotw(ig1)=conjg(Xo_scatt%rhotw(ig1))
       enddo
     endif
     !
     ! take into account the cancellation of terms
     ! occurring when dealing with the wings
     ig_start=1
     if(iq==1) ig_start=2
     !
#if defined _OPENMP
     !$omp parallel do default(shared), private(ig1,ig2)
#endif
     do ig2=1,X%ng
     do ig1=ig_start,ig2
       Xo_res(ig1,ig2)=Xo_res(ig1,ig2)-Z_eh_occ*Xo_scatt%rhotw(G_m_G(ig2,ig1))
     enddo
     enddo
#if defined _OPENMP
     !$omp end parallel do 
#endif
     !
     Xo_scatt%rhotw = rhotw_save2
     !
   endif
   !
 enddo loop_bare_grid
 !
end subroutine X_irredux_residuals

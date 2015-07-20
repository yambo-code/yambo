!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS,AM,AF
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
subroutine X_os_residuals(Xen,Xk,X,i_cg,iq,Xo_res)
 !
 use pars,          ONLY:SP,cZERO
 use wrapper,       ONLY:V_by_V_plus_V
 use X_m,           ONLY:X_t,DIP_projected,X_poles_tab,global_gauge !,X_terminator
 use parallel_m,    ONLY:PAR_Xk_bz_index !,HEAD_QP_cpu
 use electrons,     ONLY:levels,spin_occ
 use frequency,     ONLY:bare_grid_N,coarse_grid_Pt
 use collision,     ONLY:ggwinfo,collision_reset,scatterBamp_alloc,scatterBamp_free
 use D_lattice,     ONLY:nsym,DL_vol,i_time_rev,sop_inv
 use R_lattice,     ONLY:g_rot,qindx_X,bz_samp,q0_def_norm !,G_m_G
 !
 implicit none
 !
 type(levels), intent(in) :: Xen
 type(bz_samp),intent(in) :: Xk
 type(X_t),    intent(in) :: X
 integer,      intent(in) :: i_cg,iq
 complex(SP),  intent(out):: Xo_res(X%ng,X%ng)
 !
 ! Work sapce
 !
 type(ggwinfo)            :: isc
 !
 complex(SP)              :: rhotw_save(X%ng) !,eet_factor(X%ng,X%ng)
 real(SP)                 :: eh_occ
 integer                  :: ig1,ig2,ik,is,ikp,ikbz,ikpbz,i_spin,ik_bz_mem,&
&                            isp,iv,ic,isave(4),n_poles,i_bg !,alloc_err
 !
 ! prepare the scattering module
 !
 call collision_reset(isc,INIT_ONLY=.true.)
 !
 isc%ngrho=X%ng
 allocate(isc%rhotw(X%ng))
 call scatterBamp_alloc(isc)
 !
 isave=0
 !
 n_poles = sum(bare_grid_N(1:i_cg-1))
 !
 Xo_res(:,:) = cZERO
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
   ! Note the renormalization of the eh_occ=f(1-f) factor
   !
   !     n_spin n_sp_pol n_spinor  spin_occ eh_occ
   !       1        1        1         2      2
   !       2        1        2         1      1
   !       2        2        1         1      1
   !
   eh_occ = Xen%f(iv,ikp,i_spin)*(spin_occ-Xen%f(ic,ik,i_spin))/spin_occ/real(Xk%nbz)/DL_vol
   !
   ! Scattering CALL
   !-----------------
   !
   if (iq==1) then
     isc%is = (/ic,ik,1,i_spin/)
     isc%os = (/iv,ik,1,i_spin/)
     isc%qs = (/1,1,1/)
     ik_bz_mem=PAR_Xk_bz_index(ikbz)
     if (.not. X%ng==1) then
       if ( any((/isave(1)/=iv,isave(2)/=ic,isave(3)/=ik,isave(4)/=i_spin/)) ) then
         call scatterBamp(isc)
         rhotw_save=isc%rhotw
         isave=(/iv,ic,ik,i_spin/)
       endif
       do ig1=1,X%ng
         ig2=g_rot(sop_inv(is),ig1)
         isc%rhotw(ig1)=rhotw_save(ig2)
       enddo
       if (is>nsym/(i_time_rev+1)) then
         do ig1=1,X%ng
           isc%rhotw(ig1)=conjg(isc%rhotw(ig1))
         enddo
       endif
     endif
     isc%rhotw(1)=-conjg(DIP_projected(ic,iv,ik_bz_mem,i_spin))
     if(trim(global_gauge)=="velocity") isc%rhotw(1)=isc%rhotw(1)/coarse_grid_Pt(i_cg)*q0_def_norm
   else
     isc%is=(/ic,ik,is,i_spin/)
     isc%os=(/iv,ikp,isp,i_spin/)
     isc%qs=(/qindx_X(iq,ikbz,2),iq,1/)
     call scatterBamp(isc)
   endif
   !
   ! Davide: these lines seems useless ...
   !
   !if (X_terminator) then
   !  !
   !  if ( HEAD_QP_cpu ) then
   !    !$omp parallel do default(shared), private(ig1,ig2)
   !    do ig1=1,X%ng
   !      do ig2=1,X%ng
   !          eet_factor(ig1,ig2)=isc%rhotw(G_m_G(ig1,ig2))
   !      enddo
   !    enddo
   !    !$omp end parallel do
   !  endif
   !  ! 
   !endif
   !
   ! Filling the upper triangular part of the residual here ! 
   !-------------^^^^^---------------------------------------
   !$omp parallel do default(shared), private(ig2)
   do ig2=1,X%ng
     call V_by_V_plus_V(ig2,eh_occ*isc%rhotw(ig2),conjg(isc%rhotw(:ig2)),Xo_res(:ig2,ig2))
   enddo
   !$omp end parallel do
   !
 enddo loop_bare_grid
 !
 !
 ! Cleaning workspace
 !-------------------------
 deallocate(isc%rhotw)
 call scatterBamp_free(isc)
 call collision_reset(isc)
 !
end subroutine X_os_residuals

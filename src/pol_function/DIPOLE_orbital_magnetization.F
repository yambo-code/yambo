!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine Dipole_orbital_magnetization(Xen,Xk,X)
 !
 ! This routine returns <L>
 !
 use pars,                ONLY:SP,cZERO,cI,pi
 use units,               ONLY:SPEED_OF_LIGHT
 use LIVE_t,              ONLY:live_timing
 use electrons,           ONLY:levels,n_sp_pol
 use parallel_m,          ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index
 use vec_operate,         ONLY:cross_product
 use R_lattice,           ONLY:bz_samp
 use X_m,                 ONLY:X_t,DIP_iR,DIP_orbital
 use com,                 ONLY:error
 !
 implicit none
 !
 type(levels),  intent(in)    :: Xen
 type(bz_samp), intent(in)    :: Xk
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 logical                      :: l_mat_eval_DIP(X%ib_lim(2):X%ib(2),X%ib(1):X%ib_lim(1))
 integer                      :: ik,ic,iv,ib,i_sp_pol,i_spin,n_LT_steps,ik_mem
 real(SP)                     :: Eb,Ev,Ec
 complex(SP)                  :: DIP_pos(3,2)
 !
 ! Parallelization Mask 
 !======================
 ! Depending on the environment DIPOLES are distributed in different ways
 !
 call DIPOLE_build_parallelization_mask(X,l_mat_eval_DIP,n_LT_steps)
 !
 if (n_LT_steps>0) call live_timing('Dipoles: orbital magnetization',n_LT_steps)
 !
 ! Main loop over k in IBZ
 !
 do ik=1,Xk%nibz
   !
   if (n_LT_steps==0) cycle
   ! 
   if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
   !
   ik_mem=PAR_DIPk_ibz_index(ik)
   ! 
   do i_sp_pol=1,n_sp_pol
     do iv=X%ib(1),X%ib_lim(1) 
       Ev=Xen%E(iv,ik,i_sp_pol)
       do ic=max(iv,X%ib_lim(2)),X%ib(2)
         Ec=Xen%E(ic,ik,i_sp_pol)
         !
         if (.not.l_mat_eval_DIP(ic,iv)) cycle
         !
         do ib=X%ib(1),X%ib(2)
           !
           Eb=Xen%E(ib,ik,i_sp_pol)
           if (allocated(Xen%Eo)) Eb=Xen%Eo(ib,ik,i_sp_pol)
           !
           if (iv< ib) DIP_pos(:,1)=conjg(-cI*DIP_iR(:,ib,iv,ik_mem,i_sp_pol))
           if (iv>=ib) DIP_pos(:,1)=      -cI*DIP_iR(:,iv,ib,ik_mem,i_sp_pol)
           if (ic< ib) DIP_pos(:,2)=conjg(-cI*DIP_iR(:,ib,ic,ik_mem,i_sp_pol))
           if (ic>=ib) DIP_pos(:,2)=      -cI*DIP_iR(:,ic,ib,ik_mem,i_sp_pol)
           !
           ! Local part
           !
           DIP_orbital(:,ic,iv,ik_mem,i_sp_pol,1)=DIP_orbital(:,ic,iv,ik_mem,i_sp_pol,1) &
&                                                -cI*Eb*cross_product(DIP_pos(:,1),DIP_pos(:,2))
           !
           ! Itinerant part
           !
           DIP_orbital(:,ic,iv,ik_mem,i_sp_pol,2)=DIP_orbital(:,ic,iv,ik_mem,i_sp_pol,2) &
&                                                -cI*(Ev+Ec)/2._SP*cross_product(DIP_pos(:,1),DIP_pos(:,2))
           !
         enddo
         !
         call live_timing(steps=1)
         !
       enddo    ! conduction band loop
     enddo      ! valence    band loop
     ! 
   enddo        ! sp_pol loop
   !
 enddo          ! k-points
 !
 call live_timing()
 !
end subroutine Dipole_orbital_magnetization

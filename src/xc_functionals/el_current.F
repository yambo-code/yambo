!
! Copyright (C) 2000-2012 D. Sangalli and the YAMBO team
!              http://www.yambo-code.org
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
subroutine el_current(en,Xk,ik,J,J_diam,J_Vnl_x,l_directions)
 !
 ! Electronic current: j = (q/m)* < [p -   (q/c) *A] >
 !  (q=-1,m=1) ==>       =  -1    < [p + (1/137.)*A] >  (Gaussian convention)
 !
 use pars,          ONLY:SP
 use wrapper,       ONLY:V_dot_V,V_by_V_plus_V
 use electrons,     ONLY:levels,n_spin,n_sp_pol,Boltz_f
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:DL_sop
 use FFT_m,         ONLY:fft_size,fft_rot_r
 use wave_func,     ONLY:wf_state,wf,wf_x,wf_y,wf_z
#if defined _MAGNETIC
 use magnetic,      ONLY:A_magn_x,A_magn_y,A_magn_z,MAG_landau, &
&                        new_pp,x,load_cc,free_cc
#endif
 use pseudo,        ONLY:pp_kbv_dim
 use units,         ONLY:SPEED_OF_LIGHT
 !
 implicit none
 type(bz_samp)::Xk
 type(levels) ::en       
 !
 integer  :: ik
 logical  :: l_directions(3)
 real(SP) :: J(fft_size,n_sp_pol,3)
 real(SP) :: J_diam(fft_size,n_sp_pol,3)
 real(SP) :: J_Vnl_x(fft_size,n_sp_pol,3)
 !
 ! GPL_EXCLUDE_START 
 !
 ! Work Space 
 !
 integer     :: i_b,i_spin,ifft,i_wf,i1,i2,is
 real(SP)    :: J_dump(fft_size,n_sp_pol,3)
 real(SP)    :: J_dump_diam(fft_size,n_sp_pol,3)
 real(SP)    :: J_dump_Vnl_x(fft_size,n_sp_pol,3)
 complex(SP) :: J_dump_x_right(fft_size,3),J_dump_x_left(fft_size,3)
 complex(SP) :: int_x_right(3),int_x_left(3)
 real(SP)    :: pre_factor
 !
 J_dump=0._SP
 J_dump_diam=0._SP
 J_dump_Vnl_x=0._SP
 !
 call load_cc(.true.,.true.)
 !
 do i_b=en%nbf+1,en%nbm
 !If I want to compute persistent currents instead I have to loop on all bands 
 !do i_b=1,en%nbm
   do i_spin=1,n_spin
     !
     i_wf=wf_state(i_b,ik,i_spin)
     !
     do i1=1,Xk%nstar(ik)
       !
       is=Xk%star(ik,i1)
       !
       pre_factor=Boltz_f(i_b,ik,is,i_spin)/float(Xk%nbz)
       !
       ! Paramagnetic term of the current
       !
       if (l_directions(1)) J_dump(:,i_spin,1)=pre_factor*real(wf(:,i_wf)*conjg(wf_x(:,i_wf)))
       if (l_directions(2)) J_dump(:,i_spin,2)=pre_factor*real(wf(:,i_wf)*conjg(wf_y(:,i_wf)))
       if (l_directions(3)) J_dump(:,i_spin,3)=pre_factor*real(wf(:,i_wf)*conjg(wf_z(:,i_wf)))
       !
       ! Diamagnetic  term of the current
       !
#if defined _MAGNETIC
       if(MAG_landau) then
         if(allocated(A_magn_x)) J_dump_diam(:,i_spin,1)=pre_factor*abs(wf(:,i_wf))**2*A_magn_x(:)/SPEED_OF_LIGHT
         if(allocated(A_magn_y)) J_dump_diam(:,i_spin,2)=pre_factor*abs(wf(:,i_wf))**2*A_magn_y(:)/SPEED_OF_LIGHT
         if(allocated(A_magn_z)) J_dump_diam(:,i_spin,3)=pre_factor*abs(wf(:,i_wf))**2*A_magn_z(:)/SPEED_OF_LIGHT
       endif
#endif
       !
       ! [x,V_nl]     term of the current
       !
       !              V_nl(r,r')= sum_(i=1,pp_kbv_dim) f_i(r)* f_i(r')
       !
       J_dump_x_right(:,:)=(0._SP,0._SP)   ! = sum_i f_i(r)*      A_i
       J_dump_x_left(:,:)=(0._SP,0._SP)    ! = sum_i x(r) f_i(r)* B_i
       !
       do i2=1,pp_kbv_dim
         if (l_directions(1)) then
           int_x_right(1)=V_dot_V(fft_size,wf(:,i_wf),new_pp(:,i2)*x(:,1))  ! A_i = int dr' f_i(r') x(r') psi_l(r')
           int_x_left(1)=V_dot_V(fft_size,wf(:,i_wf),new_pp(:,i2))          ! B_i = int dr' f_i(r')       psi_l(r')
           !
           call V_by_V_plus_V(fft_size,int_x_right(1),conjg(new_pp(:,i2)),J_dump_x_right(:,1))       ! C(r') = C(r') + f_i(r)*      A_i
           call V_by_V_plus_V(fft_size,int_x_left(1),x(:,1)*conjg(new_pp(:,i2)),J_dump_x_left(:,1))  ! D(r') = D(r') + x(r) f_i(r)* B_i
           !
         endif
         if (l_directions(2)) then
           int_x_right(2)=V_dot_V(fft_size,wf(:,i_wf),new_pp(:,i2)*x(:,2))
           int_x_left(2)=V_dot_V(fft_size,wf(:,i_wf),new_pp(:,i2))
           !
           call V_by_V_plus_V(fft_size,int_x_right(2),conjg(new_pp(:,i2)),J_dump_x_right(:,2))
           call V_by_V_plus_V(fft_size,int_x_left(2),x(:,2)*conjg(new_pp(:,i2)),J_dump_x_left(:,2))
           !
         endif
       enddo
       ! J= (psi_l(r))* ( D(r) - C(r) )
       if (l_directions(1)) J_dump_Vnl_x(:,i_spin,1)=pre_factor*(conjg(wf(:,i_wf))*(J_dump_x_left(:,1)-J_dump_x_right(:,1) ) )
       if (l_directions(2)) J_dump_Vnl_x(:,i_spin,2)=pre_factor*(conjg(wf(:,i_wf))*(J_dump_x_left(:,2)-J_dump_x_right(:,2) ) )
       !
       ! To be fixed
       ! The current is a real (or polar) vector
       ! The spin is a pseudo (or axial) vector
       ! The spin-current must be transformed with both dl_sop and spin_sop
       !
       forall(ifft=1:fft_size) J(ifft,i_spin,:)=J(ifft,i_spin,:)  &
&                        -matmul( DL_sop(:,:,is), J_dump(fft_rot_r(is,ifft),i_spin,:) )
       forall(ifft=1:fft_size) J_diam(ifft,i_spin,:)=J_diam(ifft,i_spin,:)  &
&                        -matmul( DL_sop(:,:,is), J_dump_diam(fft_rot_r(is,ifft),i_spin,:) )
       forall(ifft=1:fft_size) J_Vnl_x(ifft,i_spin,:)=J_Vnl_x(ifft,i_spin,:)  &
&                        -matmul( DL_sop(:,:,is), J_dump_Vnl_x(fft_rot_r(is,ifft),i_spin,:) )
       !
     enddo
     !
   enddo
   !
 enddo
 !
 call free_cc()
 !
 ! GPL_EXCLUDE_END 
 !
end subroutine

!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine Vgrad_real_space_to_H(ik,H,WF,V_grad)
 !
 ! Updte of SOC interaction, i.e. last term in 
 ! Eq.(1) JCTC 11, 4870 (2015)
 !
 use electrons,      ONLY:n_spin,n_spinor,spin
 use pars,           ONLY:SP,cZERO
 use D_lattice,      ONLY:sigma_x,sigma_y,sigma_z
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:WAVEs,WF_x,WF_y,WF_z
 use wrapper,        ONLY:Vstar_dot_V
 use matrix_operate, ONLY:UP_matrix_index
 use hamiltonian,    ONLY:H_ref_bands
 use timing_m,       ONLY:timing
#if defined _RT || defined _SC
 use parallel_m,     ONLY:PAR_IND_B_mat_ordered
#endif
 implicit none
 !
 integer,     intent(in)    :: ik
 complex(SP), intent(in)    :: V_grad(fft_size,3)
 complex(SP), intent(inout) :: H(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 type(WAVEs), intent(in)    :: WF
 !
 ! Work Space
 !
 integer    :: i_spinor,j_spinor,ib,ibp,i_wf,ip_wf
 complex(SP):: c_ws(fft_size,n_spinor),c_ws_x(fft_size,n_spinor),c_ws_y(fft_size,n_spinor),c_ws_z(fft_size,n_spinor)
 !
 call timing('Vgrad_to_H',OPR='start')
 !
 c_ws  =cZERO
 !
 ! Fill ...
 !
 do ib=H_ref_bands(1),H_ref_bands(2)
   do ibp=ib,H_ref_bands(2)
   !
   if (.not.PAR_IND_B_mat_ordered%element_1D( UP_matrix_index(ib-H_ref_bands(1)+1,ibp-H_ref_bands(1)+1)-1 )) cycle
   !
   ! <i_wf|V|ip_wf>
   !
   ip_wf = WF%index(ibp,ik,1)
   !
   do j_spinor=1,n_spinor
     ! x
     c_ws_x(:,i_spinor)= V_grad(:,2)*WF_z(:,i_spinor,ip_wf)-V_grad(:,3)*WF_y(:,i_spinor,ip_wf)
     ! y
     c_ws_y(:,i_spinor)=-V_grad(:,1)*WF_z(:,i_spinor,ip_wf)+V_grad(:,3)*WF_x(:,i_spinor,ip_wf)
     ! z
     c_ws_z(:,i_spinor)= V_grad(:,1)*WF_y(:,i_spinor,ip_wf)-V_grad(:,2)*WF_x(:,i_spinor,ip_wf)
     !
   enddo
   !
   do j_spinor=1,n_spinor
     c_ws(:,j_spinor)=+sigma_x(j_spinor,1)*c_ws_x(:,1)+sigma_x(j_spinor,2)*c_ws_x(:,2) &
&                     +sigma_y(j_spinor,1)*c_ws_y(:,1)+sigma_y(j_spinor,2)*c_ws_y(:,2) &
&                     +sigma_z(j_spinor,1)*c_ws_z(:,1)+sigma_z(j_spinor,2)*c_ws_z(:,2)
   enddo
   !
   i_wf = WF%index(ib,ik,1)
   H(ib,ibp) = 0.25_SP*Vstar_dot_V(fft_size*n_spinor,WF%c(:,:,i_wf),c_ws(:,:))
   !
   if(ib==ibp) H(ib,ib) = real(H(ib,ib ),SP)
   if(ib/=ibp) H(ibp,ib)=conjg(H(ib,ibp)   )
   !
   enddo
 enddo
 !
 call timing('Vgrad_to_H',OPR='stop')
 !
end subroutine 

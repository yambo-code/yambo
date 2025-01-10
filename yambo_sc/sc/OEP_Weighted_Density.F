!
! License-Identifier: GPL
!
! Copyright (C) 2011 The Yambo Team
!
! Authors (see AUTHORS file for details): MG
!
subroutine OEP_Weighted_Density(en,Xk,rho)
 !
 ! Modified el_density sub needed for Bulat Levy modified 
 ! Slater and OEP approx/procedure. 
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:levels,n_sp_pol
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym
 use FFT_m,         ONLY:fft_size,fft_rot_r
 use wave_func,     ONLY:WF
 !
 implicit none
 type(bz_samp)::Xk
 type(levels) ::en       
 real(SP)     ::rho(fft_size)
 !
 ! Work Space 
 !
 integer :: i1,i2,ifft,i_sp
 real(SP):: rho_no_sym(fft_size),EWeight
 !
 rho=0._SP
 rho_no_sym=0._SP
 !
 if(n_sp_pol==2) call error("OEP not coded for n_sp_pol=2")
 i_sp=1
 !
 do i1=1,en%nbm(i_sp)
   do i2=1,Xk%nibz
     if (size(WF%index,1)<i1) cycle
     if (size(WF%index,2)<i2) cycle
     !
     ifft=WF%index(i1,i2,1)
     EWeight = en%e(i1,i2,1)+en%E_Fermi
     rho_no_sym(:)=rho_no_sym(:)+en%f(i1,i2,1)*Xk%weights(i2)/EWeight*abs(WF%c(:,1,ifft))**2
   enddo
 enddo
 !
 ! Symmetrization
 !
 do i1=1,nsym
   rho(:)=rho(:)+real(rho_no_sym(fft_rot_r(:,i1)),SP)/real(nsym,SP)
 enddo
 !
end subroutine

!
! License-Identifier: GPL
!
! Copyright (C) 2010 The Yambo Team
!
! Authors (see AUTHORS file for details): MG
!
subroutine OEP_Hole(E,k,Vslt,U_x)
 !
 !  Vhole(Slater) part of the exact exchange/static RPA potential 
 !  multiplied by the density 
 !
 !  Found as 
 !
 !  v_s(r) = \sum_k \sum_n phi^*_{n,k}(r)*U_x^{n,k}(r)
 !
 ! 
 use pars,            ONLY:SP,DP,schlen
 use D_lattice,       ONLY:nsym
 use electrons,       ONLY:levels,n_met_bands
 use R_lattice,       ONLY:bz_samp,nkibz
 use FFT_m,           ONLY:fft_size,fft_rot_r
 use wave_func,       ONLY:WF
 use SC,              ONLY:l_oep_EWeight
 !
 ! I/O
 !
 implicit none
 real(SP),  intent(out)   :: Vslt(fft_size)
 complex(SP),intent(in)   :: U_x(nkibz,maxval(n_met_bands),fft_size)
 type(levels), intent(in) :: E
 type(bz_samp), intent(in):: k
 !
 ! Work space
 !
 integer          ::iv1,ig,i1,ik,ifft
 real(DP)         ::Vslt_DP(fft_size)
 complex(DP)      ::f(fft_size)
 real(SP)         ::EWeight
 character(schlen)::ch
 !
 EWeight = 1._SP 
 Vslt_DP= 0._DP
 do iv1 = 1 ,E%nbm(1)
   do ik = 1,k%nibz
     if (l_oep_EWeight) EWeight = E%E(iv1,ik,1)+E%E_Fermi
     ifft=WF%index(iv1,ik,1)
     f(:)=conjg(WF%c(:,1,ifft))*U_x(ik,iv1,:)
     Vslt_DP = Vslt_DP + E%f(iv1,ik,1)*real(f,DP)*k%weights(ik)/EWeight
   end do
 end do
 Vslt= 0._SP
 !
 do i1=1,nsym
   Vslt(:fft_size)=Vslt(:fft_size)+real(Vslt_DP(fft_rot_r(:fft_size,i1)),SP)/real(nsym,SP)
 enddo
 !
end subroutine OEP_Hole

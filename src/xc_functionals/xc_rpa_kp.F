!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine xc_rpa_kp(rs,exc)
 !
 ! RPA+ : Kurth, Perdew PRB, 59 (1999)
 !
 use FFT_m,         ONLY:fft_size
 use pars,          ONLY:DP
 implicit none
 real(DP) :: rs(fft_size),exc(fft_size)
 !
 integer  :: i1 !ws
 !
 ! Parameters
 !
 real(DP), parameter :: A1=0.02586_DP,A2=0.03547_DP,A3=0.00292_DP
 !
 do i1=1,fft_size
   exc(i1)=A1/(1._DP+A2*sqrt(rs(i1))+A3*rs(i1))
 enddo
 !
end subroutine

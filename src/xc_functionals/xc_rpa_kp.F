!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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

!
! Copyright (C) 2000-2010 C. Attaccalite, A. Marini and the YAMBO team 
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
!
subroutine Fourier_Transform(freqs,fw,nw,ft,nt,dt,lda)
 !
 use pars,           ONLY:SP,DP,cI
 use timing,         ONLY:live_timing
 implicit none
 !
 integer,     intent(in)   :: nw,nt,lda
 real(SP),    intent(in)   :: dt
 complex(SP), intent(in)   :: freqs(nw)
 complex(DP), intent(out)  :: fw(nw,lda)
 complex(DP), intent(in)   :: ft(nt,lda)
 !
 ! Work Space 
 !
 integer  :: i_w,i_t
 real(SP) :: Time_reached
 !
 call live_timing('FFT:',nw)
 !
 fw=(0._DP,0._DP)
 !
 do i_w = 1, nw
   !
   do i_t = 1, nt
     Time_reached = i_t*dt       
     fw(i_w,:) = fw(i_w,:)+ft(i_t,:)*exp(cI*freqs(i_w)*Time_reached)
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
end subroutine Fourier_Transform


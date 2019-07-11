!
! Copyright (C) 2000-2019 A. Marini and the YAMBO team 
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
!=====================================================================
! Driver to 3D FFT from the QE suite of codes.
!
! fft_sign = +1  : G-space to R-space, output = \sum_G f(G)exp(+iG*R) (FW)
! fft_sign = -1  : R-space to G-space, output = \int_R f(R)exp(-iG*R) (BW)
!
! Note that as the YAMBO convention for the oscillators is
! 
!  <n k | e ^{iq.r} | n' k-q> 
!
! the +1 sign (FW) is used in scatter_Bamp as well.
!
! Note that that inverse operation of 
!
! call fft_3d(wf,fft_dim, 1,bw_plan)
!
! is
!
! call fft_3d(wf/real(fft_size,SP),fft_dim, 1,fw_plan)
!
!=====================================================================
!
subroutine fft_3d(c,n,fft_sign)
 !
 use pars,           ONLY:DP
 use com,            ONLY:error
 use fft_interfaces, ONLY:fwfft,invfft
 use fft_scalar,     ONLY:cfft3d
 use fft_base,       ONLY:dffts
 implicit none
 integer     :: fft_sign,n(3)
 complex(DP) :: c(n(1)*n(2)*n(3))
 
 !
 ! init and cheks
 !
 if ( .not.associated(dffts%nsp) ) then
   !call fft_desc_init(n,dffts)
   call error(' [FFT] dffts not initialized')
 else
   if (n(1)/=dffts%nr1.or.n(2)/=dffts%nr2.or.n(3)/=dffts%nr3) &
     call error(' [FFT] Invalid fft dimensions') 
 endif
 if ( fft_sign==0.or.fft_sign<-2.or.fft_sign>2 ) &
     call error(' [FFT] Invalid fft sign')
 !
 ! actual calls
 !
#if !defined _USE_3D_FFT
 !
 if (fft_sign==-1) then
   ! "R->G,  sign=-1, FW,  smooth"
   call fwfft( "Smooth", c, dffts )
 endif
 if (fft_sign==-2) then
   ! "R->G, sign=-2, FW,  wave"
   call fwfft( "Wave", c, dffts )
 endif
 if (fft_sign==1) then
   ! "G->R, sign=1, INV,  smooth"
   call invfft( "Smooth", c, dffts )
 endif
 if (fft_sign==2) then
   ! "G->R, sign=2, INV,  wave"
   call invfft( "Wave", c, dffts )
 endif
 !
#else
 !
 if (fft_sign < 0 ) then
   call cfft3d( c, dffts%nr1, dffts%nr2, dffts%nr3, dffts%nr1x, dffts%nr2x, dffts%nr3x, -1 )
 endif
 if (fft_sign > 0 ) then
   call cfft3d( c, dffts%nr1, dffts%nr2, dffts%nr3, dffts%nr1x, dffts%nr2x, dffts%nr3x, +1 )
 endif
 !
#endif
 !
end subroutine


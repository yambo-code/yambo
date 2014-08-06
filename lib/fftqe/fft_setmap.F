!
!        Copyright (C) 2000-2014 the YAMBO team 
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
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
! This routine has been adapted by A. Ferretti from 
! Modules/recvec_subs.f90 (Quantum-ESPRESSO distribution)
!
integer function fft_setmap(iv,fft_dim)
 !
 use fft_base,       ONLY:dffts
 implicit none
 !
 integer :: iv(3)
 integer :: fft_dim(3)
 !
 integer :: n1,n2,n3,nl
 character(10) :: subname="fft_setmap"
 !
 ! set nl and nls with the correct fft correspondence
 !

 if (fft_dim(1)/=dffts%nr1.or.fft_dim(2)/=dffts%nr2.or.fft_dim(3)/=dffts%nr3) &
&     call errore(subname,' [FFT] Invalid fft dimensions',10)

 n1 = iv(1) +1
 IF (n1<1) n1 = n1 + dffts%nr1

 n2 = iv(2) +1
 IF (n2<1) n2 = n2 + dffts%nr2

 n3 = iv(3)+1
 IF (n3<1) n3 = n3 + dffts%nr3

 if (n1<1 .or. n2<1 .or. n3<1) &
     call errore(subname,' [FFT] invalid negative n1, n2, n3 ',10)
 if (n1>dffts%nr1 .or. n2>dffts%nr2 .or. n3>dffts%nr3) &
     call errore(subname,' [FFT] Mesh too small?',10)

#if !defined (_USE_3D_FFT)
  nl = n3 + ( dffts%isind (n1 + (n2 -1) * dffts%nr1x) -1 ) * dffts%nr3x
#else
  nl = n1 + (n2 -1) * dffts%nr1x + (n3 -1) * dffts%nr1x * dffts%nr2x
#endif
 !
 fft_setmap = nl
 !
 if (nl < 0 ) call errore(subname,' [FFT] invalid negative fft_map',10)
 return
 !
end function

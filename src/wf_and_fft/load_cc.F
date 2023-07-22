!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine load_cc(manage_zero,x_zero_point,n_coord)
 !
 ! manage_zero:
 !  - false --> no action
 !  - true  --> if(x<10*epsilon(1._SP)) x=sing(x)*10*epsilon(1._SP)
 !              useful in case one needs to compute 1/x^apha
 !
 use pars,        ONLY:SP
 use FFT_m,       ONLY:fft_dim
 use D_lattice,   ONLY:x_cc,a
 !
 implicit none
 !
 ! input variable
 !
 logical, intent(in)   :: manage_zero
 real(SP), intent(in)  :: x_zero_point(3)
 integer, intent(in)   :: n_coord
 !
 real(SP), external :: saw
 !
 ! work space
 !
 real(SP)   :: rir(3),rir_saw(3),x_jump(3),width
 integer    :: ir,ir1,ir2,ir3,i_dir   
 !
 x_jump=x_zero_point-floor(x_zero_point)+0.5_SP
 x_jump=x_jump-floor(x_jump)
 !
 width=1.E-3_SP
 !
 do ir3 = 0, fft_dim(3)-1     ! z
   rir(3)=real(ir3,SP)/real(fft_dim(3),SP)
   do ir2 = 0, fft_dim(2)-1       ! y
     rir(2)=real(ir2,SP)/real(fft_dim(2),SP)
     do ir1 = 0, fft_dim(1)-1         ! x 
       rir(1)=real(ir1,SP)/real(fft_dim(1),SP)
       !
       ir = 1 + ir1 + ir2*fft_dim(1) + ir3*fft_dim(1)*fft_dim(2)
       !
       do i_dir=1,3
         rir_saw(i_dir)=saw(x_jump(i_dir),width,rir(i_dir))
       enddo
       !
       x_cc(ir,:,n_coord) = a(1,:)*rir_saw(1) + a(2,:)*rir_saw(2) + a(3,:)*rir_saw(3)
       !
       if(.not.manage_zero) cycle
       !
       do i_dir=1,3
         if(abs(x_cc(ir,i_dir,n_coord))>=10*epsilon(1._SP)) cycle
         if(x_cc(ir,i_dir,n_coord)>=0._SP) x_cc(ir,i_dir,n_coord)= 10*epsilon(1._SP)
         if(x_cc(ir,i_dir,n_coord)< 0._SP) x_cc(ir,i_dir,n_coord)=-10*epsilon(1._SP)
       enddo
       !
     enddo
   enddo
 enddo
 !
end subroutine
!
!
pure function saw( x_jump, width, x ) result ( sawout )
  !
  ! Adapted from QE, extfield.f90
  !
  use pars,     ONLY:SP
  !
  implicit none
  !
  real(SP),intent(in) :: x_jump, width, x
  !
  real(SP) :: sawout, x_shifted,x_centered
  !
  x_shifted  = x - x_jump
  x_centered = x_shifted - floor(x_shifted)
  !
  if (x_centered <= width) THEN
      sawout = (0.5_SP - x_centered/width) * (1._SP-width)
  else
      ! I would use:   sawout = x_centered - 0.5_SP * ( 1.0_SP + width )
      sawout = (-0.5_SP + (x_centered-width)/(1._SP-width)) * (1._SP-width)
  endif
  !
end function saw

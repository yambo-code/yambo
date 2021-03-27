!
!        Copyright (C) 2000-2020 the YAMBO team
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
 logical    :: manage_zero
 real(SP)   :: x_zero_point(3)
 integer    :: n_coord
 !
 ! work space
 !
 real(SP)   :: rir(3),rir_saw(3),x_jump(3)
 integer    :: ir,ir1,ir2,ir3,i_dir   
 !
 x_jump(1)=0.5_SP+x_zero_point(1)
 x_jump(2)=0.5_SP+x_zero_point(2)
 x_jump(3)=0.5_SP+x_zero_point(3)
 !
 do ir1 = 0, fft_dim(1)-1         ! x 
   rir(1)=real(ir1,SP)/real(fft_dim(1),SP)
   do ir2 = 0, fft_dim(2)-1       ! y
     rir(2)=real(ir2,SP)/real(fft_dim(2),SP)
     do ir3 = 0, fft_dim(3)-1     ! z
       rir(3)=real(ir3,SP)/real(fft_dim(3),SP)
       !
       ir = 1 + ir1 + ir2*fft_dim(1) + ir3*fft_dim(1)*fft_dim(2)
       !
       do i_dir=1,3
         rir(i_dir) = rir(i_dir)-x_jump(i_dir)
         rir(i_dir) = rir(i_dir)-floor(rir(i_dir))-0.5_SP
       enddo
       !
       !write(*,*) ir,rir,rir_saw
       !
       x_cc(ir,1,n_coord) = a(1,1)*rir(1) + a(2,1)*rir(2) + a(3,1)*rir(3)
       x_cc(ir,2,n_coord) = a(1,2)*rir(1) + a(2,2)*rir(2) + a(3,2)*rir(3)
       x_cc(ir,3,n_coord) = a(1,3)*rir(1) + a(2,3)*rir(2) + a(3,3)*rir(3)
       !
       if(.not.manage_zero) cycle
       !
       do i_dir=1,3
         if(abs(x_cc(ir,i_dir,n_coord))<10*epsilon(1._SP)) then
           if(x_cc(ir,i_dir,n_coord)>=0._SP) x_cc(ir,i_dir,n_coord)= 10*epsilon(1._SP)
           if(x_cc(ir,i_dir,n_coord)< 0._SP) x_cc(ir,i_dir,n_coord)=-10*epsilon(1._SP)
         endif
       enddo
       !
     enddo
   enddo
 enddo
 !
end subroutine

!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine load_cc(manage_zero,center_cc)
 !
 ! manage_zero:
 !  - false --> no action
 !  - true  --> if(x<10*epsilon(1._SP)) x=sing(x)*10*epsilon(1._SP)
 !              useful in case one needs to compute 1/x^apha
 !
 ! center_cc
 !  - false --> the saw-like jump is at the border of the supercell
 !  - true  --> the saw-like jump is at the center of the supercell
 !
 use pars,        ONLY:SP
 use FFT_m,       ONLY:fft_size,fft_dim
 use D_lattice,   ONLY:x_cc,a,x_zero_point
 !
 implicit none
 !
 ! input variable
 !
 logical    :: manage_zero,center_cc
 !
 ! work space
 !
 real(SP)   :: tmp_border(3),tmp_x(3),rir1,rir2,rir3
 integer    :: ir,ir1,ir2,ir3,i_dir   
 !
 call fft_setup(0,1,.true.)      ! FFT size
 !
 allocate(x_cc(fft_size,3))
 !
 ! Not implemented. Option to set the jump elsewhere
 x_zero_point=0._SP
 !
 do ir1 = 0, fft_dim(1)-1         ! x 
   rir1=real(ir1,SP)/real(fft_dim(1),SP)
   do ir2 = 0, fft_dim(2)-1       ! y
     rir2=real(ir2,SP)/real(fft_dim(2),SP)
     do ir3 = 0, fft_dim(3)-1     ! z
       rir3=real(ir3,SP)/real(fft_dim(3),SP)
       !
       ir = 1 + ir1 + ir2*fft_dim(1) + ir3*fft_dim(1)*fft_dim(2)
       !
       ! Definition: x_i=a1_i+a_2i+a3_i     i=x,y,z       in cartesian coordinates
       tmp_x(:) = a(1,:)*rir1 + a(2,:)*rir2 + a(3,:)*rir3
       !
       tmp_border(1)=a(1,1)/2._SP+a(2,1)*rir2 +a(3,1)*rir3
       tmp_border(2)=a(1,2)*rir1 +a(2,2)/2._SP+a(3,2)*rir3
       tmp_border(3)=a(1,3)*rir1 +a(2,3)*rir2 +a(3,3)/2._SP
       !
       ! Correclty compute x1, x2 and x3 with respect to cartesian axis
       ! They are saw-like and the jumps are always at the axis parallel to the vectors
       ! which defines the supercell and which cross at the center of the supercell
       ! If center_cc is specified the saw-like jump is in the middle of the super-cell 
       !
       x_cc(ir,:)=tmp_x(:)
       !
       if(.not.center_cc) cycle
       !
       tmp_border(1)=a(1,1)/2._SP+a(2,1)*rir2 +a(3,1)*rir3
       tmp_border(2)=a(1,2)*rir1 +a(2,2)/2._SP+a(3,2)*rir3
       tmp_border(3)=a(1,3)*rir1 +a(2,3)*rir2 +a(3,3)/2._SP
       !
       do i_dir=1,3
         if (tmp_x(i_dir)>tmp_border(i_dir)) x_cc(ir,:)=x_cc(ir,:)-a(i_dir,:)
       enddo
       !
       do i_dir=1,3
         if(manage_zero .and. abs(x_cc(ir,i_dir))<10*epsilon(1._SP)) then
           if(x_cc(ir,i_dir)>=0._SP) x_cc(ir,i_dir)= 10*epsilon(1._SP)
           if(x_cc(ir,i_dir)< 0._SP) x_cc(ir,i_dir)=-10*epsilon(1._SP)
         endif
       enddo
       !
     enddo
   enddo
 enddo
 !
end subroutine


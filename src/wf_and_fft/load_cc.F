!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine load_cc(manage_zero,x_zero_point,n_coord)
 !
 ! manage_zero:
 !  - false --> no action
 !  - true  --> if(x<10*epsilon(1._SP)) x=sing(x)*10*epsilon(1._SP)
 !              useful in case one needs to compute 1/x^apha
 !
 ! center_cc, 
 !  - false,i.e. x_zero_point=(0.5,0.5,0.5) --> the saw-like jump is at the border of the supercell
 !  - true ,i.e. x_zero_point=( 0 , 0 , 0 ) --> the saw-like jump is at the center of the supercell 
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
 real(SP)   :: tmp_border(3),tmp_x(3),rir1,rir2,rir3,x_jump(3)
 integer    :: ir,ir1,ir2,ir3,i_dir   
 !
 x_jump=(/a(1,1)/2._SP,a(2,2)/2._SP,a(3,3)/2._SP/)+x_zero_point
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
       ! Correctly compute x1, x2 and x3 with respect to cartesian axis
       ! They are saw-like and the jumps are always at the axis parallel to the vectors
       ! which defines the supercell and which cross at the center of the supercell
       ! If center_cc is specified the saw-like jump is in the middle of the super-cell 
       !
       tmp_border(1)= x_jump(1)  +a(2,1)*rir2 +a(3,1)*rir3
       tmp_border(2)=a(1,2)*rir1 + x_jump(2)  +a(3,2)*rir3
       tmp_border(3)=a(1,3)*rir1 +a(2,3)*rir2 + x_jump(3)
       !
       x_cc(ir,:,n_coord)=tmp_x(:)
       !
       ! 2018/03/01
       !============
       ! DS warning: this implment must be changed for the cases a(i,j)<0 
       !
       do i_dir=1,3
         if (tmp_x(i_dir)>  tmp_border(i_dir)                ) x_cc(ir,:,n_coord)=x_cc(ir,:,n_coord)-a(i_dir,:)
         !if (tmp_x(i_dir)<=(tmp_border(i_dir)-a(i_dir,i_dir))) x_cc(ir,:,n_coord)=x_cc(ir,:,n_coord)+a(i_dir,:)
       enddo
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

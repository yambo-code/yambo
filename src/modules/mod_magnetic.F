!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
module magnetic
 !
 use pars, ONLY:SP,schlen
 implicit none
 !
 character(schlen) :: MAG_hamiltonian_type
 character(schlen) :: MAG_gauge               ! 'SYMM' 'X_ASYMM' 'Y_ASYMM' 'Z_ASYMM'
 logical           :: MAG_pauli
 logical           :: MAG_landau
 !
 ! Magnetic field in polar coordinates. 
 ! Theta angle with z axis; psi angle with x axis
 !
 real(SP)          :: MAG_B
 real(SP)          :: MAG_theta
 real(SP)          :: MAG_psi
 !
 ! Magnetic field in cartesian coordinates & gauge factors
 !
 real(SP)          :: Bx
 real(SP)          :: By
 real(SP)          :: Bz
 !
 ! Aharonov Bohm variables
 !
 real(SP)                 :: MAG_radius              
 logical,     allocatable :: ir_table(:)
 !
 ! Vector potential & gauge factors
 !
 real(SP),    allocatable :: A_magn_x(:)
 real(SP),    allocatable :: A_magn_y(:)
 real(SP),    allocatable :: A_magn_z(:)
 integer                  :: gauge_factors(2,3)
 !
 ! Phase trick phasctors
 !
 logical                  :: phase_trick
 real(SP), parameter      :: phase_radius=0.01
 integer                  :: n_loops
 !                                                      ! 'Bohr_magneton/Bohr^2' 'Bohr_magneton/Bohr^3'
 ! Centered cartesian coordinates and
 !          cylindrical coordinates
 !
 real(SP),    allocatable :: x(:,:)
 real(SP)                 :: cell_center(3)
 real(SP),    allocatable :: cos_phi(:)
 real(SP),    allocatable :: sin_phi(:)
 real(SP),    allocatable :: r(:)
 !
 complex(SP), allocatable :: new_pp(:,:),old_pp(:,:)
 !
 contains
   !
   subroutine load_cc(manage_zero,center_cc)
     use FFT_m,       ONLY:fft_size,fft_dim
     use D_lattice,   ONLY:a
     ! input variable
     logical    :: manage_zero,center_cc
     real(SP)   :: tmp_center(3),tmp_x(3)
     integer    :: ir,ir1,ir2,ir3,i_dir   
     !
     call fft_setup(0,1,.true.)      ! FFT size
     allocate(x(fft_size,3))
     cell_center(:)=0.
     !
     do ir1 = 0, fft_dim(1)-1         ! x 
       do ir2 = 0, fft_dim(2)-1       ! y
         do ir3 = 0, fft_dim(3)-1     ! z
           ir = 1 + ir1 + ir2*fft_dim(1) + ir3*fft_dim(1)*fft_dim(2)
           ! Definition: x_i=a1_i+a_2i+a3_i     i=x,y,z
           !             in cartesian coordinates
           tmp_x(:) = ir1*a(1,:)/fft_dim(1) +&
&                     ir2*a(2,:)/fft_dim(2) +&
&                     ir3*a(3,:)/fft_dim(3)
           tmp_center(1)=cell_center(1)+a(1,1)/2+a(2,1)*ir2/fft_dim(2)+a(3,1)*ir3/fft_dim(3)
           tmp_center(2)=cell_center(2)+a(2,2)/2+a(1,2)*ir1/fft_dim(1)+a(3,2)*ir3/fft_dim(3)
           tmp_center(3)=cell_center(3)+a(3,3)/2+a(1,3)*ir1/fft_dim(1)+a(2,3)*ir2/fft_dim(2)
           ! Correclty compute x1, x2 and x3 with respect to cartesian axis
           ! They are saw-like and the jumps are always at the axis parallel to the vectors
           ! which defines the supercell and which cross at the center of the supercell
           ! The differents "if" take into account how the supercell is "compound" around the
           ! supercell corner
           x(ir,:)=tmp_x(:)
           if(.not.center_cc) cycle
           do i_dir=1,3
             if (tmp_x(i_dir)>tmp_center(i_dir)) x(ir,:)=x(ir,:)-a(i_dir,:)
           enddo
           if(manage_zero .and. sqrt(x(ir,1)**2+x(ir,2)**2)<epsilon(1.) ) then
             do i_dir=1,2 
               if(x(ir,i_dir)>=0.) x(ir,i_dir)= 1. !  epsilon(1.)
               if(x(ir,i_dir)< 0.) x(ir,i_dir)=-1. ! -epsilon(1.)
             enddo
           endif
           if(manage_zero.and.abs(x(ir,3))<epsilon(1.) ) then
             if(x(ir,3)>=0.) x(ir,3)= 1.   ! epsilon(1.)
             if(x(ir,3)< 0.) x(ir,3)=-1.   !-epsilon(1.)
           endif
         enddo
       enddo
     enddo
     !
   end subroutine
   !
   subroutine free_cc()
     deallocate(x)
   end subroutine
   !
   subroutine load_cyl()
     use FFT_m,       ONLY:fft_size,fft_dim
     implicit none
     logical          :: not_already_alloc
     !
     not_already_alloc=.true.
     if(allocated(x)) not_already_alloc=.false.
     if(not_already_alloc) call load_cc(.true.,.true.)
     allocate(cos_phi(fft_size),sin_phi(fft_size))
     allocate(r(fft_size))
     !
     r(:)=sqrt(x(:,1)**2+x(:,2)**2)
     cos_phi(:)=x(:,1)/r(:)
     sin_phi(:)=x(:,2)/r(:)
     !
!     cos_phi(:)=1.
!     sin_phi(:)=1.
     !
     if(not_already_alloc)  call free_cc()
     !
   end subroutine
   !
   subroutine free_cyl()
     deallocate(cos_phi,sin_phi,r)
   end subroutine
   !
end module

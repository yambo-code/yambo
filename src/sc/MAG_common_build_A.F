!
! Copyright (C) 2000-2008 A. Marini, D. Sangalli and the YAMBO team 
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
subroutine MAG_common_build_A(build)
 !
 !
 ! WARNING: you need to put you system at the corner of the supercell <---
 !
 ! This subroutine construct the magnetic vector potential.
 ! The field in a single supercell is supposed to be
 ! *******************************************************************************************
 !                               [  MAG_B    r<=MAG_radius
 !    Bx=0   ,   By=0    ,  Bz = {                                ====> A; B=rot(A)
 !                               [   0       r> MAG_radius  
 !
 ! With three possible gauges for the vector potential
 ! *****************************************************
 ! Gauge 0 --> Symmetric gauge
 !      [ -0.5*MAG_B y                                     [ 0.5*MAG_B x
 ! Ax = {                                             Ay = {                                          A_z=0
 !      [ -0.5*MAG_B (MAG_radius**2)*y/(x**2+y**2)         [ 0.5*MAG_B (MAG_radius**2)*x/(x**2+y**2)
 !
 ! Gauge 1 --> Asymmetric gauge (active only if MAG_radiu==0)
 ! Ax = -MAG_B y        Ay=  0              Az=0    
 ! Gauge 1 --> Asymmetric gauge (active only if MAG_radiu==0)
 ! Ax =  0              Ay=  MAG_B x        Az=0    
 ! *******************************************************************************************
 !
 !
 use pars,           ONLY:SP
 use memory_m,       ONLY:mem_est
 use FFT_m,          ONLY:fft_size,fft_dim
 use D_lattice,      ONLY:a
 use R_lattice,      ONLY:nkibz
 use magnetic,       ONLY:MAG_radius,MAG_B_z,MAG_landau,&
&                         MAG_gauge,wf_x,wf_y,A_magn_x,A_magn_y,x,y,gauge_factors
 use vec_operate,    ONLY:v_norm
 implicit none
 !
 ! Input variables
 logical   :: build
 !
 ! Dummies
 integer   :: ir,ir1,ir2,ir3   
 real(SP)  :: radius, r_mod,cell_center(3)
 !
 ! Allocs
 real(SP),    allocatable :: r_cell_cc(:,:)
 !
 ! Allocation 
 !============
 if(.not.build) then
   call module_free()
   return
 endif
 call fft_setup(0,1,.false.) ! FFT size
 call module_alloc()
 call local_alloc()
 !
 ! Set the gauge factors
 !=======================
 if(MAG_gauge=='X_ASYMM') then
   gauge_factors(1)=2
   gauge_factors(2)=0
 else if(MAG_gauge=='Y_ASYMM') then
   gauge_factors(1)=0
   gauge_factors(2)=2
 else
   MAG_gauge='SYMM' 
   gauge_factors(:)=1
 endif
 !
 ! A(r) Build-Up
 ! Ax , Ay  (Az=0)   
 !=================
 !
 cell_center(:)=0.
 !
 do ir1 = 0, fft_dim(1)-1         ! x 
   do ir2 = 0, fft_dim(2)-1       ! y
     do ir3 = 0, fft_dim(3)-1     ! z
       ir = 1 + ir1 + ir2*fft_dim(1) + ir3*fft_dim(1)*fft_dim(2)
       ! Definition: rcell_i=a1_i+a_2i+a3_i     i=x,y,z
       !             in cartesian coordinates
       r_cell_cc(ir,:) =  ir1*a(1,:)/fft_dim(1) +&
&                         ir2*a(2,:)/fft_dim(2) +&
&                         ir3*a(3,:)/fft_dim(3)
       !
       cell_center(1)=a(1,1)/2+a(2,1)*ir2/fft_dim(2)+a(3,1)*ir3/fft_dim(3)
       cell_center(2)=a(2,2)/2+a(1,2)*ir1/fft_dim(1)+a(3,2)*ir3/fft_dim(3)
       !
       ! Radius in the direction perpendicular to the magnetic field
       r_mod=v_norm((/r_cell_cc(ir,1),r_cell_cc(ir,2),0./))
       ! correclty compute x and y with respect to cartesian axis
       ! They are saw-like and the jump is always at the axis parallel to the vectors
       ! which defines the supercell and which cross at the center of the supercell
       ! The differents "if" take into account how the supercell is "compound" around the
       ! supercell corner
       if (r_cell_cc(ir,2)<cell_center(2) .and. r_cell_cc(ir,1)<cell_center(1)) then
         y(ir)=r_cell_cc(ir,2)
         x(ir)=r_cell_cc(ir,1)
       endif
       if (r_cell_cc(ir,2)<cell_center(2) .and. r_cell_cc(ir,1)>cell_center(1)) then
         y(ir)=r_cell_cc(ir,2)-a(1,2)
         x(ir)=r_cell_cc(ir,1)-a(1,1)
       endif
       if (r_cell_cc(ir,2)>cell_center(2) .and. r_cell_cc(ir,1)<cell_center(1)) then
         y(ir)=r_cell_cc(ir,2)-a(2,2)
         x(ir)=r_cell_cc(ir,1)-a(2,1)
       endif
       if (r_cell_cc(ir,2)>cell_center(2) .and. r_cell_cc(ir,1)>cell_center(1)) then
         y(ir)=r_cell_cc(ir,2)-a(1,2)-a(2,2)
         x(ir)=r_cell_cc(ir,1)-a(2,1)-a(1,1)
       endif
       ! Now build up the vector potential         
       if(MAG_landau) then
         if(r_mod<=MAG_radius .or. MAG_radius==0) then
           A_magn_x(ir)=-0.5*MAG_B_z*y(ir)
           A_magn_y(ir)= 0.5*MAG_B_z*x(ir)
         else
           A_magn_x(ir)=-0.5*MAG_B_z*MAG_radius**2*y(ir)/r_mod**2
           A_magn_y(ir)= 0.5*MAG_B_z*MAG_radius**2*x(ir)/r_mod**2
         endif
       endif
     enddo
   enddo
 enddo
 !
 call local_free()
 !
 contains
   !
   subroutine module_alloc()
     allocate(x(fft_size),y(fft_size))
     call mem_est("x y",(/fft_size,fft_size/))
     if(MAG_landau) then
       allocate(A_magn_x(fft_size),A_magn_y(fft_size))
       call mem_est("A_magn_x A_magn_y",(/fft_size,fft_size/))
       A_magn_x=0.
       A_magn_y=0.
     endif
     x=0.
     y=0.
   end subroutine
   !
   subroutine local_alloc()
     allocate(r_cell_cc(fft_size,3))
     call mem_est("r_cell_cc",(/fft_size*3/))
     r_cell_cc(:,:)=0.
   end subroutine
   !
   subroutine module_free()
     if(MAG_landau) then
       deallocate(A_magn_x,A_magn_y)
       call mem_est("A_magn_x A_magn_y")
     endif
     deallocate(x,y)
     call mem_est("x y")
   end subroutine
   !
   subroutine local_free()
     deallocate(r_cell_cc)
     call mem_est("r_cell_cc")
   end subroutine
   !
end subroutine

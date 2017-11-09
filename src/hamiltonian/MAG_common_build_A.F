!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
 ! The field in a single supercell is supposed to be:
 ! *******************************************************************************************
 !                               [  Bz       r<=MAG_radius
 !    Bx=0   ,   By=0    ,  Bz = {                                ====> A; B=rot(A)
 !                               [   0       r> MAG_radius  
 !
 ! With three possible gauges for the vector potential
 ! *****************************************************
 ! Gauge 0 --> Symmetric gauge
 !      [ -0.5*Bz y                                        [ 0.5*Bz x
 ! Ax = {                                             Ay = {                                          A_z=0
 !      [ -0.5*Bz (MAG_radius**2)*y/(x**2+y**2)            [ 0.5*Bz (MAG_radius**2)*x/(x**2+y**2)
 !
 ! Gauge 1 --> X_ASYMM (active only if MAG_radiu==0)
 ! Ax = -MAG_B y        Ay=  0              Az=0    
 ! Gauge 1 --> Y_ASYMM (active only if MAG_radiu==0)
 ! Ax =  0              Ay=  MAG_B x        Az=0    
 ! *******************************************************************************************
 ! The previous was the construction of the vector potential for the Bz component
 ! If we have x or y component the procedure is the same
 !
 use pars,           ONLY:SP
 use units,          ONLY:SPEED_OF_LIGHT
 use FFT_m,          ONLY:fft_size,fft_dim
 use D_lattice,      ONLY:x_cc
 use magnetic,       ONLY:MAG_radius,MAG_B,MAG_landau,MAG_psi,MAG_theta,  &
&                         MAG_gauge,Bx,By,Bz,ir_table,                    &
&                         A_magn_x,A_magn_y,A_magn_z,gauge_factors,       &
&                         phase_trick,phase_radius,n_loops
 use vec_operate,    ONLY:v_norm
#include<memory.h>
 !
 ! Input variables
 logical   :: build
 !
 ! Work space
 integer   :: ir,ir1,ir2,ir3
 real(SP)  :: r_mod
 !
 !
 ! Allocation 
 !============
 if(.not.build) then
   call module_free()
   return
 endif
 !
 ! MAG field components
 !=====================
 Bx=MAG_B*sin(MAG_theta)*cos(MAG_psi)
 By=MAG_B*sin(MAG_theta)*sin(MAG_psi)
 Bz=MAG_B*cos(MAG_theta)
 !
 call fft_setup(0,1,.true.)
 call module_alloc()
 !
 if(MAG_landau .and. phase_trick) call MAG_A_phase(n_loops)
 !
 if(.not.MAG_landau) return
 !
 ! Set the gauge factors:
 !=======================
 ! These enable to change gauge in order to "kill" the vector potential which
 ! grows linearly along one direction, useful and possible only when the field 
 ! is everywhere in the supercell
 gauge_factors(:,:)=1
 if(MAG_gauge=='X_ASYMM') then
   gauge_factors(1,2)=0         ! Az for By ~ -x <--
   gauge_factors(2,2)=2         ! Ax for By ~  z
   gauge_factors(1,3)=2         ! Ax for Bz ~ -y
   gauge_factors(2,3)=0         ! Ay for Bz ~  x <--
 else if(MAG_gauge=='Y_ASYMM') then
   gauge_factors(1,1)=0         ! Az for Bx ~  y <--
   gauge_factors(2,1)=2         ! Ay for Bx ~ -z
   gauge_factors(1,3)=0         ! Ax for Bz ~ -y <--
   gauge_factors(2,3)=2         ! Ay for Bz ~  x
 else if(MAG_gauge=='Z_ASYMM') then
   gauge_factors(1,1)=2         ! Az for Bx ~  y
   gauge_factors(2,1)=0         ! Ay for Bx ~ -z <--
   gauge_factors(1,2)=2         ! Az for By ~ -x
   gauge_factors(2,2)=0         ! Ax for By ~  z <--
 endif
 !
 YAMBO_FREE(x_cc)
 call load_cc(.false.,(/0._SP,0._SP,0._SP/))
 !
 ! A(r) Build-Up
 ! Ax , Ay , Az   
 !=================
 do ir1 = 0, fft_dim(1)-1         ! x 
   do ir2 = 0, fft_dim(2)-1       ! y
     do ir3 = 0, fft_dim(3)-1     ! z
       ir = 1 + ir1 + ir2*fft_dim(1) + ir3*fft_dim(1)*fft_dim(2)
       ! Radius in the direction perpendicular to the magnetic field
       ! The AB effect is implemented only with a flux in the a(3,:) direction
       ! Anyway I'm not sure that this is right if a(3,:) is not perpendicular
       ! to the plane defined by a(1,:) and a(2,:)
       r_mod=v_norm((/x_cc(ir,1),x_cc(ir,2),0._SP/))
       ! Now build up the vector potential         
       if(r_mod<=MAG_radius .or. MAG_radius==0) then
         if(Bx/=0.) then
           A_magn_z(ir)= 0.5*Bx*x_cc(ir,2)*gauge_factors(1,1)
           A_magn_y(ir)=-0.5*Bx*x_cc(ir,3)*gauge_factors(2,1)
         endif
         if(By/=0.) then
           A_magn_z(ir)=A_magn_z(ir)-0.5*By*x_cc(ir,1)*gauge_factors(1,2)
           A_magn_x(ir)= 0.5*By*x_cc(ir,3)*gauge_factors(2,2)
         endif
         if(Bz/=0.) then
           A_magn_x(ir)=A_magn_x(ir)-0.5*Bz*x_cc(ir,2)*gauge_factors(1,3)
           A_magn_y(ir)=A_magn_y(ir)+0.5*Bz*x_cc(ir,1)*gauge_factors(2,3)
         endif
       else
         ir_table(ir)=.false.
         if(Bx/=0.) then
           A_magn_z(ir)= 0.5*Bx*MAG_radius**2*x_cc(ir,2)/r_mod**2
           A_magn_y(ir)=-0.5*Bx*MAG_radius**2*x_cc(ir,3)/r_mod**2
         endif
         if(By/=0.) then
           A_magn_z(ir)=A_magn_z(ir)-0.5*By*MAG_radius**2*x_cc(ir,1)/r_mod**2
           A_magn_x(ir)= 0.5*By*MAG_radius**2*x_cc(ir,3)/r_mod**2
         endif
         if(Bz/=0.) then
           A_magn_x(ir)=A_magn_x(ir)-0.5*Bz*MAG_radius**2*x_cc(ir,2)/r_mod**2
           A_magn_y(ir)=A_magn_y(ir)+0.5*Bz*MAG_radius**2*x_cc(ir,1)/r_mod**2
         endif
       endif
       if(phase_trick .and. r_mod>phase_radius.and.Bz/=0.) then
         A_magn_x(ir)=A_magn_x(ir)+SPEED_OF_LIGHT*n_loops*x_cc(ir,2)/r_mod**2
         A_magn_y(ir)=A_magn_y(ir)-SPEED_OF_LIGHT*n_loops*x_cc(ir,1)/r_mod**2
       endif
     enddo
   enddo
 enddo
 !
 YAMBO_FREE(x_cc)
 !
 contains
   !
   ! Allocates objects of mod magnetic
   subroutine module_alloc()
     if(MAG_landau) then
       if(Bx/=0. .or. By/=0.) then
         YAMBO_ALLOC(A_magn_z,(fft_size))
         A_magn_z=0.
       endif
       if(Bx/=0. .or. Bz/=0.) then
         YAMBO_ALLOC(A_magn_y,(fft_size))
         A_magn_y=0.
       endif
       if(By/=0. .or. Bz/=0.) then
         YAMBO_ALLOC(A_magn_x,(fft_size))
         A_magn_x=0.
       endif
     endif
     YAMBO_ALLOC(ir_table,(fft_size))
     ir_table(:)=.true.
   end subroutine
   !
   subroutine module_free()
     if(MAG_landau) then
       YAMBO_FREE(A_magn_x)
       YAMBO_FREE(A_magn_y)
       YAMBO_FREE(A_magn_z)
     endif
     YAMBO_FREE(ir_table)
   end subroutine
   !
end subroutine

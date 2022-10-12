!
! Copyright (C) 2000-2016 A. Marini and the YAMBO team
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
 subroutine ras_presets()
   use pars,                  only : SP, schlen
   use units,                 only : HA2EV
   use surface_geometry,      only : q0x, q0y, nlayers
   use eels_kinematics,       only : E0, theta0_in, thetap_in, phi_in, bimp
   use bulkeps,               only : bshift, bbroad, bfile, btype
   use convolute,             only : gbroad
   use eels_detector,         only : thetadet, det_int_type, n_det_pt
   use model_loss_function,   only : model
   use optcut,                only : z0f, dcf, loptcut
   use ras_module,            only : locfac, loctype
   use parallel_m,            only : n_CPU_str_max,CPU_structure,ncpu

   implicit none
  
   character(schlen) :: string,name
   integer           :: max_val,min_val
  
   if (mod(ncpu,2)==0) then
      if (mod(ncpu,4)==0) then
         max_val=max(4,ncpu/4)
         min_val=min(4,ncpu/4)
      else 
         max_val=max(2,ncpu/2)
         min_val=min(2,ncpu/2)
      endif
   else 
      if (mod(ncpu,3)==0) then
         max_val=max(3,ncpu/3)
         min_val=min(3,ncpu/3)
      else 
         max_val=ncpu
         min_val=1
      endif  
   endif 
   write(string,'(I0,1X,I0)') max_val,min_val
   name=trim(string)
   !name='"'//trim(string)//'"'
   CPU_structure(3)%CPU_string=name
   CPU_structure(3)%ROLE_string="c.v"
   !write(*,*) CPU_structure(3)%CPU_string
   !
   ! mod_surface_geometry
   !
   nlayers = (/ 20.0,10.0,0.0 /) ! Layers cell/slab/bulk
   q0x = (/ 1,0,0 /)    ! q0x
   q0y = (/ 0,1,0 /)    ! q0y
   !
   ! mod_bulkeps
   !
   bshift  = 0.0_SP                 ! shift
   bbroad  = 0.0_SP                 ! bulk broadening
   bfile = "bulk.dat"   ! file of bulk
   btype = "3ri"
   !
   ! convolute
   !
   gbroad  = 0.0   ! gaussian convolution, FWHM
   !
   ! mod_eels_detector
   !
   thetadet  =  1.0      ! detector angle (angular aperture)
   det_int_type = "av"         ! Det. int. method no(ne),av(erage),fu(ll)
   n_det_pt  = 1000          ! Number of points for detector integration
   !
   ! mod_eels_kinematics
   !
   E0  = 15.0/HA2EV
   theta0_in  = 45.0
   thetap_in  =  0.0
   phi_in  =  0.0
   bimp  = 0.0
   !
   ! mod_model_loss_function
   !
   model = "ar"       ! form of loss function
   !
   ! mod_cutoff
   !
   z0f  = 0.0         ! zero of cut
   dcf  = 1.0         ! width of cut
   loptcut = .false.    ! cutoff on/off
   !
   ! mod_ras
   !
   loctype = "no"
   locfac = 0.0
   !
   !
   !

   !
   !
   !

   return
 end subroutine

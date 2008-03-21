!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team
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
   use pars,                  only : HARTREE, SP, schlen
   use surface_geometry,      only : q0x, q0y, nlayers
   use analyse_peaks,         only : lAnalysePeaks
   use eels_kinematics,       only : E0, theta0_in, thetap_in, phi_in, bimp
   use bulkeps,               only : bshift, bbroad, bfile, btype
   use convolute,             only : gbroad
   use eels_detector,         only : thetadet, det_int_type, n_det_pt
   use model_loss_function,   only : model
   use optcut,                only : z0f, dcf, loptcut
   use ras_module,            only : locfac, loctype
   implicit none
   !
   ! mod_surface_geometry
   !
   nlayers = (/ 20.0,10.0,0.0 /) ! Layers cell/slab/bulk
   q0x = (/ 1,0,0 /)    ! q0x
   q0y = (/ 0,1,0 /)    ! q0y
   !
   ! mod_analyse_peaks
   !
   lAnalysePeaks = .false.
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
   E0  = 15.0/HARTREE
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

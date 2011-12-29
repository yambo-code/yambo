!
! Copyright (C) 2000-2012 C. Hogan and the YAMBO team 
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
subroutine reelsdriver(Xen,Xk,q,wv,X)
  use pars,                    only : SP, lchlen
  use units,                   only : HA2EV
  use memory_m,                only : mem_est
  use com,                     only : msg
  use X_m,                     only : X_t
  use electrons,               only : levels
  use R_lattice,               only : bz_samp, bare_qpg, g_vec
  use frequency,               only : w_samp, W_duplicate
  use par_proc_m,              only : pp_redux_wait
  use bulkeps,                 only : scan_bulkeps, lbulkerr, clean_bulkeps, regrid
  use eels_detector,           only : setup_det
  use surface_geometry,        only : q0x, q0y, qname, setup_gvecaff
  use eels_gvec,               only : lorderG
  use eels_wgrid,              only : nw, nwp, nwtot, hwgrid, nwsec
  use reels_module,            only : npol, nfull, lepsmix
  use intchar,                 only : intc
  use loss_module,             only : loss, dealloc_loss
  implicit none 
  type(levels),       intent(in) :: Xen    ! Energies
  type(bz_samp),      intent(in) :: Xk, q  ! K/Q points
  type(X_t),       intent(inout) :: X      ! Polarization
  type(w_samp),       intent(in) :: wv     ! Energy ranges
!ws
  type(w_samp)                   :: wq, wfull     ! Energy ranges
  integer                        :: ipol, iw
  character(lchlen)              :: bulkmsg

! Full spectrum arrays
  real(SP), allocatable          :: hw(:)
  real(SP), allocatable          :: lossxy(:,:)
  logical                        :: lfail

  lorderG = .false.

  call section('*','Surface reflection electron energy loss (REELS) module')

  call section('+','Input processing.')
  !
  call setup_reels(lfail)
  !
  call print_reels ! general runtime options
  !
  call section('=','Initialiation of Q_{||}-independent quantities.')
  !
  ! Set up energy ranges/meshes and energy data from input (wv) 
  ! Note: wv = mesh across full frequency range
  !       wq = mesh redefined for each Q
  !
  call FREQUENCIES_setup(wv)     ! Define wv%n(), alloc and fill wv%p 
  call W_duplicate(wv, wq)       ! Copies n,er,dr but not p
  call FREQUENCIES_setup(wq)     ! Define wq%n(), alloc and fill wq%p 
  !
  ! The regular energy grid for final output mapping
  !
  wfull%n(1) = nfull; wfull%per_memstps = 100; wfull%er = wv%er
  call FREQUENCIES_setup(wfull) ! Define wfull%n(), alloc and fill wq%p 

  call X_pre_setup(Xen,X)       ! Some checks/reset on X%ib() and X%iq() 
  !
  ! nw    = number of points epsm1 is calculated on (i.e., wq%n(1) from input)
  ! nwsec = number of points lossq is calculated at for each section:
  !         if epsmix=true: nwsec=(nw+1)/2 ; epsmix = false nwsec=nw
  ! nwtot = total number of points in spectrum = nwp*nqtot
  ! nfull = final grid used in plotting (interpolated)
  !
  nw  = wq%n(1)
  nwp = (nw+1)/2
! nwsec = nw
! if(lepsmix) nwsec = nwp
  !
  ! G-vectors
  !
  call setup_gvecaff
! call v2vG( gspar)  ! FIX GSPAR ??
  !
  ! Read and store the bulk file data
  !
  call scan_bulkeps( bulkmsg ) 
  call msg('nrs',trim(bulkmsg))
  if(lbulkerr) call msg('nrs','Calculation will continue, but '//&
 &  'quantities requiring the bulk epsilon will not be computed.')
  !
  ! Detector integration arrays
  !
  call setup_det
  ! 
  ! Full spectrum arrays (fix)
  !
! allocate(loss(npol,5,nwtot))
  allocate(lossxy(wfull%n(1),2))
  !
  ! Main loop over polarizations for difference spectra
  !
  do ipol = 1, npol 
    call section('*','REELS polarization'//intc(ipol)//'/'//intc(npol))

    select case(ipol)
    case(1)
      X%q0 = q0x
      qname = 'x'
    case(2)
      X%q0 = q0y
      qname = 'y'
    end select

    call calc_loss_function(ipol, Xen, Xk, X, q, wq)

! Write loss to file just for checking grids
    open(unit=56,file="loss.dat")
    do iw=1,nwtot
      write(56,*) hwgrid(iw)*HA2EV,loss(1,iw)
    enddo
! End check
    !
    ! Regrid to densest mesh the total loss for final output
    !
    call regrid('imag',hwgrid,loss(1,:),real(wfull%p),lossxy(:,ipol),lfail)
! Write loss to file just for checking regrids
    open(unit=57,file="lossxy.dat")
    do iw=1,nfull
      write(57,*) real(wfull%p(iw))*HA2EV,lossxy(iw,1)
    enddo
! End check

    call dealloc_loss
    call cleanup_pol

  enddo    ! Loop on npol

  call clean_bulkeps

  if (npol.eq.1) then
    !
    ! Write regular REELS spectra
    !
  write(*,*) "write_reels_x",wfull%n(1),nfull
    call write_reels_x( lossxy(:,1), real(wfull%p), wfull%n(1) )
    !
  else if (npol.eq.2) then
    !
    ! Calculate REELS difference spectra, if requested.
    ! Format of difference should be x,y,diff,rdiff
    !
  write(*,*) "write_reelsdiff"
    call write_reelsdiff(lossxy, real(wfull%p), wfull%n(1) )
    !
  endif

! deallocate(bg_npts,cg_pt,cg_index_bg)
! call mem_est(" RGi BGn CGp CGi")
! deallocate(wq%cgx,wq%npcg,wq%pcg,wq%cgt,wq%p)
  call pp_redux_wait

  return

contains

  subroutine cleanup_pol
    use eels_wgrid,      only : hwqgrid
    !
    ! Deallocations after each polarization calculation
    !
    if(allocated(hwgrid)) deallocate(hwgrid)
    return
  end subroutine cleanup_pol

  subroutine setup_reels( lfail)
    use eels_integration,        only : print_eels_int
    use eels_detector,           only : print_eels_det, setup_eels_det
    use eels_kinematics,         only : print_eels_kin, setup_eels_kin
    use surface_geometry,        only : print_surface_geometry, setup_surface_geometry
    use reels_module,            only : lepsmix
    use parser_m,                only : parser
    implicit none
    logical,            intent(out) :: lfail
    lfail = .false.

    call parser('EpsMix',lepsmix)
    call setup_surface_geometry( lfail)
    call print_surface_geometry

    call setup_eels_kin( lfail )
    call print_eels_kin

    call setup_eels_det( lfail)
    call print_eels_det

!   call setup_eels_int( lfail)
    call print_eels_int
    return
  end subroutine setup_reels

  subroutine print_reels
    !
    ! General runtime options/flags
    !
    use reels_module,      only : npol, lepsmix
    implicit none

    if(lepsmix) call msg('r','Linear interpolation between eps(q) and eps(q+1).')
    if(.not.lepsmix) call msg('r','No interpolation between eps(q) and eps(q+1).')
    if(npol.eq.1) call msg('r','Single q-polarization calculation selected.')
    if(npol.eq.2) call msg('r',&
&           'Double q-polarization and relative difference calculation selected.')
    return
  end subroutine print_reels
  
end subroutine reelsdriver

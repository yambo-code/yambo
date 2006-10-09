!
! Copyright (C) 2000-2005 C. Hogan and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
  use pars,                   ONLY : SP, schlen, lchlen
  use com,                    ONLY : msg
  use X_m,                    ONLY : X_t, X_mat, X_epsilon
  use parser_m,               ONLY : parser
  use electrons,              ONLY : levels
  use R_lattice,               ONLY : bz_samp, bare_qpg, ng_vec, g_vec
  
  use IO_m,                    ONLY : io_control, OP_RD_CL, OP_WR_CL, VERIFY, REP, DUMP
  use wave_func,               ONLY : wf_ng
  use frequency,               ONLY : w_samp, W_duplicate
  use vec_operate,             ONLY : v_module
  use par_proc_m,              ONLY : pp_redux_wait
  use bulkeps
  use eels_kinematics
  use surface_geometry
  use eels_detector
  use eels_integration
  use eels_scattering
  use eels_qpt
  use eels_gvec
  use eels_wgrid
  use eels_inveps
  use intchar
! use allocatable_arrays
  implicit none 
  type(levels),       intent(in) :: Xen    ! Energies
  type(bz_samp),      intent(in) :: Xk, q  ! K/Q points
  type(X_t),       intent(inout) :: X      ! Polarization
  type(w_samp),       intent(in) :: wv     ! Energy ranges
!ws
  type(w_samp)                   :: wq     ! Energy ranges
  integer                        :: i1,i2,fr(2),nau,iq, ipol, rtype, npol
  integer                        :: iqi, isi, iqf, nwp, nw, ngblk, is
 real(SP)                        :: v_module, od(5), deltab
  character(10)                  :: headings(7)
  character(schlen)              :: lch, filename
  logical                        :: lnoe0, lerr
  real(SP)                       :: q0xy(2,3)
  character(lchlen)              :: bulkmsg,lch1,lch2

! Full spectrum arrays
  complex(SP), allocatable       :: eps_b(:), eps_i(:,:) 
  real(SP),  allocatable         :: hw(:)
  real(SP), allocatable          :: lossq(:,:), lossbq(:)
  real(SP), allocatable          :: loss(:,:,:)
  complex(SP), allocatable       :: epsm1(:,:,:)
  complex(SP), allocatable       :: epsm1_av(:,:,:),epsm1_avA(:,:,:),epsm1_avB(:,:,:)

  logical                        :: lfail, lbulkerr, lepsmix
  real(SP)                       :: qtest(3)

  lorderG = .false.

  call section('*','Surface reflection electron energy loss (REELS) module')

  call section('+','Input processing.')
  !
  call input_reels(lfail)
  !
  call print_reels ! general runtime options
  !
  call section('=','Initialiation of Q_{||}-independent quantities.')
  !
  ! Set up energy ranges/meshes and energy data from input (wv) 
  !
  call freqs_setup(wv) 
! call typ_cp(wv, wq) ! Copy to reels wrange, wq
  call W_duplicate(wv, wq) ! Copy to reels wrange, wq
  call freqs_setup(wq) 
  call Xchecks(q,Xen,X) ! what does this do?
  !
  ! nw    = number of points epsm1 is calculated on (i.e., wq%n(1) from input
  ! nwp   = number of points loss is calculated for at each q = (nw+1)/2
  ! nwtot = total number of points in spectrum = nwp*nqtot
  !
  nw  = wq%n(1)
  nwp = (nw+1)/2 ! or nwp - 1? Check.
  ngblk = X%ng
  !
  ! G-vectors
  !
  call setup_gvecaff
! call v2vG( gspar)  ! FIX GSPAR ??
  !
  ! Process and scan the bulk file
  !
! call input_bulkeps
  call scan_bulkeps( lbulkerr, bulkmsg ) 
  call msg('nrs',trim(bulkmsg))
  if(lbulkerr) call msg('nrs','Calculation will continue, but '//&
 &  'quantities requiring the bulk epsilon will not be computed.')
  !
  ! Detector integration arrays
  !
  call setup_det
  !
  ! Local allocations
  !
  allocate( lossq(5, nwp ) )
  allocate( lossbq(nwp ) )
  allocate( epsm1    (ngblk, ngblk, nw ) )
  allocate( epsm1_av (X%ng, X%ng, nwp) )
  allocate( epsm1_avA(X%ng, X%ng, nwp) )
  allocate( epsm1_avB(X%ng, X%ng, nwp) )
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
    call setup_output_reels(qname, X%q0, ipol)
        
    call section('+','Scattering frames set up.')

    call scattering_frame( X%q0 )
    call electron_velocity

    call section('=','Generate Q_|| vs hw table.')
    call qpar_1BZ( X%q0, q )
    call qpar_sort( X%q0, q )
    call qpar_hw( X%q0, q,  wv)   

    call section('=','Generate Gz tables.')
    call setup_iGz( ng_vec , iqmin, iqmax, wf_ng )

    call section('=','EELS integration setup.')
    if(.not.lanalytical) then
      call calc_zk
    endif
    call section('=','Energy grid setup.')
    call init_wgrid(wq, nqpar)
    call wqgrid(wq)
    call wgrid( wq%n(1) )
    call print_wgrid( wq%n(1) )

    allocate(loss(npol,5,nwtot))

    call section('=','Kinematic prefactors setup.')
    call init_prefactors
    call calc_prefactors
    call print_prefactors

    call section('=','Check in-plane symmetries.')
!   call in_plane_symmetry(lplansym)
    !
    ! Generate the bulk data for this grid
    !
    call section('=','Bulk epsilon')
    if(.not.lbulkerr) then
      ! depends on how calculate the det int over bulk term...full array or part?
      allocate( eps_b(nwtot) ) 
      call GetBulkEps( hwgrid, nwtot, eps_b, lbulkerr, bulkmsg)
      if(lbulkerr) call msg('nrs',trim(bulkmsg))
    endif
    !
    !
    !
    call section('*','Main REELS loop')
    call section('+','REELS for iq:'//intc(1)//'/'//intc(iqmax-iqmin+1))

    do iq = iqmin, iqmax     ! Loop over qpar for this polarization
!     iq runs over index of qpar, not FBZ
      if(iq.ne.iqmin) call section('=','REELS for iq:'//intc(iq-iqmin+1)//&
&     '/'//intc(iqmax-iqmin+1))
      iqf = iqpar(iq)         ! iqf = iq(fbz)
      iqi = q%sstar(iqf, 1) ! iqi = iq(ibz)
      isi = q%sstar(iqf, 2)

      call print_qpar(iq,iqf,iqi,qpar(iq,:),q%ptbz(iqf,:),q%pt(iqi,:))
      call setup_iGzrot(iqi, isi,   ngblk )     
      call modify_Xgspace( iqi,  ngblk )

      call select_wgrid(wq, iq) ! takes the grid for THIS iq
      call add_broad(wq)

!     call get_reels_epsm1av

      select case(iqmin-iq)
      case(0)
        !
        ! Calculate eps^1 for the irreducible q (IBZ)
        !
        call calc_eps( iqi, Xen, Xk, X, q, wq, epsm1, ngblk, nw, iqf )
        !
        ! Transform eps^1 to the reducible q (FBZ)
        !
        call rotate_eps( iqi, iqf, isi, epsm1 )
        !
        ! If no symmetry present, force to be symmetric (physical) in plane
        !
        call average_inveps  
        !
        ! No interpolation for iq = 1
        !
        epsm1_avA(:,:,1:nwp) = epsm1(:,:,nwp:nw) ! copy upper half for next iq
        epsm1_av(1:ngblk,1:ngblk,1:nwp) = epsm1(1:ngblk,1:ngblk,1:nwp)
        !
      case default 
!       call section('=','Next iq')
        !
        call calc_eps( iqi, Xen, Xk, X, q, wq, epsm1, ngblk, nw, iqf)
        !
        call rotate_eps( iqi, iqf, isi, epsm1 )
        !
        call average_inveps  
        !
        epsm1_avB(:,:,1:nwp) = epsm1(:,:,   1:nwp ) ! Use for interpolation
        !
        ! Interpolate eps^-1 for iq-1 and iq exact frequencies
        !
        call interpolate_inveps( epsm1_av, epsm1_avA, epsm1_avB )
        !
        epsm1_avA(:,:,1:nwp) = epsm1(:,:, nwp:nw  ) ! For next iq average
        !
      end select
      ! 
      if(lanalytical)      call calc_loss_analytical(lossq,epsm1_av,g_vec)
!     if(.not.lanalytical) call calc_loss_numerical
      !
      ! The bulk part ?
      !
      ! call calc_loss_bulk(lossbq, real(wq%p(1:nwp)), eps_b, nwp )
      !
      !  Write loss for this qpar and group of frequencies
      !
      call write_loss( real(wq%p(1:nwp)), lossq, nwp, ipol)

!     Save to single array for rel. difference calculations...
      loss(ipol,1:5,grange(iq,1):grange(iq,2)) = lossq(1:5,1:nwp) 

    enddo ! end of loop on qpar for this ipol

!   Deallocations
!   Will need to improve data hiding: many of these are not used here.

    call cleanup_eels_qpar ! <- USE them in here!

!   call X_alloc(X,-3) ! Deallocate rhoq0 to enable recalculating for q -> Y.

  enddo    ! Loop on npol

  call clean_bulkeps

  if (npol.eq.2) then
!   Remap onto fine mesh, broaden, etc
!   Calculate eels difference if possible...call a general module used
!   by another code for post-processing.
!   call remap()
!   call CalcREELSdiff(loss(1,:,:),loss(2,:,:)eps_x, eps_y)
  endif

  deallocate(lossq)
  deallocate(epsm1)
  deallocate(epsm1_av)
  deallocate(epsm1_avA)
  deallocate(epsm1_avB)
! deallocate(wq%cgx,wq%npcg,wq%pcg,wq%cgt,wq%p)
  call pp_redux_wait

  return

contains

  subroutine input_reels( lfail)
    implicit none
    logical,            intent(out) :: lfail
!   call input_surface_geometry( lfail)
    call print_surface_geometry

!   call input_eels_kin( lfail )
    call print_eels_kin

!   call input_eels_det( lfail)
    call print_eels_det

!   call input_eels_int( lfail)
    call print_eels_int
    return
  end subroutine input_reels

  subroutine print_reels
    !
    ! General runtime options/flags
    !
    use reels_module,      ONLY : npol, lepsmix
    implicit none

    if(lepsmix) call msg('r','Linear interpolation between eps(q) and eps(q+1).')
    if(.not.lepsmix) call msg('r','No interpolation between eps(q) and eps(q+1).')
    if(npol.eq.1) call msg('r','Single q-polarization calculation selected.')
    if(npol.eq.2) call msg('r',&
&           'Double q-polarization and relative difference calculation selected.')
    return
  end subroutine print_reels
  
  subroutine cleanup_eels_qpar
    implicit none
    deallocate(loss)
    deallocate(grange)
    deallocate(eps_b)
    deallocate(hwqgrid)
    deallocate(hwgrid)
    deallocate(iGz0indx)
    deallocate(iGzrotindx)
    deallocate(iqpar)
    deallocate(qpar)
    deallocate(qGwx)
    call end_prefactors
  end subroutine cleanup_eels_qpar

end subroutine reelsdriver

subroutine write_loss( hw, loss, nw, ipol)
  use com,                    ONLY : msg, of_open_close
  use pars,                   ONLY : SP, schlen, HARTREE
  implicit none
  integer,          intent(in)    :: ipol, nw
  real(SP),         intent(in)    :: loss(5,nw), hw(nw)
! ws
  real(SP)                        :: od(8)
  integer                         :: iw
  character(schlen)               :: of_name
! perhaps this needs ot be called once to open, and then append?

  write (of_name,'(a,a)') 'reels','-rpa'
  call of_open_close(of_name,'ot')

  do iw = 1, nw
    od = (/ hw(iw)*HARTREE, &
&           loss(1,iw), loss(2,iw), loss(3,iw), &
&           loss(4,iw), loss(5,iw), loss(5,iw), &
&           loss(5,iw) /) 
    call msg('o reels','',od, INDENT=0, USE_TABS=.true.)
  enddo
  call of_open_close(of_name)
 
  return
end subroutine write_loss

subroutine setup_output_reels(qname, q0, ipol)
  use pars,                   ONLY : SP, schlen
  use com,                    ONLY : msg, of_open_close
  use vec_operate,            ONLY : v_module
  implicit none
  character(schlen)               :: of_name
  character(len=1), intent(in)    :: qname
  real(SP),         intent(in)    :: q0(3)
  integer,          intent(in)    :: ipol
! wc
  character(10)                  :: headings(7)
  character(schlen)              :: lch, filename
 
! write (ioe%name,'(a,a,a)') 'reels_q',qname,'-rpa'
  write (of_name,'(a,a)') 'reels','-rpa'
  call of_open_close(of_name,'ot')

!    Titles for output file column data
  headings(1:7) = (/' E/ev[1]  ','loss(1)[2]','loss(2)[3]','loss(3)[4]', &
&                              'loss(4)[5]','loss(5)[6]','loss(6)[7]'/)

  write (lch,'(3a,3f10.5)') &
&               'REELS @ q || ',qname,' : ', q0/v_module(q0)
  call msg('o reels','#',trim(lch)) ! <-- writes to file...
  call msg('r','REELS output directed to: '//trim(lch)) ! <-- writes to file...

  call msg('o reels','#',headings(:7), INDENT=0,USE_TABS=.true.)

! call pp_redux_wait ! Why?

  return
end subroutine setup_output_reels

! subroutine init_reels(defs)
!   use pars,                  only : SP, schlen
!   use it_m,                  only : it, initdefs, E_unit,G_unit,T_unit
!   use eels_integration,      only : dc, dv, dsurf, lanalytical
!   use reels_module,          only : npol, lepsmix
!   implicit none
!   type(initdefs), intent(inout)  :: defs
!   integer, parameter             :: V_more=1, V_qp=2, V_io=3, V_debug=4
!   real(SP)                       :: limits(3)
!   character(schlen)              :: inttype, mixing
!   
!   inttype = "analytical"! EELS integration method (analytical/numerical)
!   limits = (/ -10.0, 0.0, 10.0 /) ! Integration limits (vac/surf/slab)
!   mixing = "off"
!   npol = 1
! 
 !  call it(defs,'EELIntMd', &
! &      '[REELS] EELS integration method (`analytical`,`numerical`)', inttype )
!   call it(defs,'IntLimit', '[REELS] Integration limits vac/surf/slab (a.u.)', limits )
!   call it(defs,'EpsMix'  , '[REELS] DF mixing (`on`;`off`)', mixing )
!   call it(defs,'npol'  , '[REELS] Number of polarizations (`1`,`2`)', npol )
! 
!   lanalytical = merge (.true., .false., trim(inttype).ne."numerical")
!   dv       = limits(1)
!   dsurf    = limits(2)
!   dc       = limits(3)
!   lepsmix = merge(.true., .false., trim(mixing).eq."on")

! gbroad could be here too
  
!   return
! end subroutine init_reels

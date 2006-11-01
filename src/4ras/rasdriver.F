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
subroutine rasdriver(Xen,Xk,q,wv,X) ! remove q
    
  use pars,                    only : SP, PI, schlen, lchlen
  use com,                     only : msg
  use memory_m,                only : mem_est
  use parser_m,                only : parser
  use optcut,                  only : loptcut, setup_cutoff, print_cutoff
  use electrons,               only : levels
  use R_lattice,               only : bz_samp, bare_qpg
  use IO_m,                    only : io_control, OP_RD_CL, OP_WR_CL, VERIFY, REP, DUMP
  use X_m,                     only : X_t, X_epsilon, X_mat, X_alloc
  use frequency,               only : w_samp 
  use optcut,                  only : loptcut, Xcut_rhoq0 ! just for dealloc
  use vec_operate,             only : v_module
  use par_proc_m,              only : pp_redux_wait
  use wave_func,               only : WF_free
  use ras_module
  use surface_geometry
  use bulkeps,                 only : lbulkerr, GetBulkEps
  use eels_kinematics
  use eels_detector
  use analyse_peaks
  use model_loss_function,     only : lossformlg, lossfm, print_eels_form
  !use reels_module,            only : write_reelsdiff
  implicit none
  type(levels),       intent(in) :: Xen    ! Energies
  type(bz_samp),      intent(in) :: Xk, q     ! K points
  type(X_t),       intent(inout) :: X      ! Polarization
  type(w_samp)                   :: wv     ! Energy ranges
!ws
  integer                        :: fr(2),iq, ipol, iw
  real(SP)                       :: qpard(2), q0v(3,3)
  character(schlen)              :: lch

! Full spectrum arrays 
  complex(SP), allocatable       :: eps_b(:), eps_i(:,:), surf_i(:,:)
  real(SP),  allocatable         :: lossf(:,:)

  integer                        :: nw, npol, io_err, ID
  logical                        :: lfail, lsurferr, lreelserr, lraserr
  character(lchlen)              :: bulkmsg
! External functions
  integer, external              :: ioslabeps

  call section('*','Surface spectroscopy module')

  lbulkerr = .false.; lsurferr = .false.; lraserr = .false.; lreelserr = .false.
  lfail = .false.
  !
  ! Polarizations
  !
  if(.not.lras.and..not.lreels) return 
  npol = 2
  if(lreels) npol = 3
  !
  ! Surface geometry and spectral analysis
  !
  call section('+','Surface geometry and spectral analysis')
  call setup_surface_geometry( lfail )
  call print_surface_geometry
  call setup_cutoff( lfail )
  call print_cutoff
  call print_analyse_peaks
  !
  ! REELS input parameters
  !
  if(lreels) then
    call section('=','REELS setup')
    call setup_eels_kin( lfail )
    if(lfail) lreelserr = .true.
    call print_eels_kin
    call setup_eels_det( lfail)
    if(lfail) lreelserr = .true.
    call print_eels_det
    call check_eels_det( lfail )
    if(lfail) lreelserr = .true.
    call print_eels_form
    call print_eels_geometry
    call print_eels_general( npol )
  endif
  q0v(:,1) = q0x ; q0v(:,2) = q0y ; q0v(:,3) = norm
  !
  ! Energy grids and energy dependent arrays
  !
  call freqs_setup(wv) 
  call X_checks(Xen,X)
  nw = wv%n(1) ! Have to use freqs_setup in case wv%n(1) is lowered (%)

  allocate( eps_i(nw, npol) )  
  allocate( eps_b(nw) )
  allocate( lossf(nw,4) )
  !
  ! Read and process the bulk data
  !
  call section('=','Bulk epsilon data import')
  call GetBulkEps(real(wv % p), nw, eps_b, bulkmsg)
  if(lbulkerr) lreelserr = .true.
  if(lbulkerr) lraserr = .true.
  call msg('nrs',bulkmsg)
  if(lbulkerr) call msg('nrs','Calculation will continue, but '//&
&   'quantities requiring the bulk epsilon will not be computed.')
  !
  ! Check for db.surfeps, and read all eps_i if present
  !
  call section('=','Check for eps cell database...')
  call io_control(ACTION=OP_RD_CL, COM=REP, MODE=VERIFY, SEC=(/1,2/), ID=ID)
  io_err = ioslabeps(eps_i, X, Xk, Xen, wv, q0v, npol, ID)
  if (io_err.eq.0) then
    goto 1 ! Skip calculation of X. Nasty Fortran. Check deallocates.
  endif
  call section('=','Calculating eps cell.')

  allocate( X_epsilon(2,nw) ) 
  !
  ! Main loop over polarizations
  !
  do ipol = 1, npol

    iq = 1

    select case(ipol)
    case(1)
       X%q0 = q0x
       qname = 'x'
    case(2)
       X%q0 = q0y
       qname = 'y'
       q0v(:,2) = q0y
    case(3)
       X%q0 = norm
       qname = 'z'
       q0v(:,3) = norm
    end select
    write (lch,'(3a,3f10.5)') &
&            'Absorption @ q || ',qname,' : ', X%q0/v_module(X%q0)
    call msg('ns',trim(lch)) ! <-- to std out + newline
       
    call X_alloc('X',(/X%ng,X%ng,wv%n(2)/))

    X_epsilon = (0.,0.)

    do iw = 1, wv % n(1), wv % n(2) ! loop over blocks
       
      fr = (/ iw, iw+wv%n(2)-1 /) ! the small block of frequencies
       
      call X_os(X_mat,iq,fr,Xen,Xk,wv,X)
       
      X_epsilon(1, fr(1):fr(2)) = 1.0_SP - X_mat(1,1,:)*4.0_SP*pi/ &
&                                 bare_qpg(iq,1) / bare_qpg(iq,1)
       
      call X_s(iq,fr,X,wv) 
       
      X_epsilon(2,fr(1):fr(2))=1./(X_mat(1,1,:)+1.)   ! LFEs included, if no cutoff
       
    enddo 
    !
    ! Clean up for each q
    !
    call X_alloc('X') 
    !
    ! Copy to slab eps. array
    !
    do iw = 1, nw
      eps_i(iw,ipol) = cmplx( real(X_epsilon(2,iw)), aimag(X_epsilon(2,iw)), SP )
    enddo
    call X_alloc('OptOsc')  ! X_rhoq0
    if(loptcut) deallocate(Xcut_rhoq0)
     
  enddo    
  deallocate(X_epsilon)

  call section('=','Write eps surf database: ')
  call io_control(ACTION=OP_WR_CL, COM=REP, MODE=DUMP, SEC=(/1,2/), ID=ID)
  io_err = ioslabeps(eps_i, X, Xk, Xen, wv, q0v, npol, ID)

1 continue ! db.slabeps exists; eps_i has been read
  !
  ! Dump the eps_i (cell) to file for plotting
  !
  call section('=','Write eps cell for plotting:')
  call write_eps_tensor( q0v, eps_i, real(wv % p), nw, npol, 'cell' ) 
  !
  ! Clean up a bit
  !
  !
  ! Find slab symmetries
  !
  allocate( surf_i(nw, npol ) )
  call section('=','Slab symmetries: ')
  call SpaceInvTest(Xen, Xk) 
  call get_slab_symmetry( loptcut) 
  !
  ! Extract surface dielectric function from cutoff/bulk subtraction
  !
  call section('=','Surface dielectric function')
  call extract_eps_surf(surf_i, eps_i, npol, eps_b, nw, lbulkerr, lsurferr) 
  if(lsurferr) lraserr = .true. ;  if(lsurferr) lreelserr = .true.
  if(.not.lsurferr) call write_eps_tensor( q0v, surf_i, real(wv % p), nw, npol, 'surf' ) 
  !
  ! Calculate RAS and REELS in 3-layer method
  !
  if (lras.and..not.lraserr) then
    !
    call section('=','RAS')
    call CalcRAS(real(wv % p), surf_i(1:nw,1), surf_i(1:nw,2), &
&                              eps_b(1:nw), nw)
    !
  endif
  !
  if(lreels.and..not.lreelserr) then
    
    lch = 'REELS in model form: '//trim(lossformlg(lossfm))
    call section('=',trim(lch) )
    call CalcREELS(lossf, real(wv % p), surf_i, eps_b, nw, qpard)
!<- DEBUG
!   call msg('nrs','Q_|| * d_surf range: ',qpard)
!-> DEBUG
    !call write_reelsdiff(lossf, real(wv % p), nw ) 
    !
  endif
! checked:
! eps_i eps_b surf_i
! wv%p
!
!
  !
  ! Clean up
  !
  deallocate( eps_i, eps_b, surf_i )
  deallocate(wv%p)
  call mem_est("W-p")
  deallocate(lossf)
  deallocate(Xk%pt)
  call mem_est("k_pt")
  call WF_free()
  call ras_clean

  deallocate(Xen%qp_done)
  call mem_est("en-qp_done")

  call pp_redux_wait

  return
end subroutine rasdriver

subroutine ras_clean
 use memory_m,      only:mem_est
 use bulkeps,        only : clean_bulkeps
 use surface_geometry, only : gvecaff
 use optcut, only : end_optcut,loptcut
  use frequency,     only:rg_index_bg,bg_npts,cg_pt,cg_index_bg
  use X_m,                     only : X_poles_tab
  use R_lattice,        only: g_vec, bare_qpg, g_rot, qindx_free
  use D_lattice,        only: dl_sop
  use wave_func,        only: ng_wf, wf_igk

  call qindx_free
  call clean_bulkeps
  if(allocated(gvecaff)) deallocate(gvecaff)
  if(loptcut) call end_optcut

  deallocate(g_vec,dl_sop,ng_wf,wf_igk)
  call mem_est("g_vec dl_sop ng_wf wf_igk")

  if(allocated(X_poles_tab)) then
    deallocate(X_poles_tab)
    call mem_est(" X_poles_tab ")
  endif

  if(allocated(rg_index_bg)) call mem_est("RGi BGn CGp CGi")
  if(allocated(rg_index_bg)) deallocate(rg_index_bg)
  if(allocated(bg_npts)) deallocate(bg_npts)
  if(allocated(cg_pt)) deallocate(cg_pt)
  if(allocated(cg_index_bg)) deallocate(cg_index_bg)

  deallocate(bare_qpg)
  call mem_est("bare_qpg")

  if(allocated(g_rot)) deallocate(g_rot)

  return
end subroutine ras_clean


subroutine ras2close
 use memory_m,      only:mem_est
  use R_lattice,        only: ng_in_shell,E_of_shell
  if(allocated(E_of_shell)) then
    deallocate(E_of_shell)
!   call mem_est(" RL_Eshells ")
  endif
  if(allocated(ng_in_shell)) deallocate(ng_in_shell)
! call mem_est(" RL_Gshells ")

end subroutine ras2close
 
subroutine print_eels_general( npol )
  use com, only : msg
  implicit none
  integer, intent(in)      :: npol

  if(npol.eq.2) call msg('r',&
&   'Double q-polarization and relative difference calculations [RAS].')
  if(npol.eq.3) call msg('r',&
&   'Triple q-polarization calculation selected [RAS, HREELS diff].')
  return
end subroutine print_eels_general

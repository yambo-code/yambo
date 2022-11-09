!
subroutine surfdriver(Xen,Xk,q,wv,X,Dip) 
    
  use pars,                    only : SP, PI, schlen, lchlen, cZERO
  use units,                   only : HA2EV
  use com,                     only : msg
  use DIPOLES,                 only : DIPOLE_t
  use parser_m,                only : parser
  use optcut,                  only : setup_cutoff, print_cutoff, dc
  use electrons,               only : levels
  use R_lattice,               only : bz_samp, bare_qpg, q_norm
  use IO_int,                  only : io_control,IO_and_Messaging_switch
  use IO_m,                    only : OP_RD_CL, OP_WR_CL, VERIFY, REP, DUMP
  use X_m,                     only : X_t, Alpha, X_par, X_par_lower_triangle
  use frequency,               only : w_samp 
  use optcut,                  only : loptcut
  use vec_operate,             only : v_norm
  use parallel_int,            only : PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_index,PARALLEL_WF_distribute
  use parallel_m,              only : master_cpu
  use wave_func,               ONLY : WF,WF_buffer,WF_buffered_IO
  use interfaces,              only : WF_free, WF_load
  use matrix,                  only : MATRIX_reset
  use ras_module
  use surface_geometry
  use bulkeps,                 only : lbulkerr, GetBulkEps
  use eels_kinematics
  use eels_detector
  use model_loss_function,     only : lossformlg, lossfm, print_eels_form
#include<memory.h>
  !implicit none
  type(levels),       intent(in) :: Xen    ! Energies
  type(bz_samp),   intent(inout) :: Xk, q     ! K points
  type(X_t),       intent(inout) :: X      ! Polarization
  type(w_samp)                   :: wv     ! Energy ranges
  type(DIPOLE_t)                 :: Dip
!ws
  integer                        :: fr(2),iq, ipol, iw
  real(SP)                       :: qpard(2), q0v(3,3)
  character(schlen)              :: lch

! Full spectrum arrays 
  complex(SP), allocatable       :: eps_b(:), eps_i(:,:), surf_i(:,:)
  real(SP),  allocatable         :: lossf(:,:)

  integer                        :: nw, npol, io_err, ID
  logical                        :: lfail, lsurferr, lreelserr, lraserr, Macroscopic_CPU
  character(lchlen)              :: bulkmsg

  call section('*','Surface spectroscopy module')

  lbulkerr = .false.; lsurferr = .false.; lraserr = .false.; lreelserr = .false.
  lfail = .false.; Macroscopic_CPU = .true.
  !
  ! Polarizations
  !
  if(.not.lras.and..not.lreels) return 
  npol = 2
  if(lreels) npol = 3

  call FREQUENCIES_setup(wv)
  nw = wv%n_freqs ! Have to use freqs_setup in case wv%n(1) is lowered (%)

  ! Surface geometry and spectral analysis
  !
  call section('+','Surface geometry and spectral analysis')
  call setup_surface_geometry(lfail)
  call print_surface_geometry
  call setup_cutoff(lfail)
  call print_cutoff
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
  !
  allocate( eps_i(nw,npol) )  
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
  ! Compute epsilon
  !
  call section('=','Calculating eps cell.')

  YAMBO_ALLOC(Alpha,(nw,2))
  !
  ! Main loop over polarizations
  call X_pre_setup(Xen,Xk,X)
  allocate(X_par(1))
  ! 
  do ipol = 1, npol 

  call PARALLEL_global_indexes(Xen,Xk,q,"Response_G_space",X=X)
  call X_half_mat_check(Xen,Xk,wv)
  !call X_PARALLEL_alloc(X_par(1),X%ng,wv%n_freqs,"XUP")
  call X_ALLOC_parallel(X_par(1),X%ng,wv%n_freqs,"XUP")

  call IO_and_Messaging_switch("+output",CONDITION=master_cpu)

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
&            'Absorption @ q || ',qname,' : ', X%q0/v_norm(X%q0)
    call msg('ns',trim(lch)) ! <-- to std out + newline
       

    Alpha=cZERO  

    do iw = 1, wv % n_freqs, wv %  n_freqs! loop over blocks
       
      fr = (/ iw, iw+wv%n_freqs-1 /) ! the small block of frequencies
       
      call X_irredux(iq,"Xo",X_par(1),Xen,Xk,wv,X,Dip)
      if(loptcut) X_par(1)%blc(1,1,:)=X_par(1)%blc(1,1,:)*az/dc
      if (Macroscopic_CPU) Alpha(fr(1):fr(2),1)=1.0_SP-X_par(1)%blc(1,1,:)*4.0_SP*pi/q_norm(iq)**2
       
    enddo
    !
    ! Clean up for each q
    !
    !call X_alloc('X') 
    call MATRIX_reset(X_par(1))
    YAMBO_FREE(X_par_lower_triangle%blc)
    call MATRIX_reset(X_par_lower_triangle)
    !
    ! Copy to slab eps. array
    !
    do iw = 1, nw
      eps_i(iw,ipol) = Alpha(iw,1)
    enddo
  enddo    
  !
  ! Dump the eps_i (cell) to file for plotting
  !
  call section('=','Write eps cell for plotting')
  call write_eps_tensor( q0v, eps_i, real(wv % p), nw, npol, 'cell' ) 
  !
  ! Find slab symmetries
  !
  allocate( surf_i(nw, npol ) )
  call section('=','Slab symmetries ')
  call get_slab_symmetry(loptcut) 
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
&                              eps_b(1:nw), nw, d_surf,"-rpa")
    !
  endif
  !
  if(lreels.and..not.lreelserr) then
    
    lch = 'REELS in model form: '//trim(lossformlg(lossfm))
    call section('=',trim(lch) )
    call CalcREELS(lossf, real(wv % p), surf_i, eps_b, nw, qpard)
    call write_reelsdiff(lossf, real(wv % p), nw ) 
    !
  endif
  !
  ! Clean up
  !
  call FREQUENCIES_reset(wv,"all")
  YAMBO_FREE(Alpha)
  deallocate( eps_i, eps_b, surf_i )
  deallocate(lossf)
  deallocate(X_par)
  YAMBO_FREE(wv%p)
  YAMBO_FREE(Xk%pt)
  call WF_free(WF)
  call ras_clean
  !call PP_redux_wait

  return
end subroutine surfdriver

subroutine ras_clean
 use bulkeps,          only : clean_bulkeps
 use surface_geometry, only : gvecaff
 use optcut,           only : end_optcut,loptcut
 use frequency,        only : coarse_grid_Pt, ordered_grid_index, coarse_grid_index, bare_grid_N
 use X_m,              only : X_poles_tab
 use R_lattice,        only : g_vec, bare_qpg, g_rot, qindx_free
 use D_lattice,        only : dl_sop
 use wave_func,        only : wf_nc_k, wf_igk
#include<memory.h>

  call qindx_free('ALL')
  call clean_bulkeps
  if(allocated(gvecaff)) deallocate(gvecaff)
  if(loptcut) call end_optcut
  
  YAMBO_FREE(g_vec)
  YAMBO_FREE(dl_sop)
  YAMBO_FREE(wf_nc_k)
  YAMBO_FREE(wf_igk)

  if(allocated(X_poles_tab)) then
    YAMBO_FREE(X_poles_tab)
  endif

  if(allocated(ordered_grid_index)) then
    YAMBO_FREE(ordered_grid_index)
  endif
  if(allocated(bare_grid_N)) deallocate(bare_grid_N)
  if(allocated(coarse_grid_Pt)) deallocate(coarse_grid_Pt)
  if(allocated(coarse_grid_index)) deallocate(coarse_grid_index)

  YAMBO_FREE(bare_qpg)

  if(allocated(g_rot)) deallocate(g_rot)

  return
end subroutine ras_clean


subroutine ras2close
  use R_lattice,        only: ng_in_shell,E_of_shell
  if(allocated(E_of_shell)) then
    deallocate(E_of_shell)
  endif
  if(allocated(ng_in_shell)) deallocate(ng_in_shell)

end subroutine ras2close
 
subroutine print_eels_general( npol )
  use com, only : msg
  implicit none
  integer, intent(in)      :: npol

  if(npol.eq.2) call msg('r',&
&   'Double q-polarization and relative difference calculations [RAS].')
  if(npol.eq.3) call msg('r',&
&   'Triple q-polarization calculation selected [RAS, HREELS].')
  return
end subroutine print_eels_general

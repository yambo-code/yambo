!
! Copyright (C) 2000-2009 C. Hogan and the YAMBO team
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
subroutine calc_loss_function(ipol, Xen, Xk, X, q, wq)
  use pars,                    only : SP, lchlen 
  use units,                   ONLY : HARTREE
  use memory_m,                only : mem_est
  use com,                     only : msg
  Use electrons,               only : levels
  use R_lattice,               only : bz_samp
  use X_m,                     only : X_t, X_poles_tab, X_alloc
  use frequency,               only : w_samp, rg_index_bg,bg_npts,cg_pt, cg_index_bg
  use eels_qpt,                only : iqmin, iqmax, qpar_1BZ, qpar_sort, qpar_hw
  use wave_func,               only : wf_ng
  use eels_wgrid,              only : init_wgrid, wqgrid, wgrid, print_wgrid, hwgrid, &
&                                     nw, nwp, nwtot, wr, nwsec, get_nwsec
  use intchar,                 only : intc
  use bulkeps,                 only : lbulkerr, generate_bulkeps
  use eels_integration,        only : lanalytical, calc_zk, calc_loss_analytical
  use eels_scattering,         only : scattering_frame, electron_velocity, &
&                                     init_prefactors, calc_prefactors, print_prefactors
  use eels_gvec,               only : setup_iGz
  use reels_module,            only : npol, lepsmix
  use eels_inveps,             only : epsm1_avA
  use loss_module,             only : loss, alloc_loss
  ! Just for deallocation:
  ! X_poles_tab, rg_index_bg,bg_npts,cg_pt, cg_index_bg
  implicit none
  type(levels),       intent(in) :: Xen    ! Energies
  type(bz_samp),      intent(in) :: Xk, q  ! K/Q points
  type(X_t),       intent(inout) :: X      ! Polarization
  type(w_samp),    intent(inout) :: wq     ! Energy ranges
! Local arrays
  integer,            intent(in) :: ipol
  complex(SP), allocatable       :: eps_b(:)
! real(SP)                       :: lossqw(4, nw ), lossbq(nw) ! dynamic or largest?
  real(SP), allocatable          :: lossqw( :,: ), lossbq(:) ! dynamic or largest?
  complex(SP), allocatable       :: epsm1_av(:,:,:)
  integer                        :: iq
  character(lchlen)              :: bulkmsg


  call section('+','Scattering frames set up.')
  call scattering_frame( X%q0 )
  call electron_velocity

  call section('=','Generate Q_|| vs hw table.')
  call qpar_1BZ( X%q0, q )
  call qpar_sort( X%q0, q )
  call qpar_hw( X%q0, q,  wq) ! sets nqeels

  call section('=','Generate Gz tables.')
  call setup_iGz( iqmin, iqmax )

  call section('=','EELS integration setup.')
  if(.not.lanalytical) then
    call calc_zk
  endif

  call section('=','Energy grid setup.')
  call init_wgrid(wq)
  call wqgrid(wq)
  call wgrid                   ! sets nwtot
  call print_wgrid( wq )

  call alloc_loss( 4, nwtot )

  call section('=','Kinematic prefactors setup.')
  call init_prefactors
  call calc_prefactors
  call print_prefactors

! call section('=','Check in-plane symmetries.')
! call in_plane_symmetry(lplansym)
  !
  ! Generate the bulk data for this grid
  !
  call section('=','Bulk epsilon')
  if(.not.lbulkerr) then
    ! Size depends on how calculate the det int over bulk term...
    !  full array or part? nwtot or nwp?
    allocate( eps_b(nwtot) )
    call generate_bulkeps( hwgrid, nwtot, eps_b, bulkmsg)
    if(lbulkerr) call msg('nrs',trim(bulkmsg))
  endif
  !
  allocate( epsm1_avA(X%ng, X%ng, nwp) )
  !
  call write_loss_sec( 0 )
  !
  call section('*','Main REELS loop')
  call section('+','REELS for iq:'//intc(1)//'/'//intc(iqmax-iqmin+1))

  do iq = iqmin, iqmax     ! Loop over qpar for this polarization (iq = qpar index)

    if(iq.ne.iqmin) call section('=','REELS for iq:'//intc(iq-iqmin+1)//&
&                                             '/'//intc(iqmax-iqmin+1))
    !
    ! Define nwsec for mixing
    !
    nwsec = get_nwsec(iq)
    allocate( epsm1_av ( X%ng, X%ng, nwsec) )
    allocate( lossqw   ( 4, nwsec) )

    write(*,*) "call average epsm1"
    call average_epsm1_q(epsm1_av, iq, Xen, Xk, X, q, wq)
    write(*,*) "call average epsm1 done",wq%p(1)*HARTREE
    ! The correct grid should now be in wq!
    !
    if(lanalytical)      call calc_loss_analytical(lossqw,iq,epsm1_av)
    write(*,*) "call loss analytical done",nwp,wr(iq,:)

!   if(.not.lanalytical) call calc_loss_numerical
    !
    ! The bulk part: could be outside loop
    !
    ! call calc_loss_bulk(lossbq, real(wq%p(1:nwp)), eps_b, nwp )
    !
    ! Write loss for this qpar and group of frequencies (nwsec block)
    !
    call write_loss_sec( 1 )
    !
    write(*,*) "call hw range", wr(iq,:)
!   loss( 1:4, wr(iq,1):wr(iq,2) ) = lossqw(1:4,1:wr(iq,3))
    if(lepsmix) then
      loss( 1:4, wr(iq,1):wr(iq,2) ) = lossqw(1:4,1:nwsec)
    else
      if(iq.eq.iqmin) loss( 1:4, wr(iq,1):wr(iq,2) ) = lossqw(1:4,1:nwsec)
      ! this is the only place where loss != 1:nwsec
      if(iq.ne.iqmin) loss( 1:4, wr(iq,1):wr(iq,2) ) = &
&                                         lossqw(1:4, nwp:nwsec)
    endif
    !
    ! Clean up q-dependent allocates
    !
    deallocate(epsm1_av,lossqw)
    if (allocated(rg_index_bg)) deallocate(rg_index_bg)
    deallocate(X_poles_tab, bg_npts,cg_pt,cg_index_bg)
    call mem_est("X_poles_tab RGi BGn CGp CGi")
    !
  enddo ! end of loop on qpar for this ipol

  write(*,*) "done main loop"

! Deallocations
! Will need to improve data hiding: many of these are not used here.
! call write_reels_sec( real(wq%p(1:nwp)), lossqw, nwp, ipol, X%q0,2)
  call write_loss_sec( 2 )
  call cleanup_eels_qpar ! <- USE them in here!
  call X_alloc('OptOsc') ! Deallocate rhoq0 to enable recalculating for q -> Y.

  return
contains

!subroutine cleanup_eels_qpar
!end cleanup_eels_qpar
subroutine write_loss_sec( imode )
  use com,                    only : msg, of_open_close
  use pars,                   only : schlen, lchlen
  use units,                  only : HARTREE 
  use eels_wgrid,             only : hwqgrid
! use reels_module,           only : npol
  use surface_geometry,       only : qname
  use vec_operate,            only : v_norm
  implicit none
! integer,          intent(in)    :: ipol, nw, imode
  integer,          intent(in)    :: imode
! real(SP),         intent(in)    :: loss(5,nw), hw(nw), q0(3)
! ws
  real(SP)                        :: hw(nwsec)
  character(10)                   :: headings(7)
  real(SP)                        :: od(8)
  integer                         :: iw
  character(schlen), save         :: of_name
  character(lchlen)               :: lch

! Maybe this wont work for two files. Might need to use 'o reels_sec'//qname
! because of write at end.

  hw(:) = real(wq%p(1:nwsec))

  select case(imode)
  case(0)

    if(npol.eq.1) then
      write (of_name,'(a,a)') 'reels_sec','-rpa'
      call of_open_close(of_name,'ot')
      write (lch,'(a,3f10.5)') &
&               'REELS @ q || ', X%q0/v_norm(X%q0)
    else if(npol.eq.2) then
      write (of_name,'(a,a,a)') 'reels_sec_',qname,'-rpa'
      call of_open_close(of_name,'ot')
    write (lch,'(3a,3f10.5)') &
&               'REELS @ q || ',qname,' : ', X%q0/v_norm(X%q0)
    endif

    call msg('o reels_sec','#',trim(lch), INDENT=0) ! <-- writes to file...
    call msg('r','REELS output directed to: '//trim(lch))

!    Titles for output file column data
    headings(1:7) = (/' E/ev[1]  ','loss(1)[2]','loss(2)[3]','loss(3)[4]', &
&                              'loss(4)[5]','loss(5)[6]','loss(6)[7]'/)
    call msg('o reels_sec','#',headings(1:7), INDENT=0,USE_TABS=.true.)
    call msg('o reels_sec','#','',INDENT=0)

  case(1)

    call of_open_close(of_name,'oa')
    do iw = 1, nwsec
      od(1:5) = (/ hw(iw)*HARTREE, &
&             lossqw(1,iw), lossqw(2,iw), lossqw(3,iw), &
&             lossqw(4,iw) /)
      call msg('o reels_sec','',od(1:5), INDENT=0, USE_TABS=.true.)
!      od = (/ hw(iw)*HARTREE, &
!&             lossqw(1,iw), lossqw(2,iw), lossqw(3,iw), &
!&             lossqw(4,iw), lossqw(5,iw), lossqw(5,iw), &
!&             lossqw(5,iw) /)
!      call msg('o reels_sec','',od, INDENT=0, USE_TABS=.true.)
    enddo
    call msg('o reels_sec','','')
    call msg('o reels_sec','','')
    call of_open_close(of_name)

  case(2)

    call of_open_close(of_name)

  end select

  return
end subroutine write_loss_sec

subroutine cleanup_eels_qpar
  !
  ! For easier coding: take more care of this when code works
  !
  use eels_qpt,        only : qGwx, iqpar, qpar
  use eels_wgrid,      only : hwqgrid, wr, wqtab
  use eels_gvec,       only : iGz0indx, iGzrotindx
  use eels_scattering, only : end_prefactors
  implicit none
! if(allocated(loss)) deallocate(loss)
  if(allocated(wr)) deallocate(wr)
  if(allocated(hwqgrid)) deallocate(hwqgrid)
  if(allocated(wqtab)) deallocate(wqtab)
  if(allocated(iGz0indx)) deallocate(iGz0indx)
  if(allocated(iGzrotindx)) deallocate(iGzrotindx)
  if(allocated(iqpar)) deallocate(iqpar)
  if(allocated(qpar)) deallocate(qpar)
  if(allocated(qGwx)) deallocate(qGwx)
  if(allocated(epsm1_av)) deallocate(epsm1_av)
  if(allocated(epsm1_avA)) deallocate(epsm1_avA)
  if(allocated(eps_b)) deallocate(eps_b)
  call end_prefactors
end subroutine cleanup_eels_qpar

end subroutine calc_loss_function


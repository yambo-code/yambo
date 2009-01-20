
subroutine write_reels_sec( hw, loss, nw, ipol, q0, imode)
  use com,                    only : msg, of_open_close
  use pars,                   only : SP, schlen, lchlen
  use units,                  ONLY : HARTREE
  use reels_module,           only : npol
  use surface_geometry,       only : q0x, q0y, qname
  use vec_operate,            only : v_norm
  implicit none
  integer,          intent(in)    :: ipol, nw, imode
  real(SP),         intent(in)    :: loss(4,nw), hw(nw),q0(3)
! ws
  character(10)                   :: headings(7)
  real(SP)                        :: od(8)
  integer                         :: iw
  character(schlen)               :: of_name
  character(lchlen)               :: lch
! perhaps this needs ot be called once to open, and then append?

! Maybe this wont work for two files. Might need to use 'o reels_sec'//qname
! because of write at end.

  select case(imode)
  case(0)

    if(npol.eq.1) then  
      write (of_name,'(a,a)') 'reels_sec','-rpa'
      call of_open_close(of_name,'ot')
      write (lch,'(a,3f10.5)') &
&               'REELS @ q || ', q0/v_norm(q0)
    else if(npol.eq.2) then  
      write (of_name,'(a,a,a)') 'reels_sec_',qname,'-rpa'
      call of_open_close(of_name,'ot')
    write (lch,'(3a,3f10.5)') &
&               'REELS @ q || ',qname,' : ', q0/v_norm(q0)
    endif

    call msg('o reels_sec','#',trim(lch), INDENT=0) ! <-- writes to file...
    call msg('r','REELS output directed to: '//trim(lch)) 

!    Titles for output file column data
    headings(1:7) = (/' E/ev[1]  ','loss(1)[2]','loss(2)[3]','loss(3)[4]', &
&                              'loss(4)[5]','loss(5)[6]','loss(6)[7]'/)
    call msg('o reels_sec','#',headings(1:7), INDENT=0,USE_TABS=.true.)
    call msg('o reels_sec','#','',INDENT=0)

  case(1)

    call of_open_close(of_name,'a')
    do iw = 1, nw
      od(1:5) = (/ hw(iw)*HARTREE, &
&             loss(1,iw), loss(2,iw), loss(3,iw), &
&             loss(4,iw) /) 
      call msg('o reels_sec','',od(1:5), INDENT=0, USE_TABS=.true.)
!      od = (/ hw(iw)*HARTREE, &
!&             loss(1,iw), loss(2,iw), loss(3,iw), &
!&             loss(4,iw), loss(5,iw), loss(5,iw), &
!&             loss(5,iw) /) 
!      call msg('o reels_sec','',od, INDENT=0, USE_TABS=.true.)
    enddo
    call msg('o reels_sec','','')
    call msg('o reels_sec','','')
    call of_open_close(of_name)

  case(2)

    call of_open_close(of_name)

  end select
 
  return
end subroutine write_reels_sec

! File			Grid	Data
! 1pol
! o.reels_sec-rpa   	dynamic	4
! o.reels-rpa		regular 1
! 2pol
! o.reels_sec_X-rpa	dynamic 4
! o.reels_sec_Y-rpa 	dynamic 4
! o.reels_diff-rpa	regular 1

subroutine write_reels_x( loss, hw, nw)
  use com,                    only : msg, of_open_close
  use pars,                   only : SP, schlen
  use units,                  only : HARTREE
  implicit none
  integer,          intent(in)    :: nw
  real(SP),         intent(in)    :: loss(nw), hw(nw)
! ws
  real(SP)                        :: od(8)
  integer                         :: iw
  character(schlen)               :: of_name


  write (of_name,'(a,a)') 'reels','-rpa'
  call of_open_close(of_name,'ot')

  do iw = 1, nw
    od(1:2) = (/ hw(iw)*HARTREE, &
&           loss(iw) /)
    call msg('o reels','',od(1:2), INDENT=0, USE_TABS=.true.)
!    od(1:5) = (/ hw(iw)*HARTREE, &
!&           loss(1,iw), loss(2,iw), loss(3,iw), &
!&           loss(4,iw) /)
!    call msg('o reels','',od(1:5), INDENT=0, USE_TABS=.true.)
!    od = (/ hw(iw)*HARTREE, &
!&           loss(1,iw), loss(2,iw), loss(3,iw), &
!&           loss(4,iw), loss(5,iw), loss(5,iw), &
!&           loss(5,iw) /)
!    call msg('o reels','',od, INDENT=0, USE_TABS=.true.)
  enddo
  call of_open_close(of_name)

  return
end subroutine write_reels_x

subroutine write_reelsdiff(lossf, hw, nw )
  use pars,                   ONLY : PI, SP, schlen
  use units,                  ONLY : HARTREE
  use com,                    ONLY : msg, of_open_close
  use convolute
  implicit none
  integer,      intent(in)        :: nw
  real(SP),     intent(in)        :: hw(nw), lossf(nw,2)
! Local
  integer                         :: iw
  real(SP)                        :: od(5), lossx, lossy, diff, rdiff, avg
  real(SP), parameter             :: small = 1.0e-5, zero = 1.0e-5
  real(SP)                        :: lossg(nw,2)
  character(schlen)               :: of_name
  character(10)                   :: headings(5)
  !
  ! Set up REELS difference output (add extra tag to distinguish three-layer or full)
  !
  write(of_name,'(a,a,a)') 'reels_diff','-rpa'
! write(of_name,'(a,a,a)') 'reels_three_layer','-rpa'
  call of_open_close(of_name,'ot')
  !
  ! Output file titles
  !
  headings(1:5) = (/' 1',' 2',' 3',' 4',' 5' /)
  call msg('o reels','#',headings(1:5),INDENT=0,USE_TABS=.true.)
  headings(1:5) = (/'hw (eV) ','REELS(x)','REELS(y)','  Diff  ','Rel Diff' /)
  call msg('o reels','#',headings(1:5),INDENT=0,USE_TABS=.true.)
  !
  ! Add gaussian broadening if necessary
  !
  lossg = lossf
  call convolute_gaussian(lossg(:,1), hw, nw)
  call convolute_gaussian(lossg(:,2), hw, nw)
  !
  ! Write data to file
  !
  do iw = 1, nw
    lossx = lossg(iw,1)
    if(abs(lossx).le.zero) lossx = 0.0 ! numerical problems with hw -> 0...
    lossy = lossg(iw,2)
    if(abs(lossy).le.zero) lossy = 0.0
    diff  = lossx - lossy
    avg   = ( lossx + lossy ) * 0.5_SP
    if(abs(avg).le.small) avg = small
    rdiff = ( lossx - lossy ) / avg
    od = (/ hw(iw)*HARTREE, lossx, lossy, diff, rdiff /)
    call msg('o reels','',od, INDENT=0, USE_TABS=.true.)
  enddo
  !
  call of_open_close(of_name)
  !
  return
end subroutine write_reelsdiff


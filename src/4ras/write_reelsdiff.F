subroutine write_reelsdiff(lossf, hw, nw )
  use pars,                   ONLY : PI, SP, schlen, HARTREE
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


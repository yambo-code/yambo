subroutine write_reelsdiff(lossf, hw, nw )
  use pars,                   ONLY : PI, SP, schlen
  use units,                  ONLY : HA2EV
  use com,                    ONLY : msg, of_open_close
  use convolute
  implicit none
  integer,      intent(in)        :: nw
  real(SP),     intent(in)        :: hw(nw), lossf(nw,2)
! Local
  integer                         :: iw
  real(SP)                        :: od(9), lossx, lossy, diff, rdiff, avg
  real(SP), parameter             :: small = 1.0e-5, zero = 1.0e-5
  real(SP)                        :: lossg(nw,2),d2x(nw), d2y(nw),avgd2loss
  real(SP)                        :: hwev(nw),tmp(nw)
  character(schlen)               :: of_name
  character(10)                   :: headings(9)
  !
  ! Set up REELS difference output (add extra tag to distinguish three-layer or full)
  !
  write(of_name,'(a,a,a)') 'reels_diff','-rpa'
! write(of_name,'(a,a,a)') 'reels_three_layer','-rpa'
  call of_open_close(of_name,'ot')
  !
  ! Output file titles
  !
  headings(1:9) = (/' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9' /)
  call msg('o reels','#',headings(1:9),INDENT=0,USE_TABS=.true.)
  headings(1:9) = (/'hw (eV) ','REELS(x)','REELS(y)','   Avg  ', &
&  '  Diff  ','Rel Diff','-2drv(x)','-2drv(y)','-Avg2Drv' /)
  call msg('o reels','#',headings(1:9),INDENT=0,USE_TABS=.true.)
  !
  ! Add gaussian broadening if necessary
  !
  lossg = lossf
  call convolute_gaussian(lossg(:,1), hw, nw)
  call convolute_gaussian(lossg(:,2), hw, nw)
  !
  ! Calculate derivative spectra
  forall(iw=1:nw) hwev(iw) = hw(iw)*HA2EV
  call second_derivative(d2x,lossg(:,1),hwev,nw) 
  call second_derivative(d2y,lossg(:,2),hwev,nw) 
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
    avgd2loss  = ( d2x(iw) + d2y(iw) ) * 0.5_SP
    od = (/ hwev(iw), lossx, lossy, avg , diff, rdiff , &
&           -d2x(iw), -d2y(iw), -avgd2loss /)
    call msg('o reels','',od, INDENT=0, USE_TABS=.true.)
  enddo
  !
  call of_open_close(of_name)
  !
  return
end subroutine write_reelsdiff

subroutine second_derivative(d2f,f,x,n)
  use pars,       only : SP
  implicit none
  integer, intent(in)  :: n
  real(SP),intent(in)  :: f(n),x(n)
  real(SP),intent(out) :: d2f(n)
  integer              :: iw

  if(n.lt.3) then
    d2f = 0.0_SP
    return
  endif
  do iw=2,n-1
    d2f(iw) = (f(iw+1) + f(iw-1) -2.0_SP*f(iw) ) &
&          / ( x(iw+1)-x(iw) ) / ( x(iw)-x(iw-1) )
  enddo
  d2f(1) = d2f(2)
  d2f(n) = d2f(n-1)
  return
end subroutine second_derivative

!
module bulkeps
  !
  ! Routines and types for reading, storing and processing 
  ! the bulk dielectric function
  !
  use pars,                 ONLY : schlen, lchlen, SP, PI, cI
  use units,                ONLY : HA2EV
  save
  private

  real(SP)                      :: bshift, bbroad
  character(schlen)             :: bfile, btype
  logical                       :: lbulkshift
  !
  ! Tracks if the bulk data has been successfully read or processed.
  !
  logical                       :: lbulkerr
  !
  ! The -original- bulk data is stored here:
  !
  real(SP), allocatable, private:: e1in(:),e2in(:),hwin(:)
  integer, private              :: nline

  public :: init_bulk
  public :: GetBulkEps           ! All in one
  public :: scan_bulkeps, clean_bulkeps, lbulkerr, generate_bulkeps, regrid
  ! 
  ! For input
  ! 
  public :: bshift, bbroad, bfile, btype

contains

subroutine init_bulk(defs)
  use it_tools,              ONLY : it
  use it_m,                  ONLY : initdefs, E_unit,G_unit,T_unit
  implicit none
  integer, parameter             :: V_general=1, V_qp=2, V_io=3
  type(initdefs), intent(inout)  :: defs

  call it(defs,'BulkFile', '[RAS] File containing bulk', bfile )
  call it(defs,'BulkForm', '[RAS] Format of bulk (`3ri`,`3ir`,`2KK`,`3KK`) ',btype )
#if defined _SURF
  call it(defs,'BlkShift', '[RAS] GW shift of bulk eps ', bshift, &
&         E_unit,verb_level=V_general  )
#else
  call it(defs,'BlkShift', '[RAS] GW shift of bulk eps ', bshift, &
&         E_unit  )
#endif
  call it(defs,'BlkBroad', '[RAS] Broadening of bulk eps ', bbroad, E_unit )

  return
end subroutine init_bulk


  subroutine clean_bulkeps
    implicit none
    if(allocated(e1in)) deallocate(e1in)
    if(allocated(e2in)) deallocate(e2in)
    if(allocated(hwin)) deallocate(hwin)
    return
  end subroutine clean_bulkeps

  subroutine scan_bulkeps(errmsg)
    !use com,           ONLY:warning
    !
    ! Read and store bulk file data.
    !
    implicit none
    character(lchlen), intent(out) :: errmsg
    logical                        :: ldum
    real(SP)                       :: dum
    integer                        :: i
    integer, parameter             :: un = 56

    lbulkerr = .false. ! If error occurs, lerr -> .true.
!   Check if already read
    if(allocated(e2in)) then 
      errmsg='Bulk data already associated.'
      return
    endif

!   Test on inputs
    if (index(btype,'KK') > 0 .and. bbroad < 0.000001) then
      errmsg='No broadening specified for KK of bulk eps.'
      lbulkerr = .true.
      return
    endif

!   Test for existence of file
    inquire ( file = bfile, exist = ldum) 
    if(.not.ldum) then
      errmsg='Unable to find file '//trim(bfile)
      lbulkerr = .true.
      return
    endif

!   Open bulk file
    open ( un, file = trim(bfile), err=998)
    nline = 0

!   Read number of lines
    do while(.true.)
      read(un,*,end=99)
      nline = nline + 1
    enddo
99  rewind(un)
    if (nline <= 1) then
      errmsg='Problem determining number of lines in bulk file.'
      return
      lbulkerr = .true.
    endif

!   Allocate internal storage arrays
    allocate( e2in(nline), e1in(nline), hwin(nline) )

!   Read bulk data

    do i = 1,nline
      if(trim(btype).eq."2KK") read(un,*,err=999, end=999) hwin(i),e2in(i)
      if(trim(btype).eq."3KK") read(un,*,err=999, end=999) hwin(i),dum,e2in(i)
      if(trim(btype).eq."3ri") read(un,*,err=999, end=999) hwin(i), e1in(i), e2in(i)
      if(trim(btype).eq."3ir") read(un,*,err=999, end=999) hwin(i), e2in(i), e1in(i)
      hwin(i) = hwin(i)/HA2EV
!     write(73,*) hwin(i)*HARTREE,e2in(i),e1in(i)
    enddo
    close(un)

!   Check the order of the columns
    if(any(e2in.lt.0.0_SP)) then
!      lbulkerr = .true.
       call warning('Bulk epsilon has negative absorption! '//&
&                   'Check the column order/metallicity.')
    endif

!   All ok
    write(errmsg,'(a,i5,a)') 'Read and stored ',nline,&
&     ' records of bulk epsilon data from '//trim(bfile)
    return   

999 continue
    errmsg='Strange problem reading the bulk file.'
    lbulkerr = .true.
    close(un)
    return
998 continue
    errmsg='Problem opening the bulk file for reading.'
    lbulkerr = .true.
    return
  end subroutine scan_bulkeps

  subroutine shift_bulkeps
    implicit none
    integer                  :: i
    do i = 1, nline 
      hwin(i) = hwin(i) + bshift
    enddo
    return
  end subroutine shift_bulkeps

  subroutine GetBulkEps(hw, nw, eps_b, errmsg)
    !
    ! A wrapper routine to read and process the bulk data in one call.
    !
    implicit none
!   Input
    integer, intent(in)      :: nw       ! column
    real(SP), intent(in)     :: hw(:)
!   Output
    complex(SP), intent(out) :: eps_b(:)
    character(lchlen), intent(inout) :: errmsg
!   Work space
    integer                  :: i

    ! Check to see if this has been read before, and failed...
    if(lbulkerr) return
    !
    ! Try to read in the data....if not, return
    !
    call scan_bulkeps(errmsg)
    if(lbulkerr) return
    !
    ! Process the bulk data into the required form
    !
    call generate_bulkeps(hw, nw, eps_b, errmsg)
    if(lbulkerr) return
!   !
!   !  Dump the calculated stuff to file for checking
!   !
    open(unit=81,file = "bulk.out")
    do i = 1, nw
      write(81,102) hw(i)*HA2EV, real(eps_b(i)), AIMAG(eps_b(i))
    enddo
    close(81)
    !
    !   Successful generation of bulk data
    !
    write(errmsg,103) 'Processed ',nw,' data [',&
&   hw(1)*HA2EV,'-',hw(nw)*HA2EV,'eV] from',nline,' records [',&
&   hwin(1)*HA2EV,'-',hwin(nline)*HA2EV,'eV]'
    return

102 format(f8.3,2f12.6)
103 format(a,i4,a,f5.2,a,f5.2,a,i5,a,f5.2,a,f5.2,a)
  end subroutine getbulkeps

  subroutine generate_bulkeps(hw, nw, eps_b, errmsg)
    implicit none
!   Input
    integer, intent(in)      :: nw       ! column
    real(SP), intent(in)     :: hw(:)
!   Output
    complex(SP), intent(out) :: eps_b(:)
    character(lchlen), intent(inout) :: errmsg
!   Work space
    integer                  :: i
    real(SP)                 :: e1(nw), e2(nw) ! temp eps_b
    complex(SP)              :: eps, omega
    logical                  :: lerr2
    !
    ! Rigid shift of the bulk spectrum
    !
    lbulkshift = merge(.true., .false., bshift.gt.0.00001)
    if(lbulkshift) call shift_bulkeps
    !
    ! Transform and regrid
    !
    if( index(btype,"KK") > 0 ) then
      !
      !  Regrid the input e2bulk to e2 in wv
      !
      call regrid( 'imag', hwin, e2in, hw, e2, lerr2)
      if (lerr2) then
        lbulkerr = .true.
        errmsg='Problem doing regrid of imag. bulk eps data.'
        return 
      endif
      !
      !  Do Kramig-Kroners transform to obtain e1
      !
      eps= 0d0
      e2(0) = 0
      e2(nw) = 0
      eps_b(:) = 0
      do i = 1,nw
        omega = real(hw(i))+cI*bbroad
        call epsKK(omega,e2,hw,nw,eps,nw,lerr2)
        if (lerr2) then
          lbulkerr = .true.
          errmsg='Problem doing Kramers Kronig of resonant part.'
          return 
        endif
        eps_b(i)=eps_b(i)+eps
      enddo
      !
      ! Add the missing 1 to real part.
      !
      forall(i=1:nw) eps_b(i) = eps_b(i)+1.0_SP
      !
      !  Real and imaginary supplied, just regrid
      !
    else if( trim(btype) == "3ir" .or. trim(btype) == "3ri" ) then 

      call regrid( 'real', hwin, e1in, hw, e1, lerr2)
      if (lerr2) then
        lbulkerr = .true.
        errmsg='Problem doing regrid of real. bulk eps data.'
        return 
      endif
      call regrid( 'imag', hwin, e2in, hw, e2, lerr2)
      if (lerr2) then
        lbulkerr = .true.
        errmsg='Problem doing regrid of imag. bulk eps data.'
        return 
      endif

      do i = 1, nw
        eps_b(i) = cmplx( e1(i), e2(i), SP)
      enddo
    else
      lbulkerr = .true.
      errmsg='Wrong input option for bulk file format.'
      return
    endif
!   !
!   !  Dump the calculated stuff to file for checking
!   !
!   ! 
!   open(unit=81,file = "bulk.out")
!   do i = 1, nw
!     write(81,102) hw(i)*HA2EV, real(eps_b(i)), AIMAG(eps_b(i))
!   enddo
!   close(81)
    !
    !   Successful generation of bulk data
    !
    write(errmsg,103) 'Processed ',nw,' data [',&
&   hw(1)*HA2EV,'-',hw(nw)*HA2EV,'eV] from',nline,' records [',&
&   hwin(1)*HA2EV,'-',hwin(nline)*HA2EV,'eV]'
    return

102 format(f8.3,2f9.4)
103 format(a,i4,a,f5.2,a,f5.2,a,i5,a,f5.2,a,f5.2,a)
  end subroutine generate_bulkeps

  subroutine epsKK(x,am,xp,n,fc,nmax,lerr)
    implicit none
    logical,     intent(out) :: lerr
    integer,     intent(in)  :: n, nmax
    real(SP),    intent(in)  :: xp(nmax), am(nmax)
    complex(SP), intent(in)  :: x
    complex(SP), intent(out) :: fc
!   Work space
    integer :: m, j

    lerr = .false.

    m = n - 1
    fc = 0d0

    do j = 2,m
       ! Rousseau's method [Spectrochim. Acta A 259, 119849 (2021)]
       fc =fc+am(j)*((((x-xp(j-1))/(xp(j)-xp(j-1))*log((x-xp(j))/(x-xp(j-1))))-((xp(j+1)-x)/(xp(j+1)-xp(j))*&
       &   log((x-xp(j))/(x-xp(j+1)))))-((x+xp(j-1))/(xp(j)-xp(j-1))*log((x+xp(j))/(x+xp(j-1)))+&
       &   (xp(j+1)+x)/(xp(j+1)-xp(j))*log((x+xp(j))/(x+xp(j+1)))))
    enddo

    fc = fc/pi
    return
  end subroutine epsKK

  subroutine regrid(form,xin,fin,xout,fout, lerr)
    !
    !   Simple linear interpolation routine to map a function (epsilon) on an
    !   irregular grid to a different irregular grid.
    !   Outside the known grid the function takes reasonable values...
    !
    implicit none
    character(len=4), intent(in) :: form
    ! form = real
    !        imag
    !        free
    logical,  intent(out) :: lerr
    real(SP), intent(in)  :: fin(:)    ! function known at xin
    real(SP), intent(out) :: fout(:)   ! function required at xout
    real(SP), intent(in)  :: xin(:)  
    real(SP), intent(in)  :: xout(:)  
    integer               :: n_in,n_out, i
    real(SP)              :: ratio
    integer               :: i1,i2

    lerr = .false.

    n_in  = size(fin)
    n_out = size(fout)

    if (n_out.ne.size(xout).or.n_in.ne.size(xin)) then
       lerr = .true.
       return
    endif

    !
    ! Loop over all points x_out(1:n_out)
    !
    do i = 1, n_out
       !
       ! 0 <---hw [Im(eps)=0] hw---> +inf
       !
       if ((form=='imag'.or.form=='free') .and. &
&         (xout(i).lt.xin(1).or.xout(i).gt.xin(n_in))) then
          fout(i) = 0.0_SP
          cycle
       !
       ! 0 <---hw [Re(eps)=Re(eps=0)]
       !
       else if (form=='real'.and. xout(i).lt.xin(1)) then
          fout(i) = fin(1)
          cycle
       !
       ! [Re(eps)=1] hw---> +inf (a tail would be better!)
       !
       else if (form=='real' .and. xout(i).gt.xin(n_in)) then
          fout(i) = 1.0_SP
          cycle
       endif
       !
       ! Identify closest point to xout(i) in array xin(1:n_in)
       !
       i1 = minval( minloc( abs(xin-xout(i)) ) )
       !
       ! Find bracketing two points in xin(1:n_in)
       !
       if ( xin(i1) >  xout(i) ) then  ! swap points
         i2 = i1
         i1 = i1-1
       else if ( xin(i1) <=  xout(i) ) then ! ok
         i2 = i1 + 1
       endif
!      if ( i2 >  n_in ) then
!         i2 = i1
!         i1 = i1-1
!      endif
       if( i2 .gt. n_in ) then
          fout(i) = fin(n_in)
       else if( abs( xin(i2) - xin(i1) ) <= 0.00001 ) then ! buggy
          fout(i) = fin(i1)
       else
          ratio = abs( xout(i) - xin(i1) )/abs( xin(i2) - xin(i1) )
          fout(i) = fin(i1) + ratio*( fin(i2) - fin(i1) )
       endif
    enddo
    return
  end subroutine regrid

end module bulkeps


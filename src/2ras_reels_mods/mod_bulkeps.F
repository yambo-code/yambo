!
! Copyright (C) 2000-2005 C. Hogan  and the SELF team 
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
module bulkeps
  !
  ! Routines and types for reading, storing and processing 
  ! the bulk dielectric function
  !
  use pars,                 ONLY : schlen, lchlen, SP, PI, HARTREE
  save
  private

  real(SP)                      :: bshift, bbroad
  character(schlen)             :: bfile, btype
  logical                       :: lbulkshift
  !
  ! The -original- bulk data is stored here:
  !
  real(SP), allocatable, private:: e1in(:),e2in(:),hwin(:)
  integer, private              :: nline

  public :: init_bulk
  public :: GetBulkEps           ! All in one
  public :: scan_bulkeps, clean_bulkeps
  ! 
  ! For input
  ! 
  public :: bshift, bbroad, bfile, btype

contains

subroutine init_bulk(defs)
  use it_m,                  ONLY : it, initdefs, E_unit,G_unit,T_unit
  implicit none
  integer, parameter             :: V_more=1, V_qp=2, V_io=3, V_debug=4
  type(initdefs), intent(inout)  :: defs

  call it(defs,'BulkFile', '[RAS] File containing bulk', bfile )
  call it(defs,'BulkForm', '[RAS] Format of bulk (`3ri`,`3ir`,`2KK`,`3KK`) ',btype )
  call it(defs,'BlkShift', '[RAS] GW shift of bulk eps ', bshift, &
&         E_unit,verb_level=V_more  )
  call it(defs,'BlkBroad', '[RAS] Broadening of bulk eps ', bbroad, E_unit )

  bshift = bshift/HARTREE
  bbroad = bbroad/HARTREE

  lbulkshift = merge(.true., .false., bshift.gt.0.001)
  return
end subroutine init_bulk


  subroutine clean_bulkeps
    implicit none
    if(allocated(e1in)) deallocate(e1in)
    if(allocated(e2in)) deallocate(e2in)
    if(allocated(hwin)) deallocate(hwin)
    return
  end subroutine clean_bulkeps

  subroutine scan_bulkeps(lerr, errmsg)
    !
    ! Read and store bulk file data.
    !
    implicit none
    logical, intent(out)           :: lerr
    character(lchlen), intent(out) :: errmsg
    logical                        :: ldum
    real(SP)                       :: dum
    integer                        :: i
    integer, parameter             :: un = 56

    lerr = .false. ! If error occurs, lerr -> .true.
!   Check if already read
    if(allocated(e2in)) then 
      errmsg='Bulk data already associated.'
      return
    endif

!   Test on inputs
    if (index(btype,'KK') > 0 .and. bbroad < 0.0001) then
      errmsg='No broadening specified for KK of bulk eps.'
      lerr = .true.
      return
    endif

!   Test for existence of file
    inquire ( file = bfile, exist = ldum) 
    if(.not.ldum) then
      errmsg='Unable to find file '//trim(bfile)
      lerr = .true.
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
      lerr = .true.
    endif

!   Allocate internal storage arrays
    allocate( e2in(nline), e1in(nline), hwin(nline) )

!   Read bulk data

    do i = 1,nline
      if(trim(btype).eq."2KK") read(un,*,err=999, end=999) hwin(i),e2in(i)
      if(trim(btype).eq."3KK") read(un,*,err=999, end=999) hwin(i),dum,e2in(i)
      if(trim(btype).eq."3ri") read(un,*,err=999, end=999) hwin(i), e1in(i), e2in(i)
      if(trim(btype).eq."3ir") read(un,*,err=999, end=999) hwin(i), e2in(i), e1in(i)
      hwin(i) = hwin(i)/HARTREE
      write(73,*) hwin(i)*HARTREE,e2in(i),e1in(i)
    enddo
    close(un)

!   All ok
    write(errmsg,'(a,i5,a)') 'Read and stored ',nline,&
&     ' records of bulk epsilon data from '//trim(bfile)
    return   

999 continue
    errmsg='Strange problem reading the bulk file.'
    lerr = .true.
    close(un)
    return
998 continue
    errmsg='Problem opening the bulk file for reading.'
    lerr = .true.
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

  subroutine GetBulkEps(hw, nw, eps_b, lerr, errmsg)
    implicit none
!   Input
    integer, intent(in)      :: nw       ! column
    real(SP), intent(in)     :: hw(:)
!   Output
    logical, intent(inout)     :: lerr
    complex(SP), intent(out) :: eps_b(:)
    character(lchlen), intent(inout) :: errmsg
!   Work space
    integer                  :: i
    real(SP)                 :: e1(nw), e2(nw) ! temp eps_b
    real(SP)                 :: ahalb, deb, fz
    complex(SP)              :: eps, omega
    logical                  :: lerr2

    ! Check to see if this has been read before, and failed...
    if(lerr) return
    !
    ! Try to read in the data....if not, return
    !
    call scan_bulkeps(lerr, errmsg)
    if(lerr) return
    !
    ! Rigid shift of the bulk spectrum
    !
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
        lerr = .true.
        errmsg='Problem doing regrid of imag. bulk eps data.'
        return 
      endif
      !
      !  Do Kramig-Kroners transform to obtain e1
      !
      fz = 0.0
      deb = hw(2) - hw(1)
      ahalb=hw(1)-deb/2.0_SP
      eps_b(:) = 0
      do i = 1, nw
        omega = CMPLX(hw(i),bbroad) 
        call epsKK(omega,e2,hw,nw,ahalb,fz,eps,nw,lerr2)
        if (lerr2) then
          lerr = .true.
          errmsg='Problem doing Kramers Kronig of resonant part.'
          return 
        endif
        eps_b(i) = eps_b(i) + eps
        omega = CMPLX(-hw(i),-bbroad) 
        call epsKK(omega,e2,hw,nw,ahalb,fz,eps,nw,lerr2)
        if (lerr2) then
          lerr = .true.
          errmsg='Problem doing Kramers Kronig of anti-resonant part.'
          return 
        endif
        eps_b(i) = eps_b(i) + eps
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
        lerr = .true.
        errmsg='Problem doing regrid of real. bulk eps data.'
        return 
      endif
      call regrid( 'imag', hwin, e2in, hw, e2, lerr2)
      if (lerr2) then
        lerr = .true.
        errmsg='Problem doing regrid of imag. bulk eps data.'
        return 
      endif

      do i = 1, nw
        eps_b(i) = cmplx( e1(i), e2(i), SP)
      enddo
    else
      lerr = .true.
      errmsg='Wrong input option for bulk file format.'
      return
    endif
! -->DEBUG
!   !
!   !  Dump the calculated stuff to file for checking
!   !
!   ! 
!   if(lboss) then
    open(unit=81,file = "bulk.out")
    do i = 1, nw
      write(81,102) hw(i)*HARTREE, real(eps_b(i)), AIMAG(eps_b(i))
    enddo
    close(81)
!   endif
! <--DEBUG
    !
    !   Successful generation of bulk data
    !
    write(errmsg,103) 'Processed ',nw,' data [',&
&   hw(1)*HARTREE,'-',hw(nw)*HARTREE,'eV] from',nline,' records [',&
&   hwin(1)*HARTREE,'-',hwin(nline)*HARTREE,'eV]'
    return

102   format(f8.3,2f9.4)
103 format(a,i4,a,f5.2,a,f5.2,a,i5,a,f5.2,a,f5.2,a)
  end subroutine getbulkeps

  subroutine epsKK(x,am,xp,n,a,fz,fc,nmax,lerr)
    implicit none
    integer,     intent(in)  :: n, nmax
    real(SP),    intent(in)  :: a, fz, xp(nmax), am(nmax)
    complex(SP), intent(in)  :: x
    complex(SP), intent(out) :: fc
    logical,     intent(out) :: lerr
!   Work space
    integer :: m, j
    real(SP) :: ev, e
    complex(SP) :: b, c, d, f, h

    lerr = .false.

    m = n - 1
    f = x - a
    ev = xp(1) - a
    if (ABS(ev).LT.1.E-10) then
       lerr = .true.
       return
    endif

    b = log(1.0_SP - ev/f)
    fc = fz * (-1.0_SP + log((xp(1) - x)/(a-x)) * (xp(1) - x)/ev)

    do j = 1, m
       e = xp(j+1) - xp(j)
       d = xp(j+1) - x
       h = x - xp(j)
       c = log(1.0_SP-e/h)
       fc = fc + am(j)*( c*d/e + b*f/ev )
       b = c
       f = h
       ev = e
    enddo
    fc = fc + am(n) * (1.0_SP + b*f/ev)
    fc = fc/pi
    return
  end subroutine epsKK

  subroutine regrid(form,xin,fin,xout,fout,lerr)
    !
    !   Simple linear interpolation routine to map a function (epsilon) on an
    !   irregular grid to a different irregular grid.
    !   Outside the known grid the function takes reasonable values...
    !
    implicit none
    character(len=4), intent(in) :: form
    real(SP), intent(in)  :: fin(:)    ! function known at xin
    real(SP), intent(out) :: fout(:)   ! function required at xout
    real(SP), intent(in)  :: xin(:)  
    real(SP), intent(in)  :: xout(:)  
    logical,  intent(out) :: lerr
    integer               :: n_in,n_out, i
    real(SP)              :: ratio
    integer               :: i1,i2

    lerr = .false.

    n_in  = size(fin)
    n_out = size(fout)

    if (n_out.ne.size(xout)) then
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
       if (form=='imag' .and. (xout(i).lt.xin(1).or.xout(i).gt.xin(n_in))) then
          fout(i) = 0.0_SP
          cycle
       !
       ! 0 <---hw [Re(eps)=Re(eps=0)]
       !
       else if (form=='real' .and. xout(i).lt.xin(1)) then
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
       i2 = i1 + 1
!      if ( xin(i1) >  xout(i) ) then
       if ( i2 >  n_in ) then
          i2 = i1
          i1 = i1-1
       endif
       if( i2 .gt. n_in ) then
          write(*,*) "how can this be?",i,n_out,n_in,i1,i2, xin(i1)
          fout(i) = fin(n_out)
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


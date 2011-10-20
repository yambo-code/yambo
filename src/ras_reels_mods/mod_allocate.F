!
! Copyright (C) 2000-2011 C. Hogan  and the YAMBO team 
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
module allocatable_arrays
  !
  ! Handles allocating and deallocating of arrays, with
  ! error reporting.
  !
  ! Mostly for debugging purposes on large systems.
  ! Note: allocatable arrays as dummy arguments are not
  ! supported in Fortran90 (pgf90, for example), so do not
  ! use in general codes.
  !
  ! Recall
  ! a(10,3) :  rank=2; extents=10,3; shape=(10x3); 
  ! size=30 (sometimes [incorrectly] called extent)
  ! size in memory  = size * (# bytes in type)
  !
  use pars,                   ONLY : schlen, SP
  use com,   ONLY:msg
  implicit none
  integer, parameter              :: k12=selected_int_kind(12)
  integer, parameter              :: it=selected_int_kind(7)
  integer(kind=k12)               :: asize
  integer, private                :: extent(7), rank, ierr
  real(SP)                        :: amem
  real(SP), parameter             :: kb = 1024_SP
  logical, private                         :: lerr, lname
  logical                         :: lreport ! if .true., force reporting
  character(len=schlen), private           :: arrname, lnch1, lnch2, lnch3

  interface my_allocate
    module procedure allocate_i1
    module procedure allocate_r1
    module procedure allocate_r2
    module procedure allocate_c3
  end interface my_allocate

  interface my_deallocate
    module procedure deallocate_c1
    module procedure deallocate_c2
    module procedure deallocate_c3
    module procedure deallocate_r1
    module procedure deallocate_r2
    module procedure deallocate_r3
    module procedure deallocate_i1
    module procedure deallocate_i2
  end interface my_deallocate

contains

  subroutine allocate_i1(a,ndim1,lrep,aname)
  implicit none
    integer, allocatable           :: a(:)
    integer, intent(in)            :: ndim1
    character(*), optional         :: aname
    logical,      optional         :: lrep
    
    rank = 1
    extent(1) = ndim1
    asize  = ndim1
    amem   = real(ndim1)/kb/kb*it

    lreport = .false. ; if(present(lrep)) lreport = lrep

    lname = .false. ; if(present(aname)) lname = .true.
    if(lname) arrname = trim(aname)

    allocate(a(ndim1),stat=ierr) 
    if(ierr.gt.0) call allocate_msg
    if(lreport.and.ierr.eq.0) call allocate_msg
    
    return
  end subroutine allocate_i1
  subroutine allocate_r1(a,ndim1,lrep,aname)
  implicit none
    real(SP), allocatable       :: a(:)
    integer, intent(in)            :: ndim1
    character(*), optional         :: aname
    logical,      optional         :: lrep
    
    rank = 1
    extent(1:1) = (/ ndim1 /)
    asize  = ndim1
    amem   = real(ndim1)/kb/kb*SP

    lreport = .false. ; if(present(lrep)) lreport = lrep

    lname = .false. ; if(present(aname)) lname = .true.
    if(lname) arrname = trim(aname)

    allocate(a(ndim1),stat=ierr) 
    if(ierr.gt.0) call allocate_msg
    if(lreport.and.ierr.eq.0) call allocate_msg
    
    return
  end subroutine allocate_r1


  subroutine allocate_r2(a,ndim1,ndim2,lrep,aname)
  implicit none
    real(SP), allocatable          :: a(:,:)
    integer, intent(in)            :: ndim1,ndim2
    character(*), optional         :: aname
    logical,      optional         :: lrep
    
    rank = 2
    extent(1:2) = (/ ndim1, ndim2 /)
    asize  = ndim1*ndim2
    amem   = real(ndim1)*real(ndim2)/kb/kb*SP

    lreport = .false. ; if(present(lrep)) lreport = lrep

    lname = .false. ; if(present(aname)) lname = .true.
    if(lname) arrname = trim(aname)

    allocate(a(ndim1,ndim2),stat=ierr) 
    if(ierr.gt.0) call allocate_msg
    if(lreport.and.ierr.eq.0) call allocate_msg
    
    return
  end subroutine allocate_r2


  subroutine allocate_c3(a,ndim1,ndim2,ndim3,lrep,aname)
  implicit none
    complex(SP), allocatable       :: a(:,:,:)
    integer, intent(in)            :: ndim1,ndim2,ndim3
    character(*), optional         :: aname
    logical,      optional         :: lrep
    
    rank = 3
    extent(1:3) = (/ ndim1, ndim2, ndim3 /)
    asize  = ndim1*ndim2*ndim3 
    amem   = real(ndim1)*real(ndim2)*real(ndim3)/kb/kb*SP*2

    lreport = .false. ; if(present(lrep)) lreport = lrep

    lname = .false. ; if(present(aname)) lname = .true.
    if(lname) arrname = trim(aname)

    allocate(a(ndim1,ndim2,ndim3),stat=ierr) 
    if(ierr.gt.0) call allocate_msg
    if(lreport.and.ierr.eq.0) call allocate_msg
    
    return
  end subroutine allocate_c3

  subroutine deallocate_c2(a,aname)
  implicit none
    complex(SP), allocatable       :: a(:,:)
    character(*), optional         :: aname

    lname = .false.
    if(present(aname)) arrname = trim(aname)

    deallocate(a,stat=ierr) 
    if(ierr.gt.0) call deallocate_msg
    return
  end subroutine deallocate_c2

  subroutine deallocate_c1(a,aname)
  implicit none
    complex(SP), allocatable       :: a(:)
    character(*), optional         :: aname

    lname = .false.
    if(present(aname)) arrname = trim(aname)

    deallocate(a,stat=ierr) 
    if(ierr.gt.0) call deallocate_msg
    return
  end subroutine deallocate_c1
  subroutine deallocate_c3(a,aname)
  implicit none
    complex(SP), allocatable       :: a(:,:,:)
    character(*), optional         :: aname

    lname = .false.
    if(present(aname)) arrname = trim(aname)

    deallocate(a,stat=ierr) 
    if(ierr.gt.0) call deallocate_msg
    return
  end subroutine deallocate_c3

  subroutine deallocate_r2(a,aname)
  implicit none
    real(SP), allocatable       :: a(:,:)
    character(*), optional         :: aname

    lname = .false.
    if(present(aname)) arrname = trim(aname)

    deallocate(a,stat=ierr) 
    if(ierr.gt.0) call deallocate_msg
    return
  end subroutine deallocate_r2

  subroutine deallocate_r1(a,aname)
  implicit none
    real(SP), allocatable       :: a(:)
    character(*), optional         :: aname

    lname = .false.
    if(present(aname)) arrname = trim(aname)

    deallocate(a,stat=ierr) 
    if(ierr.gt.0) call deallocate_msg
    return
  end subroutine deallocate_r1
  subroutine deallocate_r3(a,aname)
  implicit none
    real(SP), allocatable       :: a(:,:,:)
    character(*), optional         :: aname

    lname = .false.
    if(present(aname)) arrname = trim(aname)

    deallocate(a,stat=ierr) 
    if(ierr.gt.0) call deallocate_msg
    return
  end subroutine deallocate_r3

  subroutine deallocate_i1(a,aname)
  implicit none
    integer, allocatable       :: a(:)
    character(*), optional         :: aname

    lname = .false.
    if(present(aname)) arrname = trim(aname)

    deallocate(a,stat=ierr) 
    if(ierr.gt.0) call deallocate_msg
    return
  end subroutine deallocate_i1
  subroutine deallocate_i2(a,aname)
  implicit none
    integer, allocatable       :: a(:,:)
    character(*), optional         :: aname

    lname = .false.
    if(present(aname)) arrname = trim(aname)

    deallocate(a,stat=ierr) 
    if(ierr.gt.0) call deallocate_msg
    return
  end subroutine deallocate_i2

  subroutine deallocate_msg
  implicit none
    write(lnch3,98) 
    call msg('nrs',lnch3)
    write(lnch3,100) 
    call msg('nrs',lnch3)
    if(lname) call msg('nrs','Filename: '//trim(arrname))
    return
  end subroutine deallocate_msg

  subroutine allocate_msg
    implicit none
    integer          :: exmax, lenmax
    character(schlen)    :: form

    write(lnch3,98) 
    call msg('nrs',lnch3)

    write(lnch1,100)
    write(lnch2,101)
    if(ierr.gt.0) call msg('nrs',trim(lnch1))
    if(ierr.eq.0) call msg('nrs',trim(lnch2))

    write(lnch1,102) trim(arrname)
    if(lname) call msg('nrs',trim(lnch1))
    write(lnch1,103) amem
    call msg('nrs',trim(lnch1))
    write(lnch1,104) asize
    call msg('nrs',trim(lnch1))

    exmax = maxval(extent(1:rank))
    lenmax = nint(log10(real(exmax)+0.001))+1

    if(rank.eq.1) then
      write(form,'(1x,a,i1,a,i1,a)') '(1x,a11,',rank-1,'(i',lenmax,'),a)'
      write(lnch2,form) "Shape  : ( ",extent(1)," )"
    else
      write(form,'(a,i1,a,i1,a,i1,a)') &
&              '(1x,a11,',rank-1,'(i',lenmax,',"x"),i',lenmax,',a)'
      write(lnch2,form) "Shape  : ( ",extent(1:rank-1),extent(rank)," )"
    endif
    call msg('nrs',trim(lnch2))

    call msg('nrs',lnch3)
    return
98  format(1x,30("-"))
100 format(1x,"Array allocation failed. Exiting.")
101 format(1x,"Array allocation successful.")
102 format(1x,"Name   :   ",a)
103 format(1x,"Memory :   ",5x,f7.2," Mb.")
104 format(1x,"Size   :   ",i12)
105 format(1x,"Shape  : ( ",3(i2,"x"),")")
  end subroutine allocate_msg

end module allocatable_arrays

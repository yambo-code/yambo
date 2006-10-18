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
module eels_wgrid
  !
  ! Routines for the q-dependent energy grids
  !
  use com,                   only : msg
  use pars,                   only : schlen, SP, PI, HARTREE
  use frequency,              only : w_samp
  use eels_qpt,               only : iqmin, iqmax, nqpar
  implicit none
  real(SP), allocatable           :: hwqgrid(:,:) ! n(2) ?
  real(SP), allocatable           :: hwgrid(:) 
  integer                         :: nwtot, nw, nwp
  integer, allocatable            :: hw_range(:,:)

contains

  subroutine init_wgrid(wq)
    use com,                   only : msg
    implicit none
    type(w_samp), intent(inout)    :: wq
    character(schlen)              :: lch1

!   Odd number of points always!
    if(mod(wq%n(1),2).eq.0) then

      call msg('nrs','Warning: even number of energy points not allowed in REELS.')
      write(lch1,'(a,i5)') 'Resetting wq%n(1) to',wq%n(1)+1
      call msg('nrs',trim(lch1))

      wq%n(1) = wq%n(1)+1
      deallocate(wq%p)
      allocate(wq%p(wq%n(1)))
    endif

!   For now, make sure n(2) = n(1)
    if(wq%n(2).ne.wq%n(1)) then
      wq%n(2) = wq%n(1)
      call msg('nrs','Warning: EnStps total|memory must be the same.')
    endif
      
    allocate(hwqgrid(wq%n(1),nqpar))
    allocate(hw_range(nqpar,2))

    return 
  end subroutine init_wgrid

  subroutine wgrid
    !
    ! Set up full contiguous grid (no degeneracy)
    !
    use eels_qpt, only : nqeels
    use reels_module,          only : lepsmix
    implicit none
    integer :: ist, iw, iq, i1
    real(SP), allocatable :: tempw(:)
    character(schlen)              :: lch1

!   For both cases of lepsmix, take only the intermediate points.
!   since only one spectra should be returned in loss 
!   if no mixing, reels_sec will contain the bare spectra
    
    ist = (nw+1)/2
    allocate(tempw(nqeels*nw))

!  iw is a counter to track through hwqgrid...
   iw = 1
   if(.not.lepsmix) then
     iw = 1
     do i1 = 1,ist-1
       tempw(iw) = hwqgrid(i1,iqmin) 
       iw = iw + 1
     enddo
   endif

!  hw_range refers to points on the full continuous grid, and is just used
!  for filling loss(:,nwtot)
      do iq = iqmin, iqmax
        hw_range(iq,1) = iw
        do i1 = ist, nw
          tempw(iw) = hwqgrid(i1,iq) 
          iw = iw + 1
        enddo
        hw_range(iq,2) = iw-1
      enddo
      nwtot = iw-1

    if(.not.lepsmix) hw_range(iqmin,1) = 1

    allocate(hwgrid(nwtot)) ! this shouldnt depend on ipol in fact...
    hwgrid(:) = tempw(1:nwtot)
    deallocate(tempw) 



    return
  end subroutine wgrid

  subroutine wqgrid(wq)
    use eels_qpt
    implicit none
    type(w_samp), intent(in)       :: wq
    real(SP)                       :: wmin, wmax, dw1, dw2
    integer                        :: ist, iw, iq, nw
    real(SP), parameter            :: weps = 0.0001
    logical                        :: ldebug

!   Odd number of points always!
    nw = wq%n(1)
    wmin = wq%er(1)
    wmax = wq%er(2)
    ldebug = .false.

!
!   Special case: nqpar = 1, one q-point (q*)
!
    if(ldebug) then
    write(*,*) "Make wgrid: nw = ",nw," nqpar = ",nqpar
    write(*,*) "Make wgrid: wmin = ",wmin*HARTREE," wmax = ",wmax*HARTREE
    endif

!nqpar, iqmin

    if(nqpar == 1) then
      if(qGwx(iqmin).gt.wmax.or.qGwx(iqmin).lt.wmin) then 
!
!       If q* is outside required energy range, divide it evenly
!          O       
!             |                  |
!           wmin-o-o-o-o-o-o-o-wmax
!
        dw1 = (wmax - wmin)/(nw-1) ! ist - 1 intervals
        do iw=1,nw
          hwqgrid(iw,iqmin) = wmin+(iw-1)*dw1
        enddo
      else
!
!       Find best partitioning of points to include the exact q*
!                    O       
!             |      |           |
!           wmin-o-o-O-o-o-o-o-wmax
!
        ist = nint((qGwx(iqmin) - wmin)/(wmax-wmin)*nw)
        dw1 = (qGwx(iqmin) - wmin)/(ist-1) ! ist - 1 intervals
        dw2 = (wmax - qGwx(iqmin))/(nw-ist) ! nwp-ist intervals : nwp - ist + ist - 1 = nwp - 1 intervals
        do iw=1,ist
          hwqgrid(iw,iqmin) = wmin+(iw-1)*dw1
        enddo
        do iw=ist+1,nw
          hwqgrid(iw,iqmin) = qGwx(1)+(iw-ist)*dw2
        enddo
      endif
      return
    endif

!
!   All other grids (nw = 7, ist = 4) [ist = exact point]
!
    ist = (nw+1)/2

! Have to decide if O below wmin will be used or not!
! Note: There will always be a F > wmax.
! Note: There may not be a I < wmax (unless use negative?).

!                                    iqmax
!                                      |
!                                f f f F f f f hwqgrid(1:7,iqmax)
!   iqmin                h h h H h h h  
!      |         x x x X x x x      |   
!      | o o o O o o o              |   
!i i i I i i i                      |   hwqgrid(1:7,iqmin)
!         |                         |  
!       wmin----------------------wmax

!   Intermediate points I...F (the double grids)

!   do iq = 1,nqpar-1
    do iq = iqmin, iqmax-1
      dw1 = (qGwx(iq+1) - qGwx(iq))/ist
      do iw = 1,ist-1
        hwqgrid(ist+iw,iq) = qGwx(iq) + iw*dw1  ! interval [   :...)
        hwqgrid(iw,iq+1)   = qGwx(iq) + iw*dw1  ! interval (...:   ]
        if(ldebug) write(*,*) "hwqgridA",ist+iw,iq,hwqgrid(ist+iw,iq)*HARTREE
        if(ldebug) write(*,*) "hwqgridB",iw,iq+1,hwqgrid(ist+iw,iq)*HARTREE
      enddo
    enddo

!   Exact energies (I, O, X, H, F)

!   do iq = 1,nqpar
    do iq = iqmin, iqmax
      hwqgrid(ist,iq) =  qGwx(iq)
      if(ldebug) write(*,*) "hwqgridC",ist,iq,hwqgrid(ist,iq)*HARTREE
    enddo

!   Points at lower end. Two cases:
!                o o o O o o o              |   hwqgrid(1,1:7)
!        i i i I i i i
!         |                         |  
!        wmin----------------------wmax
!   Generate points between wmin and I

!   if(wmin.lt.qGwx(iqmin).or.abs(wmin-qGwx(iqmin)).lt.weps) then
    if(wmin.lt.qGwx(iqmin)) then
      dw1 = (qGwx(iqmin) - wmin)/(ist-1) ! ist-1 intervals
      do iw = 1,ist-1
        hwqgrid(iw,iqmin) = wmin + (iw-1)*dw1
        if(ldebug) write(*,*) "hwqgridD",iw,iqmin,hwqgrid(iw,iqmin)*HARTREE
      enddo
    else if(wmin.ge.qGwx(iqmin)) then
!   Points at lower end below O
!                o o o O o o o              |   hwqgrid(1,1:7)
!        i i i I i i i
!                |                         |  
!              wmin----------------------wmax
!   Generate points below I, but not below zero.
      dw1 = ( qGwx(iqmin+1) - qGwx(iqmin) )/ist ! ist intervals
      dw2 = ( qGwx(iqmin)   - 0.0_SP      )/ist ! ist intervals
      dw1 = abs(min(dw1,dw2)) ! force to be positive
      do iw = 1, ist-1
!       hwqgrid(ist-iw,iqmin) = qGwx(iqmin) - (iw)*dw1
        hwqgrid(iw,iqmin) = qGwx(iqmin) - (ist-iw)*dw1
        if(ldebug) write(*,*) "hwqgridE",iw,iqmin,hwqgrid(iw,iqmin)*HARTREE,dw1
      enddo
    endif
 
!   Points at higher end above F (are these needed? well, they are free...)
!                      f f f F f f f    hwqgrid(3,4:7)
!        |                 |  
!       wmin--------------wmax

    dw1 = (qGwx(iqmax) - qGwx(iqmax-1))/ist 
    do iw = 1,ist-1
      hwqgrid(ist+iw, iqmax) = qGwx(iqmax) + iw*dw1  ! interval [ : )
      if(ldebug) write(*,*) "hwqgridF",ist+iw,iqmax,hwqgrid(ist+iw,iqmax)
    enddo

    return
  end subroutine wqgrid

  subroutine add_broad(wv)
    implicit none
    type(w_samp), intent(inout) :: wv
    integer :: i1
    real(SP), external :: damping

    do i1=1,wv%n(1)
      wv%p(i1)=real(wv%p(i1)) + &
&              damping(wv%er,wv%dr,real(wv%p(i1)))*cmplx(0.,1.,SP)
    enddo
    return
  end subroutine add_broad

  subroutine select_wgrid(wq, iq)
    implicit none
    type(w_samp), intent(out) :: wq
    integer, intent(in) :: iq
    complex(SP)         :: ctemp(size(real(wq%p(:))))
!   integer             :: i1, iw

    ctemp(:) = cmplx(hwqgrid(:,iq),aimag(wq%p(:)) )
    wq%p(:) = ctemp(:)

  end subroutine select_wgrid
  
  subroutine print_wgrid(wq)
    use eels_qpt
    use com,                only:msg
    implicit none
    character(schlen)              :: lch
!   integer, intent(in) :: nw
    type(w_samp), intent(in) :: wq
    integer :: iq, iw
    !
    ! Report some of the grid parameters
    !
    call msg('rs','Energy points per q [full/step]: ',wq%n)
    call msg('r','Full energy range (eV): ',wq%er*HARTREE)

    write(lch,'(a5,a8,a9,a10)') "iq","nwmin","nwmax","nwp"
    call msg('r',trim(lch))
    write(lch,'(3x,i10,i9,i10)') 1,nw,nwp
    call msg('r',trim(lch))

    do iq = iqmin, iqmax
      write(lch,'(i3, 3(a,f8.3))') iq," [",hwqgrid(1,iq)*HARTREE,":",&
&            hwqgrid(nw,iq)*HARTREE," ]",hwqgrid(nwp,iq)*HARTREE
      call msg('r',trim(lch))
    enddo

    call msg('nr','Mapping to contiguous grid in loss:')
    do iq = iqmin, iqmax
      write(lch,'(a,i3,a,i4,a,i4,a)') &
&     'Range: iq = ',iq,'--> [',hw_range(iq,1),' -',hw_range(iq,2),']'
      call msg('r',trim(lch))
    enddo
    write(lch,'(a,i4,a)') 'Contiguous range : [1 -',nwtot,']'
    call msg('nr',trim(lch))
    !
    ! Write the energy grid to file for testing USE MSG
    !
    open(unit=80,file="wgrid.dat")
    write(80,120) "# hw","qGwx","iw","iq"
    do iq = iqmin, iqmax
      do iw = 1, nw
!        write(80,121) iw,hwqgrid(iw,iq)*HARTREE,iq-iqmin+1,qGwx(iq)
!        write(80,121) iw,hwqgrid(iw,iq)*HARTREE,iq,qGwx(iq)
         write(80,121) hwqgrid(iw,iq)*HARTREE,qGwx(iq),iw,iq
      enddo
         write(80,*) 
         write(80,*) 
    enddo
    close(80)
120 format(a4, a8,  a3,a9)
121 format(f8.3,f9.4,i4,i3)

  end subroutine print_wgrid

end module eels_wgrid

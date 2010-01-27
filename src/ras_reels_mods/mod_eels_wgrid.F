!
! Copyright (C) 2000-2010 C. Hogan  and the YAMBO team 
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
module eels_wgrid
  !
  ! Routines for the q-dependent energy grids
  !
  use com,                    only : msg
  use pars,                   only : schlen, SP, PI
  use units,                  only : HARTREE
  use frequency,              only : w_samp
  use eels_qpt,               only : iqmin, iqmax, nqpar
  implicit none
  real(SP), allocatable           :: hwqgrid(:,:) ! n(2) ?
  !
  ! hwgrid( nw , nqpar ) : For iq in [1:nqpar] and iw in [1:nw] (epsm1)
  !                        hwqgrid(iw,iq) gives the energy in a.u.
  !                        Independent of lepsmix and nwp
  !
  integer, allocatable            :: wqtab(:,:) ! n(2) ?
  !
  ! wqtab ( nw    , nqpar ) : For iq in [1:nqpar] and iw in [1:nwsec]
  !                        wqtab(iw,iq) gives the index of iw in [1:nwtot] array
  !                        (used to find prefactor for (iw,iq) in each loss_sec)
  !
  real(SP), allocatable           :: hwgrid(:) 
  !
  ! hwgrid ( nwtot )     : Energies on the nwtot continuous grid
  !
  integer                         :: nwtot, nw, nwp, nwsec
  !
  ! nw    = number of points epsm1 is calculated on (i.e., wq%n(1) from input)
  ! nwp   = magic point (nw+1)/2 : at which loss is calculated exactly
  ! nwsec = number of points lossq/lossqw is calculated at for each q (section):
  !         if lepsmix=false: nwsec = nw
  !         if lepsmix=true : nwsec = nwp     (iqmin <= iq < iqmax),
  !                                   nw      (         iq = iqmax).
  ! nwtot = total number of (unique) points in spectrum = nqeels*nwp + (nwp-1)
  ! nwfull = number of points in interpolated spectrum 
  !
  integer, allocatable            :: wr(:,:)
  !
  !  wr (nqpar, 3) : For iq in [1:nqpar], wr(iq,1 or 2) gives the upper
  !                        and lower index of iw in [1:nwtot] array for this iq block
  !                        when filling loss(:,nwtot) array
  !                   wr(iq,3) gives the size wr(iq,2)-wr(iq,1)+1

contains

  subroutine init_wgrid(wq)
    !
    !
    !
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
      
    allocate( hwqgrid ( wq%n(1), nqpar ))
    allocate( wr( nqpar , 3 ))

    return 
  end subroutine init_wgrid

  subroutine wgrid
    !
    ! Set up full contiguous grid (no degeneracy)
    !
    use eels_qpt, only : nqeels
    use reels_module,          only : lepsmix
    implicit none
    integer :: iw, iq, i1, iw1
    real(SP), allocatable :: tempw(:)
    character(schlen)              :: lch1

    allocate(tempw(nqeels*nw))
    !
    ! Define the nwtot grid (common for lepsmix T/F)
    !
    forall(iw=1:nwp-1) tempw(iw) = hwqgrid(iw,iqmin)
    iw = nwp
    do iq = iqmin,iqmax
       do i1 = nwp, nw
         tempw(iw) = hwqgrid(i1,iq)
         iw = iw + 1
       enddo
    enddo
    nwtot = iw-1 
    !
    ! Define the wqtab and wr arrays
    !
    allocate( wqtab   ( nw, nqpar ))

    if(lepsmix) then
      iw = 1
      do iq = iqmin,iqmax
        wr(iq,1) = iw
        nwsec = get_nwsec(iq)
        do i1 = 1,nwsec  ! need a second counter as nwsec is nonuniform
           wqtab(i1,iq) = iw
           iw = iw + 1
        enddo
        wr(iq,2) = iw - 1
      enddo
    else
      nwsec = get_nwsec(1)
      forall(iq=iqmin:iqmax,iw=1:nwsec) wqtab(iw,iq) = (iq-iqmin)*nwp + iw
      do iq = iqmin,iqmax
        wr(iq,1) = (iq-iqmin)*nwp + nwp
        wr(iq,2) = (iq-iqmin)*nwp + nw
      enddo
      wr(iqmin,1) = 1
    endif
    forall(iq=iqmin:iqmax) wr(iq,3) = wr(iq,2) - wr(iq,1) + 1
    !
    ! Reset the hwgrid (doesnt have to be done here now, nwtot is known before)
    !
    allocate(hwgrid(nwtot)) ! this shouldnt depend on ipol in fact...check!
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
      !
      !   Points at lower end below O
      !                o o o O o o o              |   hwqgrid(1,1:7)
      !        i i i I i i i
      !                |                         |  
      !              wmin----------------------wmax
      !   Generate points below I, but not below zero.
      !
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
    real(SP), external :: FREQUENCIES_damping

    do i1=1,wv%n(1)
      wv%p(i1)=real(wv%p(i1)) + &
&              FREQUENCIES_damping(wv,real(wv%p(i1)))*cmplx(0.,1.,SP)
    enddo
    return
  end subroutine add_broad

  subroutine select_wgrid(wq, iq)
    implicit none
    type(w_samp), intent(inout) :: wq
    integer,       intent(in)   :: iq
    complex(SP)                 :: ctemp(size(real(wq%p(:))))

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

    call msg('nr','Large overlapping grid:')
    write(lch,'(a5,a8,a9,a10)') "iq","nwmin","nwmax","nwp"
    call msg('r',trim(lch))
    write(lch,'(3x,i10,i9,i10)') 1,nw,nwp
    call msg('r',trim(lch))

    do iq = iqmin, iqmax
      write(lch,'(i3, 3(a,f8.3))') iq," [",hwqgrid(1,iq)*HARTREE,":",&
&            hwqgrid(nw,iq)*HARTREE," ]",hwqgrid(nwp,iq)*HARTREE
      call msg('r',trim(lch))
    enddo

    write(lch,'(a,i4,a)') 'Contiguous range : [1 -',nwtot,']'
    call msg('nr',trim(lch))

    call msg('nr','Mapping to contiguous grid for loss:')
    do iq = iqmin, iqmax
      write(lch,'(a,i3,a,i4,a,i4,a,i4)') &
&     'Range: iq = ',iq,'--> [',wr(iq,1),' -',wr(iq,2),'] size:',wr(iq,3)
      call msg('r',trim(lch))
    enddo

    call msg('nr','Mapping to sectional grids for loss:')
    do iq = iqmin, iqmax
      nwsec = get_nwsec(iq)
      write(lch,'(a,i3,a,i4,a,i4,a,i4)') &
&     'Range: iq = ',iq,'--> [',wqtab(1,iq),' -',wqtab(nwsec,iq),'] size:',nwsec
      call msg('r',trim(lch))
    enddo
    !
    ! Write the energy grid to file for testing USE MSG
    !
    open(unit=80,file="wgrid.dat")
    write(80,120) "# hw","qGwx","iw","iq","wqtab"
    do iq = iqmin, iqmax
      nwsec = get_nwsec(iq)
      do iw = 1, nwsec
         write(80,121) hwqgrid(iw,iq)*HARTREE,qGwx(iq),iw,iq,wqtab(iw,iq)
      enddo
         write(80,*) 
         write(80,*) 
    enddo
    close(80)
120 format(a8,  a9,  a3,a3,a6)
121 format(f8.3,f9.4,i4,i3,i6)

  end subroutine print_wgrid
  
    integer function get_nwsec(iq)
    use reels_module,          only : lepsmix
    integer, intent(in)  :: iq
    if(lepsmix) then
       if(iq.ge.iqmin) get_nwsec = nwp
       if(iq.eq.iqmax) get_nwsec = nw   ! overwrite for iqmax
    else
      get_nwsec = nw
    endif
    return
  end function get_nwsec


end module eels_wgrid

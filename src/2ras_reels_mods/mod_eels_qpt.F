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
module eels_qpt
  ! 
  ! q-point manipulations for HREELS
  !
  use pars,             ONLY : lchlen, SP, HARTREE
  use com,              ONLY : msg
  use R_lattice,        ONLY : bz_samp
  use surface_geometry
  use eels_kinematics
  use intchar

  real(SP), private         :: q1(3), qa(3), qs(3)
  integer, allocatable      :: iqpar(:)
  real(SP), allocatable     :: qpar(:,:), qGwx(:)
  integer                   :: nqpar, iqmin, iqmax, nqeels
  save
  private
 
  public :: nqpar, iqmin, iqmax, nqeels, qGwx, iqpar, print_qpar, qpar
  public :: qpar_1BZ, qpar_sort, qpar_hw

contains

  subroutine print_qpar( iq, iqf, iqi, qpar, qptf, qpti )
    !
    ! Debugging
    !
    implicit none
    integer, intent(in)            :: iq, iqf, iqi
    real(SP), intent(in)           :: qpar(3),qptf(3),qpti(3)
    integer                        :: od(3)
    character(lchlen)              :: lch1

    od(1:3) = (/ iq, iqf, iqi /)
    write(lch1,'(3f10.5)') qptf(1:3)
    call msg('nrs','Selecting Q_||: '//trim(lch1))
    call msg('r','qpar(iq)/q(FBZ)/q(IBZ) indices:',od(1:3))
    call msg('rs','qpar   : ',qpar(1:3))
    call msg('r','q(FBZ) : ',qptf(1:3))
    call msg('rs','q(IBZ) : ',qpti(1:3))
    return
  end subroutine print_qpar

  subroutine qpar_hw( q0, q,  wv ) 
    !
    ! Generate list of qpar ordered wrt qGwx
    !
    use frequency,           ONLY : w_samp
    use vec_operate,         ONLY : v_module
    implicit none
    real(SP),         intent(in) :: q0(3)
    type(bz_samp),    intent(in) :: q
    type(w_samp),     intent(in) :: wv     ! Energy ranges
!   Local
    integer                      :: i1,i, iq
    character(10)                :: titles(6)
    character(lchlen)            :: lch1,lch2
    real(SP)                     :: od(6)
    real(SP)                     :: tempin(nqpar),tempout(nqpar)
    real(SP)                     :: qmag, hwq_, wmin, wmax
    integer                      :: indx(nqpar),indx2(nqpar)

    allocate(qGwx(nqpar))

    wmin = wv%er(1)
    wmax = wv%er(2)
    iq=0
    iqmin = 1
    iqmax = nqpar
    do i1 = 1,nqpar
      q1(:) = qpar(i1,:)
      qmag = sign(v_module(q1),dot_product(q1,q0))
      hwq_ = hwq(qmag)  ! Calculates the energy corresponding to this |Q| in specular geometry
      qGwx(i1) = hwq_
      if(hwq_ <= wmin) iqmin = i1
      if(hwq_ <= wmax) iqmax = min(i1+1,nqpar)
    enddo

    write(lch1,101) "iq ","iqpar","qpar","qpar","qpar","qGwx(eV)"
    write(lch2,'(5x,20("-"))') 
    call msg('nr',trim(lch1))
    do i1=1,nqpar
      if(i1.eq.iqmin) call msg('r',trim(lch2),"-> iqmin <-----")
      q1(:) = qpar(i1,:)
      write(lch1,100) i1,iqpar(i1),q1(:),qGwx(i1)*HARTREE
      call msg('r',trim(lch1))
      if(i1.eq.iqmax) call msg('r',trim(lch2),"-> iqmax <-----")
    enddo
100 format(i5,i5,3f8.3,f10.3)
101 format(a5,a5,3a8,a10)

    nqeels = iqmax - iqmin + 1

    return
  end subroutine qpar_hw

  subroutine qpar_sort( q0, q )
    use vec_operate,         ONLY : sort
    implicit none
    real(SP),      intent(in)    :: q0(3)
    type(bz_samp), intent(in)    :: q
    integer                      :: i1,i
    real(SP)                     :: tempin(nqpar),tempout(nqpar)
    integer                      :: indx(nqpar),indx2(nqpar)
!   real(SP)                     :: v_module

    !
    ! Sort Qpar array in order of increasing qGwx energy
    !
    do i1 = 1,nqpar
      q1(:) = q%ptbz(iqpar(i1),:) 
      !
      ! Return the magnitude of |q1| times the sign of q1.q0
      !
      tempin(i1) = sign(sqrt(dot_product(q1,q1)),dot_product(q1,q0))
    enddo
    call sort(tempin,tempout,indx)
    indx2(:) = iqpar(1:nqpar)
    iqpar(1:nqpar) = indx2(indx(:))
    !
    ! Now define this sorted qpar array
    !
    allocate(qpar(nqpar,3)) 

!<-- DEBUG
!   write(*,99) "iq ","iqpar","qpar"
!--> DEBUG
    do i1 = 1,nqpar
      qpar(i1,:) = q%ptbz(iqpar(i1),:) 
!<-- DEBUG
!     write(*,100) i1,iqpar(i1),qpar(i1,:)
!--> DEBUG
    enddo
99 format(a5,a5,3a6)
100 format(i5,i5,3f6.3)

    return
  end subroutine qpar_sort

  subroutine qpar_1BZ( q0, q )
    implicit none
    real(SP),      intent(in)    :: q0(3)
    type(bz_samp), intent(in)    :: q
    integer                      :: i1,i
    integer                      :: itemp(q%nbz), iqi, isi
    character(lchlen)            :: lch1,lch2

!-->DEBUG ! this information is written already in the report file
!   do i1=1,q%nibz
!     write (lch2,200)   'Q [',i1,'] :',q%pt(i1,:),' (iku) * weight ',q%weights(i1)
!     call msg('r',trim(lch2))
!   enddo
!<--DEBUG
    !
    ! Generate the q points in the full BZ -> q%ptbz
    !
    call k_ibz2bz(q,'i',.false.)  ! i,a,c Shift to the BZ?   
    !
    ! Write the list to the report file (reduce this)
    !
    call msg('r','Q points in the full BZ : '//intc(q%nbz))
    do i1=1,q%nbz
      iqi = q%sstar(i1,1)
      isi = q%sstar(i1,2)
      write (lch2,201)   'Q [',i1,'] :',q%ptbz(i1,:),' (iku) ',iqi,isi
      call msg('r',trim(lch2))
    enddo
    !
    ! Identify points parallel to Q_||
    !
    nqpar = 0
    do i1 = 1,q%nbz
      if(collin(q%ptbz(i1,:),q0(:), 0.0001)) then
        nqpar = nqpar + 1
        itemp(nqpar) = i1
      endif
    enddo
    !
    ! Store nqpar points in iqpar: relates qpar back to q%ptbz array
    !
    call msg('nr','Total Q_|| points found: ',nqpar)
    allocate(iqpar(nqpar)) 
    iqpar(1:nqpar) = itemp(1:nqpar)

    return
200 format(a,i5,a,3f12.5,a,f10.5)
201 format(a,i5,a,3f12.5,a,2i5)
  end subroutine qpar_1BZ

  logical function collin(q,q_orig,comp)
    use pars,                ONLY : SP
    real(SP) q(3),q_orig(3),norm1,norm2,cost,comp

    if(q(1).eq.0.and.q(2).eq.0.and.q(3).eq.0) then
    collin = .true.
    return
 endif
 if(q_orig(1).eq.0.and.q_orig(2).eq.0.and.q_orig(3).eq.0) then
    stop
 endif
 norm1 = sqrt(dot_product(q,q))
 norm2 = sqrt(dot_product(q_orig,q_orig))
 cost = dot_product(q,q_orig)/norm1/norm2
 if(abs(abs(cost) - 1.d0).lt.comp) then
    collin = .true.
 else
    collin = .false.
 endif
 return
 end function collin

end module eels_qpt

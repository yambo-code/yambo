!
! Copyright (C) 2000-2009 C. Hogan  and the YAMBO team 
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
module eels_gvec
  ! 
  ! G vector manipulation for HREELS code
  !
  use pars,             ONLY : lchlen, SP
  use units,            ONLY : HARTREE
  use com,              ONLY : msg
  use surface_geometry
  use eels_qpt

  integer                   :: ngeels
  integer, allocatable      :: iGz0indx(:)
  integer, allocatable      :: iGzrotindx(:)
  logical                   :: lorderG
  save
  private

  public :: setup_iGz, setup_iGzrot, iGz0indx, lorderG, modify_Xgspace
  public :: iGzrotindx ! only for dealloc...

contains

  subroutine modify_Xgspace( iq, ngblk )
    use R_lattice,          ONLY  : g_rot, bare_qpg
    implicit none
    integer,          intent(in) :: iq, ngblk
    real(SP)                     :: rtmp(ngblk)

    if(lorderG) then
      g_rot(1,:) = -1 ! For checking
      g_rot(1,1:ngblk) = iGzrotindx(1:ngblk)
      rtmp(1:ngblk) = bare_qpg( iq, iGzrotindx(1:ngblk) )  
      bare_qpg(iq,1:ngblk) = rtmp(1:ngblk)
    endif

    return
  end subroutine modify_Xgspace

  subroutine setup_iGzrot( iqi, isi,   ngblk ) 
    !
    ! Calculates index of {G_1} vectors for symmop R (isi) such that
    ! {G_1} = R^-1 {G_z+ G_|| + G_R} in general, or:
    ! {G_1} = R^-1 {G_z} for now:
    ! i.e. iGzrotindx(iG) = R^-1 {G}
    !
    use com,                only : error
    use R_lattice,          ONLY  : g_rot,ng_closed
    use D_lattice,          ONLY  : sop_inv, dl_sop
    implicit none
    integer,          intent(in) :: iqi, isi, ngblk
    integer                      :: ig, ig1, ig2, ig_, isinv
    character(lchlen)            :: lch1

    ! NB: isinv != isi + nsym/(itr+1)+1 -- that is I^-1 R !
    isinv = sop_inv(isi) ! This is the inverse operator.   
    write(lch1,'(a10,i2,a)') "R     (is=",isi,") "
!   call msg('nr',trim(lch1),reshape(dl_sop(:,:,isi),(/9/)))   
    call msg('nr',trim(lch1),dl_sop(1,:,isi))   
    call msg('r',trim(lch1),dl_sop(2,:,isi))   
    call msg('r',trim(lch1),dl_sop(3,:,isi))   
    write(lch1,'(a10,i2,a)') "R^{-1}(is=",isinv,") "
!   call msg('r',trim(lch1),reshape(dl_sop(:,:,isinv),(/9/)))   
    call msg('nr',trim(lch1),dl_sop(1,:,isinv))   
    call msg('r',trim(lch1),dl_sop(2,:,isinv))   
    call msg('r',trim(lch1),dl_sop(3,:,isinv))   

    iGzrotindx(:) = 0
! NEED to check that enough Gs are used...
! 4it/gops.F: allocate(g_rot(nsym,ng_closed))
    do ig_ = 1, ngblk
!   do ig_ = 1, ngeels
      ig = iGz0indx(ig_)                    ! the real eps(Q_||,G,G')
!     write(*,*) "ig",ig_,ngeels,ig,isinv,size(g_rot,1),size(g_rot,2)
      if(ig.gt.size(g_rot,2)) then
         call error("Reqested too many g-vectors, rerun gops")
      endif
      iGzrotindx(ig_) = g_rot( isinv, ig )  ! the...
!     iGzrotindx(ig_) = iGz0indx(ig_)  ! quick fix
!     write(*,*) "ig",ig_,ig,isinv,iGzrotindx(ig_)
    enddo
    if(ANY(iGzrotindx(1:ngblk).eq.0)) stop "Wrong iGzrotindx"
    !
    ! Report the new iGz0 list
    !
    write(lch1,99) "ig","iG","ix","iy","iz","iG_1","ix","iy","iz"
    call msg('r',trim(lch1))
    do ig_ = 1,ngblk
      ig  =   iGz0indx( ig_) 
      ig1 = iGzrotindx( ig_) 
      write(lch1,100) ig_, ig,gvecaff(ig,ix),gvecaff(ig,iy),gvecaff(ig,iz), &
&     ig1,gvecaff(ig1,ix),gvecaff(ig1,iy),gvecaff(ig1,iz)
      call msg('r',trim(lch1))
    enddo

!   Want to calculate eps_G1,G1p(qibz)

    return
99  format(a5,a7,3a4,2x,a7,3a4)
100 format(i5,i7,3i4,2x,i7,3i4)
  end subroutine setup_iGzrot

  subroutine setup_iGz( iqmin, iqmax )
! ngeels is the number of G = (0,0,Gz) in ng_vec
    use R_lattice,               only : ng_vec
    use wave_func,               only : wf_ng
    implicit none
    integer, intent(in)          :: iqmin, iqmax
    integer                      :: ig, ig1
    integer                      :: gtemp(ng_vec), ii
    logical                      :: lbelow
    character(lchlen)            :: lch1

   ! This routine necessary if Q = q+G has G != 0

    ngeels = 0
    do ig = 1, ng_vec
      !
      !  Search for the true set of G = (0,0,Gz)
      !
      if(gvecaff(ig,ix).eq.0.and.&
&        gvecaff(ig,iy).eq.0) then
           ngeels = ngeels + 1
           gtemp(ngeels) = ig
      endif
    enddo 
    allocate(iGz0indx(ngeels))
    allocate(iGzrotindx(ngeels))

    write(lch1,103) "iGz","iG","Gx","Gy","Gz"
    call msg('r',lch1)
    iGz0indx(1:ngeels) = gtemp(1:ngeels)
    lbelow = .true.
    ii = 0
    do ig = 1,ngeels
      ig1 = iGz0indx(ig)
      if(ig1.gt.wf_ng.and.lbelow) then
         lbelow =.false.
         write(lch1,101) "wf_ng",wf_ng
         call msg('r',lch1)
      endif
      write(lch1,100) ig,ig1,gvecaff(ig1,ix),gvecaff(ig1,iy),gvecaff(ig1,iz)
      call msg('r',lch1)
      if(.not.lbelow) ii = ii+1
      if(ii.eq.5) exit 
    enddo
    write(lch1,102) 
    call msg('r',lch1)
    call msg('r',lch1)
    call msg('r',lch1)
    write(lch1,101) "ngmax",ng_vec
    call msg('r',lch1)
    ig1 = iGz0indx(ngeels)
    write(lch1,100) ngeels,ig1,gvecaff(ig1,ix),gvecaff(ig1,iy),gvecaff(ig1,iz)
    call msg('r',lch1)
    write(lch1,101) "ngeels",ngeels
    
    return
99  format(a,i6,a,i5,a,i5)
100 format(2i7,3x,3i4)
103 format(2a7,3x,3a4)
102 format(2(1x,"------"),(1x,"--"),3(1x,"---"))
101 format(2x,a7,1x,20("-"),"> ",i7)

    return
  end subroutine setup_iGz

end module eels_gvec

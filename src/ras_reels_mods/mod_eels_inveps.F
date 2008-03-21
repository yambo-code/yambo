!
! Copyright (C) 2000-2008 C. Hogan  and the YAMBO team 
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
module eels_inveps
  !
  ! Handles calculation, writing, printing and processing of eps-1
  !
  use pars,                   ONLY : schlen, SP, pi, lchlen

! logical                   :: linveps
  complex(SP), allocatable        :: epsm1_avA(:,:,:)
  save
  private

  public :: average_inveps, interpolate_inveps, sort_eps, rotate_eps, print_eps, epsm1_avA
  

contains

  subroutine average_inveps ! for q + -q
  end subroutine average_inveps

  subroutine rotate_eps( iqi, iqf, isi, epsm1 )
    implicit none
    complex(SP), intent(inout)       :: epsm1(:,:,:)
    integer, intent(in)              :: iqi, iqf, isi
    return
  end subroutine rotate_eps

  subroutine interpolate_inveps(epsm1_av, epsm1_avB )
    !    
    !          x x x X x x x   B
    !  . . . O o o o[ ]        A  
    !          1 2 3 4 
    !    
    implicit none
    complex(SP), intent(in)          :: epsm1_avB(:,:,:)
    complex(SP), intent(inout)       :: epsm1_av(:,:,:)
    integer                          :: nwp, ngblk, ig1, ig2, iw
    real(SP)                         :: mixfac

    nwp = size(epsm1_av,3)
    ngblk = size(epsm1_av,1)
    if(size(epsm1_avB,3).ne.nwp.or.size(epsm1_avA,3).ne.nwp) stop "inconsistent interp"

    do iw = 1,nwp
      mixfac = real(iw)/real(nwp) 
      do ig1 = 1,ngblk
      do ig2 = 1,ngblk
        epsm1_av(ig1,ig2,iw) =     mixfac * epsm1_avB(ig1,ig2,iw) + &
&                       (1.0_SP-mixfac) * epsm1_avA(ig1,ig2,iw)
      enddo
      enddo
    enddo

    return
  end subroutine interpolate_inveps


  subroutine sort_eps(iGtmp,epstmp,iGindx,gvecaff,eps,ngblk)
    !   
    !   Use this routine to order eps for printing with respect to
    !   iGz0indx elements or default gvec ordering
    !   Generates: iGtmp(iG,3) array = gvecaff
    !              epstmp(8,8)       = eps
    ! 
    use eels_gvec,             ONLY : lorderG
    implicit none
    integer, intent(in)            :: iGindx(:)
    integer, intent(in)            :: gvecaff(:,:)
    integer, intent(out)           :: iGtmp(:,:)
    complex(SP), intent(in)        :: eps(:,:)
    complex(SP), intent(out)       :: epstmp(:,:)
    integer                        :: ig1, ig2, itmp, ig, iig1,iig2
    integer                        :: ngblk

    itmp = min(8,ngblk) 
    !
    !   G-vectors
    !
    do ig1 = 1, itmp
       ig  = iGindx(ig1)
       if(lorderG)      iGtmp(ig1,:) = gvecaff(ig,:)   ! Gz indices
       if(.not.lorderG) iGtmp(ig1,:) = gvecaff(ig1,:)  ! G  indices
    enddo
    if(lorderG.and.ngblk.gt.8) &
&              iGtmp(9,:) = gvecaff(iGindx(ngblk),:) 
    if((.not.lorderG).and.ngblk.gt.8) &
&              iGtmp(9,:) = gvecaff(ngblk,:) 
    !
    !   eps matrix
    !
    epstmp(1:itmp,1:itmp) = eps(1:itmp, 1:itmp)
    if(ngblk.gt.8) then 
      epstmp(1:8, 9  ) = eps(  1:8, ngblk)
      epstmp( 9 , 1:8) = eps(ngblk, 1:8)
      epstmp( 9 , 9  ) = eps(ngblk, ngblk)
    endif

    return
  end subroutine sort_eps

  subroutine print_eps(iGtmp,eps,ngblk,prmsg)
    use com,   ONLY:msg
    implicit none
    integer,intent(in)             :: iGtmp(:,:), ngblk
    complex(SP), intent(in)        :: eps(:,:)
    integer                        :: ig1, ig2
    character(14)                  :: form
    character(*)                   :: prmsg
    character(lchlen)              :: lch1,lch2
    integer                        :: itmp8, itmp9

    itmp8 = min(8,ngblk)
    itmp9 = min(9,ngblk)
    call msg('nr',trim(prmsg))

    write(form,'(a5,i3,a6)') '(10x,',11*itmp9,'("-"))'        
    write(lch2,form)

    call msg('nr',trim(lch2))
    write(lch1,9) (iGtmp(ig1,:),ig1=1,itmp9)
    call msg('r',trim(lch1))
    call msg('r',trim(lch2))

    do ig1=1, itmp9
      if(ngblk.gt.9.and.ig1.eq.9) call msg('r',trim(lch2))
      write(lch1,10) iGtmp(ig1,:),(real(eps(ig1,ig2)),ig2=1,itmp9)
      call msg('r',trim(lch1))
    enddo        
    call msg('r',trim(lch2))

    call msg('nr',trim(lch2))
    write(lch1,9) (iGtmp(ig1,:),ig1=1,itmp9)
    call msg('r',trim(lch1))
    call msg('r',trim(lch2))

    do ig1=1,itmp9
      if(ngblk.gt.9.and.ig1.eq.9) call msg('r',trim(lch2))
      write(lch1,10) iGtmp(ig1,:),(aimag(eps(ig1,ig2)),ig2=1,itmp9)
      call msg('r',trim(lch1))
    enddo
    call msg('r',trim(lch2))

    return
10 format(3i3,"|",8(f10.5,"|"),"|",f10.5,"|")
9 format(10x,8(1x,3i3,"|"),"|",(1x,3i3,"|"))
100 format(1x,30("-"))
  end subroutine print_eps

!<-- DEBUG
! subroutine print_eels_inveps
!   use msgp
!   use msgm
!   implicit none
!    if(lanalytical) &
!&     call msg('nr','Analytical integration of inverse epsilon.')
!    if(.not.lanalytical) &
!&     call msg('nr','Numerical integration of inverse epsilon.')
!    call msg('r','Integration limits:')
!    call msg('r','Vacuum  limit (a.u.) :',dv )
!    call msg('r','Surface limit (a.u.) :',ds )
!    call msg('r','Slab    limit (a.u.) :',dc )
!   return
! end subroutine print_eels_inveps
!--> DEBUG

end module eels_inveps

!
! Copyright (C) 2000-2010 C. Attaccalite and the YAMBO team 
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
subroutine bz_interpolation(R1D,R2D,C1D,C2D,USER_k,ID)
  use pars,        ONLY:SP
  use R_lattice,   ONLY:bz_samp
  use com,         ONLY:error
  use interpolate, ONLY:interpls,REAL1D,REAL2D,CMPLX1D,CMPLX2D,nwave,get_ID
  implicit none
  type(bz_samp), intent(in)            :: USER_k
  real(SP),      intent(out), optional :: R1D(:,:),R2D(:,:,:)
  complex(SP),   intent(out), optional :: C1D(:,:),C2D(:,:,:)
  integer,       intent(in)            :: ID
  !
  ! Work Space
  !
  real(SP), allocatable :: RDATA(:,:)
  integer               :: outdim(2),ik,i1,twoi1
  !
  if(interpls(ID)%ndim==0) call error("Interpolation coefficients not present!")
  !
  if(interpls(ID)%interp_type==REAL1D.and.present(R1D)) then
    call fourier_interpolation(USER_k,R1D,interpls(ID)%engre,interpls(ID)%ndim)
    return
  endif
  !
  allocate(RDATA(interpls(ID)%ndim,USER_k%nibz))
  call fourier_interpolation(USER_k,R1D,interpls(ID)%engre,interpls(ID)%ndim)
  !
  if(interpls(ID)%interp_type==CMPLX1D.and.present(C1D)) then
    !   
    outdim(1)=interpls(ID)%ndim/2
    !
    do ik=1,USER_k%nibz
      C1D(1:outdim(1),ik)=CMPLX(RDATA(1:outdim(1),ik),RDATA(outdim(1)+1:2*outdim(1),ik))
    enddo
    !
  elseif(interpls(ID)%interp_type==REAL2D.and.present(R2D)) then
    !
    outdim(1)               =size(R2D(:,1,1))
    outdim(2)               =size(R2D(1,:,1))
    do i1=1,outdim(2)
      R2D(1:outdim(1),i1,1:USER_k%nibz)=RDATA((i1-1)*outdim(1)+1:i1*outdim(1),1:USER_k%nibz)
    enddo
    !
  elseif(interpls(ID)%interp_type==CMPLX2D.and.present(C2D)) then
    !
    outdim(1)               =size(C2D(:,1,1))
    outdim(2)               =size(C2D(1,:,1))
    !
    do i1=1,outdim(2)
      twoi1=i1+outdim(2)
      do ik=1,USER_k%nibz
        C2D(1:outdim(1),i1,ik)   =cmplx(RDATA((i1-1)*outdim(1)+1:i1*outdim(1),ik),RDATA((twoi1-1)*outdim(1)+1:twoi1*outdim(1),ik)) 
      enddo
    enddo
    !
  endif
  !
  deallocate(RDATA)
  !
end subroutine bz_interpolation

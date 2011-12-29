!
! Copyright (C) 2000-2012 C. Attaccalite and the YAMBO team 
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
subroutine eval_interpolation_coeff(R1D,R2D,C1D,C2D,k,ID)
  use pars,        ONLY:SP
  use R_lattice,   ONLY:bz_samp
  use interpolate, ONLY:interpls,REAL1D,REAL2D,CMPLX1D,CMPLX2D,nwave,get_ID
  implicit none
  type(bz_samp), intent(in)           :: k
  real(SP),      intent(in), optional :: R1D(:,:),R2D(:,:,:)
  complex(SP),   intent(in), optional :: C1D(:,:),C2D(:,:,:)
  integer,       intent(inout)        :: ID
  !
  ! Work Space
  !
  real(SP), allocatable :: RDATA(:,:)
  integer               :: inpdim(2),i1
  !
  call get_ID(ID)
  !
  if(present(R1D)) then
    !
    interpls(ID)%interp_type=REAL1D
    interpls(ID)%ndim       =size(R1D(:,1))
    allocate(interpls(ID)%engre(nwave,interpls(ID)%ndim))
    call bz_interp_coeff(k,R1D,interpls(ID)%engre,interpls(ID)%ndim)
    return
    !
  endif
  !
  if(present(C1D)) then
    !
    interpls(ID)%interp_type=CMPLX1D
    inpdim(1)               =size(C1D(:,1))
    interpls(ID)%ndim       =2*inpdim(1)
    allocate(interpls(ID)%engre(nwave,interpls(ID)%ndim))
    allocate(RDATA(interpls(ID)%ndim,k%nibz))
    RDATA(1:inpdim(1),1:k%nibz)            = real(C1D(1:inpdim(1),1:k%nibz))
    RDATA(inpdim(1)+1:2*inpdim(1),1:k%nibz)=aimag(C1D(1:inpdim(1),1:k%nibz))
    !
  elseif(present(R2D)) then
    !
    interpls(ID)%interp_type=REAL2D
    inpdim(1)               =size(R2D(:,1,1))
    inpdim(2)               =size(R2D(1,:,1))
    interpls(ID)%ndim       =inpdim(1)*inpdim(2)
    allocate(interpls(ID)%engre(nwave,interpls(ID)%ndim))
    allocate(RDATA(interpls(ID)%ndim,k%nibz))
    do i1=1,inpdim(2)
      RDATA((i1-1)*inpdim(1)+1:i1*inpdim(1),1:k%nibz)            = R2D(1:inpdim(1),i1,1:k%nibz)
    enddo
    !
  elseif(present(C2D)) then
    !
    interpls(ID)%interp_type=CMPLX2D
    inpdim(1)               =size(C2D(:,1,1))
    inpdim(2)               =size(C2D(1,:,1))
    interpls(ID)%ndim       =2*inpdim(1)*inpdim(2)
    allocate(interpls(ID)%engre(nwave,interpls(ID)%ndim))
    allocate(RDATA(interpls(ID)%ndim,k%nibz))
    do i1=1,inpdim(2)
      RDATA((i1-1)*inpdim(1)+1:i1*inpdim(1),1:k%nibz) =  real(C2D(1:inpdim(1),i1,1:k%nibz))
    enddo
    do i1=inpdim(2)+1,2*inpdim(2)
      RDATA((i1-1)*inpdim(1)+1:i1*inpdim(1),1:k%nibz) = aimag(C2D(1:inpdim(1),i1,1:k%nibz))
    enddo
    !
  endif
  !  
  call bz_interp_coeff(k,R1D,interpls(ID)%engre,interpls(ID)%ndim)
  deallocate(RDATA)
  !
 end subroutine  eval_interpolation_coeff

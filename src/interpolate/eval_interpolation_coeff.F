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
subroutine eval_interpolation_coeff(R1D,R2D,C1D,C2D,k,ID)
  use pars,        ONLY:SP
  use R_lattice,   ONLY:bz_samp
  use interpolate, ONLY:interpls,REAL1D,REAL2D,CMPLX1D,CMPLX2D,nwave,get_ID
  implicit none
  type(bz_samp), intent(in)           :: k
  real(SP),      intent(in), optional :: R1D(:,:),R2D(:,:,:)
  real(SP),      intent(in), optional :: C1D(:,:),C2D(:,:,:)
  integer,       intent(inout)        :: ID
  !
  ! Work Space
  !
  real(SP), allocatable :: RDATA(:,:)
  !
  call get_ID(ID)
  !
  if(present(R1D)) then
    !
    interpls(ID)%interp_type=REAL1D
    interpls(ID)%ndim       =size(R1D(:,1))
    allocate(interpls(ID)%engre(nwave,interpls(ID)%ndim))
    call bz_interp_coeff(k,R1D,interpls(ID)%engre,interpls(ID)%ndim)
    !
  endif
  !  
 end subroutine  eval_interpolation_coeff

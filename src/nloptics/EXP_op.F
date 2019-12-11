!
!        Copyright (C) 2000-2019 the YAMBO team
!            http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AC MG 
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
subroutine EXP_op(U,H,dT)
  use pars,           ONLY:SP,cI,cZERO,cONE
  use nl_optics,      ONLY:NL_bands
  implicit none
  complex(SP), intent(out) :: U(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  complex(SP), intent(in)  :: H(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  real(SP),    intent(in)  :: dT
  !
  ! Work Space
  !
  complex(SP)  :: H_tmp(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  integer      :: i1
  !
  ! Expand U=exp(-i*dt*H) up to the fourth order
  !
  U=cZERO
  do i1=NL_bands(1),NL_bands(2)
    U(i1,i1)=cONE
  enddo
  !
  H_tmp = matmul(H,H)
  U=U-cI*dT*H+(cI*dT)**2*H_tmp/2._SP-(cI*dT)**3/6._SP* matmul(H_tmp,H)
  U=U+(cI*dT)**4/24._SP*matmul(H_tmp,H_tmp)
    !
    ! H_tmp=matmul(H_tmp,H)
    ! !
    ! U=U-(cI*dT)**5/120._SP*H_tmp
    ! !
    ! H_tmp=matmul(H_tmp,H)
    ! !
    ! U=U+(cI*dT)**6/720._SP*H_tmp
    ! !
    ! H_tmp=matmul(H_tmp,H)
    ! !
    ! U=U-(cI*dT)**7/5040._SP*H_tmp
    ! !
    ! H_tmp=matmul(H_tmp,H)
    ! !
    ! U=U+(cI*dT)**8/40320._SP*H_tmp
    ! !
    ! U_out(ib,:,:)=U(:,:)
    !
end subroutine EXP_op

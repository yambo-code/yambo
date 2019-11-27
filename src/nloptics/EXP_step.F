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
subroutine EXP_step(K_out,V_in,H_in,I_in,nbf,dT)
  use pars,           ONLY:SP,cZERO
  use nl_optics,      ONLY:NL_bands
  implicit none
  integer,     intent(in)  :: nbf
  complex(SP), intent(in)  :: V_in(NL_bands(2),nbf)
  complex(SP), intent(out) :: K_out(NL_bands(2),nbf)
  complex(SP), intent(in)  :: H_in(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  complex(SP), intent(in)  :: I_in(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nbf)
  real(SP),    intent(in)  :: dT
  !
  ! Work Space
  !
  complex(SP)  :: U(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  complex(SP)  :: H(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  integer      :: ib
  !
  ! Expand U=exp(-i*dt*H) up to the fourth (eigth) order
  !
!$omp parallel do default(shared), private(ib,U,H)
  do ib=1,nbf
    !
    H=H_in+I_in(:,:,ib)
    !
    call EXP_op(U,H,dT)
    !
    K_out(NL_bands(1):,ib)=matmul(U,V_in(NL_bands(1):,ib))-V_in(NL_bands(1):,ib)
    !
  enddo
!$omp end parallel do
  !
  if(NL_bands(1)/=1) K_out(:NL_bands(1)-1,:)=cZERO
  !
end subroutine

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
subroutine INVINT_step(V_out,V_in,H_in,I_in,nbf,dT)
  !
  use wrapper,        ONLY:M_by_M,M_by_V
  use pars,           ONLY:SP,cI,cZERO,cONE
  use nl_optics,      ONLY:NL_bands,NL_nbands
  use openmp,         ONLY:OPENMP_update,master_thread
  !
  implicit none
  !
  integer,     intent(in)  :: nbf
  complex(SP), intent(in)  :: V_in(NL_bands(2),nbf)
  complex(SP), intent(out) :: V_out(NL_bands(2),nbf)
  complex(SP), intent(in)  :: H_in(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  complex(SP), intent(in)  :: I_in(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nbf)
  real(SP),    intent(in)  :: dT
  !
  ! Workspace
  !
  complex(SP)   :: Hp(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  complex(SP)   :: Hm(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
  complex(SP)   :: tmp_det
  integer       :: i1,ib
  logical       :: l_use_inv,l_use_lin
  !
  l_use_inv=.false.
  l_use_lin=.true.
  !
!$omp parallel default(shared), private(ib,Hp,Hm)
  call OPENMP_update(master_thread)
!$omp do
  do ib=1,nbf
    !
    Hp=cZERO
    forall(i1=NL_bands(1):NL_bands(2)) Hp(i1,i1)=cONE
    Hm=Hp
    !
    Hm=Hm-cI*dT/2._SP*(H_in+I_in(:,:,ib))
    Hp=Hp+cI*dT/2._SP*(H_in+I_in(:,:,ib))
    !
    if (l_use_inv) then
      !
      call SERIAL_inversion(NL_nbands,Hp,tmp_det,.false.)
      call M_by_M('n','n',NL_nbands,Hm,Hp,Hm)
      !
    else if (l_use_lin) then
      !
      call SERIAL_lin_system(NL_nbands,NL_nbands,Hp,Hm)
      !
    endif
    !
    call M_by_V('n',NL_nbands,Hm,V_in(NL_bands(1):,ib),V_out(NL_bands(1):,ib))
    !
    if(NL_bands(1)/=1) V_out(:NL_bands(1)-1,ib)=V_in(:NL_bands(1)-1,ib)
    !
  enddo
!$omp end do
!$omp end parallel
  !
  call OPENMP_update(master_thread)
  !
end subroutine INVINT_step

!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine f_build_ext(E,Ef)
 !
 use pars,       ONLY:SP
 use units,      ONLY:HA2EV
 use electrons,  ONLY:levels,spin_occ,n_sp_pol
 use D_lattice,  ONLY:T_elecs,T_holes
 use functions,  ONLY:Fermi_fnc
 !
 implicit none 
 !
 real(SP), intent(in)           :: Ef
 type(levels), intent(inout)    :: E
 !
 ! Work Space
 !
 integer :: i1,i2,is
 !
 !$omp parallel do default(shared), private(i1,i2,is), collapse(3)
 do i1=1,E%nb
   do i2=1,E%nk
     do is=1,n_sp_pol
       if((E%E(i1,i2,is)-Ef)<=0._SP) E%f(i1,i2,is)=spin_occ*Fermi_fnc(E%E(i1,i2,is)-Ef,T_holes)
       if((E%E(i1,i2,is)-Ef)> 0._SP) E%f(i1,i2,is)=spin_occ*Fermi_fnc(E%E(i1,i2,is)-Ef,T_elecs)
     enddo
   enddo 
 enddo
 !$omp end parallel do
 !
end subroutine

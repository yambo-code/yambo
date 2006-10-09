!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
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
 subroutine extend_occupations(X_Efermi,X_nbf,X_nbm,E)
!=====================================================
 use pars,       ONLY:SP,HARTREE
 use electrons,  ONLY:levels,n_spin,spin_occ,BZ_RIM_nbands,&
&                     BZ_RIM_tot_nkpts
 use com,        ONLY:msg,warning
 use D_lattice,  ONLY:Tel
 implicit none
 type(levels)::E
 integer     ::X_nbf,X_nbm
 real(SP)    ::X_Efermi(3)
!ws
 real(SP) :: Ffermi,Ef_test
 real(SP),parameter :: Ef_step=0.1/HARTREE
!
! I extend to the E levels the Fermi level of X k-grid
! being careful to keep the same electronic character
! if the Xk grid.
! If this is not possible a warning message is reported. 
!
 if (.not.associated(E%f)) allocate(E%f(E%nb,E%nk,n_spin))
!
 call f_build(X_Efermi(1))
 E%Efermi=X_Efermi
 call fermi_msg(E,0)
 if (E%nbf==X_nbf.and.E%nbm==X_nbm) then
   E%E=E%E-E%Efermi(1)
   if (associated(E%E_RIM)) E%E_RIM=E%E_RIM-E%Efermi(1)
   return
 endif
 Ef_test=X_Efermi(2)+Ef_step
 do while(Ef_test<X_Efermi(3))
   call f_build(Ef_test)
   call fermi_msg(E,0)
   if (E%nbf==X_nbf.and.E%nbm==X_nbm) then
     E%Efermi(1)=Ef_test
     exit
   endif
   Ef_test=Ef_test+Ef_step
 enddo
 E%E=E%E-E%Efermi(1)
 if (E%nbf/=X_nbf.or.E%nbm/=X_nbm) then
   call warning('X and total energy levels have different metallic character')
 endif
 contains
!
  subroutine f_build(Ef)
!=======================
  integer :: i1,i2,is
  real(SP)::Ef,Ffermi
  do i1=1,E%nb
    do i2=1,E%nk
      do is=1,n_spin
        E%f(i1,i2,is)=spin_occ*Ffermi(E%E(i1,i2,is)-Ef,Tel)
      enddo
    enddo 
  enddo
  if (associated(E%E_RIM)) then
    do i1=1,BZ_RIM_nbands
      do i2=1,BZ_RIM_tot_nkpts
        do is=1,n_spin
          E%f_RIM(i1,i2,is)=spin_occ*Ffermi(E%E_RIM(i1,i2,is)-Ef,Tel)
        enddo
      enddo 
    enddo
  endif
  end subroutine
 end subroutine

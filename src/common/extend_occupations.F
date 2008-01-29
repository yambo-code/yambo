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
subroutine extend_occupations(Xe,E)
 !
 ! The occupation of E levels are decided using 
 ! the Fermi "Levels" of Xe.
 !
 ! Also the RIM levels/occ.s are shifted/defined accordingly.
 ! Note that to check if the RIM is present the Xe%E_RIM pointer
 ! is checked. 
 !
 ! This s.r. takes care that E keeps the same electronic character
 ! if the Xe grid.
 ! If this is not possible a warning message is reported. 
 !
 use pars,       ONLY:SP,HARTREE
 use electrons,  ONLY:levels,n_sp_pol,spin_occ,BZ_RIM_nbands,&
&                     BZ_RIM_tot_nkpts
 use com,        ONLY:msg,warning
 use D_lattice,  ONLY:Tel
 use functions,  ONLY:Fermi_fnc
 implicit none
 type(levels)::Xe,E
 !
 !Work Space
 !
 real(SP) :: Ef_test
 real(SP),parameter :: Ef_step=0.1/HARTREE
 !
 if (.not.associated(E%f)) allocate(E%f(E%nb,E%nk,n_sp_pol))
 !
 E%Efermi=Xe%Efermi
 call f_build(Xe%Efermi(1))
 call fermi_msg(E,0)
 !
 ! If Xe and E have same metallic character then
 ! simply shift E components (Xe have been already shifted
 ! in the Fermi routine)
 !
 if (E%nbf==Xe%nbf.and.E%nbm==Xe%nbm) then
   E%E=E%E-E%Efermi(1)
   if (associated(E%E_RIM)) E%E_RIM =E%E_RIM -E%Efermi(1)
   return
 endif
 !
 ! If Xe and E have not the same metallic character 
 ! find a range for the Efermi that keeps the
 ! to grids with the same character
 !
 Ef_test=Xe%Efermi(2)+Ef_step
 do while(Ef_test<Xe%Efermi(3))
   call f_build(Ef_test)
   call fermi_msg(E,0)
   if (E%nbf==Xe%nbf.and.E%nbm==Xe%nbm) then
     E%Efermi(1)=Ef_test
     exit
   endif
   Ef_test=Ef_test+Ef_step
 enddo
 !
 E%E=E%E-E%Efermi(1)
 if (associated(Xe%E_RIM)) then
   E%E_RIM =E%E_RIM -E%Efermi(1)
   Xe%E_RIM=Xe%E_RIM-E%Efermi(1)
 endif
 !
 if (E%nbf/=Xe%nbf.or.E%nbm/=Xe%nbm) then
   call warning('X and total energy levels have different metallic character')
 endif
 !
 contains
   !
   subroutine f_build(Ef)
   !
   integer :: i1,i2,is
   real(SP):: Ef
   do i1=1,E%nb
     do i2=1,E%nk
       do is=1,n_sp_pol
         E%f(i1,i2,is)=spin_occ*Fermi_fnc(E%E(i1,i2,is)-Ef,Tel)
       enddo
     enddo 
   enddo
   !
   if (associated(E%E_RIM)) then
     do i1=1,BZ_RIM_nbands
       do i2=1,BZ_RIM_tot_nkpts
         do is=1,n_sp_pol
           E%f_RIM(i1,i2,is) =spin_occ*Fermi_fnc(E%E_RIM(i1,i2,is)-Ef,Tel)
           Xe%f_RIM(i1,i2,is)=E%f_RIM(i1,i2,is)
         enddo
       enddo 
     enddo
   endif
   !
   end subroutine
   !
end subroutine

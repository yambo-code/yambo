!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine OCCUPATIONS_Extend(Xe,E)
 !
 ! The occupation of E levels are decided using 
 ! the Fermi "Levels" of Xe.
 !
 ! Also the Double Grid levels/occ.s are shifted/defined accordingly.
 ! Note that to check if the Double Grid is present the Xe%E_DbGd pointer
 ! is checked. 
 !
 ! This s.r. takes care that E keeps the same electronic character
 ! if the Xe grid.
 ! If this is not possible a warning message is reported. 
 !
 use pars,       ONLY:SP
 use units,      ONLY:HA2EV
 use electrons,  ONLY:levels,n_sp_pol,spin_occ,BZ_DbGd_nbands,&
&                     BZ_DbGd_tot_nk_levels
 use com,        ONLY:warning
 use D_lattice,  ONLY:T_elecs,T_holes
 use functions,  ONLY:Fermi_fnc
 use interfaces, ONLY:OCCUPATIONS_Gaps
#include<memory.h>
 !
 type(levels)::Xe,E
 !
 !Work Space
 !
 real(SP) :: Ef_test
 real(SP),parameter :: Ef_step=0.1/HA2EV
 !
  if (.not.allocated(E%f)) then
   YAMBO_ALLOC(E%f,(E%nb,E%nk,n_sp_pol))
 endif
 !
 E%E_Fermi=Xe%E_Fermi
 E%E_VBM =Xe%E_VBM
 E%E_CBm =Xe%E_CBm
 call f_build(Xe%E_Fermi)
 call OCCUPATIONS_Gaps(E)
 !
 ! If Xe and E have same metallic character then
 ! simply shift E components (Xe have been already shifted
 ! in the Fermi routine)
 !
 if (E%nbf==Xe%nbf.and.E%nbm==Xe%nbm) then
   E%E=E%E-E%E_Fermi
   E%E_VBM =E%E_VBM-E%E_Fermi
   E%E_CBm =E%E_CBm-E%E_Fermi
   if (allocated(E%E_DbGd)) E%E_DbGd =E%E_DbGd -E%E_Fermi
   return
 endif
 !
 ! If Xe and E have not the same metallic character 
 ! find a range for the E_Fermi that keeps the
 ! two grids with the same character
 !
 Ef_test=maxval(Xe%E_VBM(:n_sp_pol))+Ef_step
 do while(Ef_test<minval(Xe%E_CBm(:n_sp_pol)))
   call f_build(Ef_test)
   call OCCUPATIONS_Gaps(E)
   if (E%nbf==Xe%nbf.and.E%nbm==Xe%nbm) then
     E%E_Fermi=Ef_test
     exit
   endif
   Ef_test=Ef_test+Ef_step
 enddo
 !
 E%E=E%E-E%E_Fermi
 E%E_VBM =E%E_VBM-E%E_Fermi
 E%E_CBm =E%E_CBm-E%E_Fermi
 if (allocated(Xe%E_DbGd)) E%E_DbGd =E%E_DbGd -E%E_Fermi
 !
 if (E%nbf/=Xe%nbf.or.E%nbm/=Xe%nbm) then
   call warning('Global and response function energy levels have different metallic character')
 endif
 !
 contains
   !
   subroutine f_build(Ef)
     !
     integer :: i1,i2,is
     real(SP):: Ef
     !
     do i1=1,E%nb
       do i2=1,E%nk
         do is=1,n_sp_pol
           if((E%E(i1,i2,is)-Ef)<=0._SP) E%f(i1,i2,is)=spin_occ*Fermi_fnc(E%E(i1,i2,is)-Ef,T_holes)
           if((E%E(i1,i2,is)-Ef)> 0._SP) E%f(i1,i2,is)=spin_occ*Fermi_fnc(E%E(i1,i2,is)-Ef,T_elecs)
         enddo
       enddo 
     enddo
     if (allocated(E%E_DbGd)) then
       do i1=1,BZ_DbGd_nbands
         do i2=1,BZ_DbGd_tot_nk_levels
           do is=1,n_sp_pol
             if( (E%E_DbGd(i1,i2,is)-Ef)<=0._SP) E%f_DbGd(i1,i2,is) =spin_occ*Fermi_fnc(E%E_DbGd(i1,i2,is)-Ef,T_holes)
             if( (E%E_DbGd(i1,i2,is)-Ef)> 0._SP) E%f_DbGd(i1,i2,is) =spin_occ*Fermi_fnc(E%E_DbGd(i1,i2,is)-Ef,T_elecs)
             Xe%f_DbGd(i1,i2,is)=E%f_DbGd(i1,i2,is)
           enddo
         enddo 
       enddo
     endif
   end subroutine
   !
end subroutine

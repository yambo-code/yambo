!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine RT_impose_N_conservation(E,k)
 !
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:levels,spin_occ
 use drivers,        ONLY:l_carrier_dynamics
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,it_now,RT_nk,RT_all2ibz,RT_Nk_ibz
 use real_time,      ONLY:E_E_Hole_lifetime,E_E_Electron_lifetime,MEM_index,&
&                         G_MEM_steps,G_lesser
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k
 !
 ! Work Space
 !
 integer          :: ib,ik,it,i_todo,ikp
 real(SP)         :: f_occ,CONDITION(2),prefactor(2),k_weight
 integer, external:: secant
 !
 if (.not.l_carrier_dynamics) return
 !
 CONDITION=0._SP
 prefactor=(/0._SP,10._SP/)
 it=1
 !
 do while (it<100) 
   if (it==1) i_todo=1
   if (it==2) i_todo=2
   do ik=1,RT_nk
     ikp=RT_all2ibz(ik)
     k_weight=k%weights(ikp)/real(RT_Nk_ibz(ikp),SP)
     do ib=SC_bands(1),SC_bands(2)
       f_occ=-cI*G_lesser(ib,ib,ik,MEM_index(it_now,G_MEM_steps))
       CONDITION(i_todo)=CONDITION(i_todo)+(E_E_Hole_lifetime(ib,ik)*(spin_occ-f_occ)-&
&                        prefactor(i_todo)*E_E_Electron_lifetime(ib,ik)*f_occ)*k_weight
     enddo
   enddo
   it=it+1
   if (it<3) cycle
   i_todo=secant(prefactor(1),CONDITION(1),prefactor(2),CONDITION(2),1.E-7,1)
   if (i_todo<0) exit
   CONDITION(i_todo)=0._SP
 enddo
 !
 do ik=1,RT_nk
   do ib=SC_bands(1),SC_bands(2)
     E_E_Electron_lifetime(ib,ik)=E_E_Electron_lifetime(ib,ik)*prefactor(iabs(i_todo))
   enddo
 enddo
 !
end subroutine RT_impose_N_conservation

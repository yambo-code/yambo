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
subroutine RT_occupations_repair(E,k)
 !
 ! Andrea April 2nd 2012
 ! 
 ! The el-ph interaction and other spurious numerical effects can induce small variations
 ! of the number of electrons. This, in turns, affects all related observables
 !
 use units,          ONLY:HA2EV
 use pars,           ONLY:SP
 use electrons,      ONLY:Nel
 use SC,             ONLY:SC_bands,RT_occupations
 use QP_m,           ONLY:QP_table,QP_n_states
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use vec_operate,    ONLY:sort,degeneration_finder
 !
 implicit none
 !
 type(bz_samp) :: k
 type(levels)  :: E
 !
 ! Work Space
 !
 integer  :: Eo_sorted_index((SC_bands(2)-SC_bands(1)+1)*k%nibz)
 integer  :: first_el((SC_bands(2)-SC_bands(1)+1)*k%nibz)
 integer  :: n_of_el((SC_bands(2)-SC_bands(1)+1)*k%nibz)
 integer  :: n_deg_grp,i1,i2,i3,ib,ik,inx
 !
 real(SP) :: Eo_sorted((SC_bands(2)-SC_bands(1)+1)*k%nibz)
 real(SP) :: sym_value
 !
 call QP_state_table_setup(E)
 !
 call sort(Eo_sorted,indx=Eo_sorted_index)
 call degeneration_finder(Eo_sorted,QP_n_states,first_el,n_of_el,n_deg_grp,0.0001/HA2EV)
 !
 ! COMMENTED
 ! Davide: This must be overcome.
 ! The external field broke the symmetries and degenerate
 ! states [i.e. E(ik)=E(S_old(ik)] can have different occupations
 !
 !
 do i1=1,n_deg_grp
   do i3=1,2
     if (i3==1) sym_value=0.
     do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
       inx = Eo_sorted_index(i2)
       ib  = QP_table(inx,1)
       ik  = QP_table(inx,3)
       if (ib>SC_bands(2)) cycle
       if (ib<SC_bands(1)) then
         if (i3==1) sym_value=sym_value+E%f(ib,ik,1)/real( n_of_el(i1) )
         if (i3==2) E%f(ib,ik,1)=sym_value
       else
         if (i3==1) sym_value=sym_value+RT_occupations(ib,ik)/real( n_of_el(i1) )
         if (i3==2) RT_occupations(ib,ik)=sym_value
       endif
     enddo
   enddo
 enddo
 !
 ! Correction of the number of electrons
 !=======================================
 !
 ! COMMENTED
 ! Davide: This gives a constan drift in the number of electrons / holes
 !         of the lowest unoccupied / highest occupied state which is
 !         problematic for "low pumping"
 !
 ! Andrea: Lines removed. The on-the-fly fix provided by RT_impose_N_conservation.F
 !         makes this procedure redundant
 !
end subroutine RT_occupations_repair

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
 use units,          ONLY:HA2EV,HA2KEL
 use YPP,            ONLY:Fermi_fit_points
 use pars,           ONLY:SP,lchlen
 use electrons,      ONLY:Nel,n_full_bands
 use SC,             ONLY:SC_bands,RT_occupations,n_SC_descriptions,SC_description
 use QP_m,           ONLY:QP_table,QP_n_states
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use vec_operate,    ONLY:sort,degeneration_finder
 use com,            ONLY:warning,of_open_close
 use stderr,         ONLY:real2ch
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
 integer  :: n_deg_grp,i1,i2,i3,ib,ik,inx,i_c
 !
 real(SP) :: Eo_sorted((SC_bands(2)-SC_bands(1)+1)*k%nibz)
 real(SP) :: delta_Nel,sym_value,delta_G
 !
 call QP_state_table_setup(E)
 call eval_delta_Nel()
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
 !if (delta_Nel>1.E-5) call warning('Number viol.   :'//trim(real2ch(delta_Nel)) )
 !
 ! COMMENTED
 ! Davide: This gives a constan drift in the number of electrons / holes
 !         of the lowest unoccupied / highest occupied state which is
 !         problematic for "low pumping"
 !
 !i3=1
 !tresh=1.E-3
 !
 !do while (abs(delta_Nel)>tresh) 
 !  !
 !  if (i3>100) then
 !    tresh=tresh*5
 !    i3=1
 !  endif
 !  !
 !  do ik=1,k%nibz
 !    do ib=SC_bands(1),n_full_bands
 !      !
 !      if (RT_occupations(ib,ik)<1.E-5.or.abs(RT_occupations(ib,ik)-2.)<1.E-5) cycle
 !      !
 !      if (delta_Nel<0.) RT_occupations(ib,ik)=RT_occupations(ib,ik)*1.005
 !      if (delta_Nel>0.) RT_occupations(ib,ik)=RT_occupations(ib,ik)*0.995
 !      !
 !    enddo
 !  enddo
 !  !
 !  call eval_delta_Nel()
 !  !
 !  i3=i3+1
 !  !
 !enddo
 !
 call eval_delta_G()
 !
 contains
   !
   subroutine eval_delta_Nel()
     !
     i_c=0
     !
     delta_Nel=0.0_SP
     do ik=1,E%nk
       do ib=1,SC_bands(2)
         if (ib<SC_bands(1)) then
           delta_Nel=delta_Nel+E%f(ib,ik,1)*k%weights(ik)
         else
           i_c=i_c+1
           Eo_sorted(i_c) =E%E(ib,ik,1)
           delta_Nel=delta_Nel+RT_occupations(ib,ik)*k%weights(ik)
         endif
       enddo
     enddo
     delta_Nel=delta_Nel-Nel
     !
   end subroutine
   !
   subroutine eval_delta_G()
     !
     use pars, ONLY:zero_dfl
     real(SP)     ::dG_sign
     !
     delta_G=0._SP
     !
     do ik=1,E%nk
       do ib=1,SC_bands(2)
         !
         dG_sign=1.
         if (E%f(ib,ik,1)<zero_dfl) dG_sign=-1.
         !
         if (ib<SC_bands(1)) then
           delta_G=delta_G+dG_sign*E%f(ib,ik,1)*k%weights(ik)
         else
           delta_G=delta_G+dG_sign*RT_occupations(ib,ik)*k%weights(ik)
         endif
         !
       enddo
     enddo
     !
   end subroutine
   !
end subroutine RT_occupations_repair

! 
! Copyright (C) 2000-2012 A. Marini and the YAMBO team
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
subroutine RT_occupations_repair(E,k,eval_T)
 !
 ! Andrea April 2nd 2012
 ! 
 ! The el-ph interaction and other spurious numerical effects can induce small variations
 ! of the number of electrons. This, in turns, affects all related observables
 !
 use units,          ONLY:HA2EV
 use YPP,            ONLY:Fermi_fit_points
 use pars,           ONLY:SP
 use electrons,      ONLY:nel,n_full_bands
 use SC,             ONLY:SC_bands,RT_occupations
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use vec_operate,    ONLY:sort,degeneration_finder
 !
 implicit none
 !
 type(bz_samp) :: k
 type(levels)  :: E
 logical       :: eval_T
 !
 ! Work Space
 !
 integer  :: ik,ib,i_c,Eo_sorted_index(SC_bands(2)*k%nibz),n_states,i1,i2,&
&            first_el(SC_bands(2)*k%nibz),n_of_el(SC_bands(2)*k%nibz),n_deg_grp,&
&            QP_table(SC_bands(2)*k%nibz,2),inx,i3
 real(SP) :: Nel_summed,RT_occupations_grouped(SC_bands(2)*k%nibz),f_dist(2),&
&            Eo_sorted(SC_bands(2)*k%nibz),sym_value,gamma_f(2),E_f(2),Eo_over_T,Eo,T_el
 !
 n_states=SC_bands(2)*k%nibz
 !
 i_c=0
 !
 Nel_summed=0.0_SP
 do ik=1,E%nk
   do ib=1,SC_bands(2)
     i_c=i_c+1
     Eo_sorted(i_c) =E%E(ib,ik,1)
     QP_table(i_c,:)=(/ib,ik/)
     if (ib<SC_bands(1)) then
       Nel_summed=Nel_summed+E%f(ib,ik,1)*k%weights(ik)
     else
       Nel_summed=Nel_summed+RT_occupations(ib,ik)*k%weights(ik)
     endif
   enddo
 enddo
 !
 write (*,*) Nel_summed,Nel
 !
 call sort(Eo_sorted,indx=Eo_sorted_index)
 call degeneration_finder(Eo_sorted,n_states,first_el,n_of_el,n_deg_grp,0.0001/HA2EV)
 !
 do i1=1,n_deg_grp
   do i3=1,2
     if (i3==1) sym_value=0.
     do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
       inx = Eo_sorted_index(i2)
       ib  = QP_table(inx,1)
       ik  = QP_table(inx,2)
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
 f_dist=1000.
 !
 do i2=1,2
   do i1=1,n_deg_grp
     inx = Eo_sorted_index(first_el(i1))
     ib  = QP_table(inx,1)
     ik  = QP_table(inx,2)
     if (i2==2) then
       if (ib>SC_bands(1)) then
         if (ib<=n_full_bands) write (100,*) E%E(ib,ik,1)*HA2EV,RT_occupations(ib,ik)
         if (ib> n_full_bands) write (100,*) E%E(ib,ik,1)*HA2EV,RT_occupations(ib,ik)
       endif
     else
       if ( abs(RT_occupations(ib,ik)-Fermi_fit_points(1))<f_dist(1)) then
         f_dist(1)= abs(RT_occupations(ib,ik)-Fermi_fit_points(1))
         E_f(1)=E%E(ib,ik,1)
         gamma_f(1)=log(2./RT_occupations(ib,ik)-1.)
       endif
       if ( abs(RT_occupations(ib,ik)-Fermi_fit_points(2))<f_dist(1)) then
         f_dist(2)= abs(RT_occupations(ib,ik)-Fermi_fit_points(2))
         E_f(2)=E%E(ib,ik,1)
         gamma_f(2)=log(2./RT_occupations(ib,ik)-1.)
       endif
     endif
   enddo
   if (i2==1) then
     Eo_over_T= (gamma_f(2)*E_f(1)-gamma_f(1)*E_f(2))/(E_f(2)-E_f(1))
     T_el=E_f(1)/(gamma_f(1)+Eo_over_T)
   endif
 enddo
 !
 !Eo_over_T= -gamma_f(1)
 !T_el=E_f(2)/(gamma_f(2)+Eo_over_T)
 !
 Eo=Eo_over_T*T_el
 !
 write (*,*) T_el*HA2EV,Eo*HA2EV
 !
 ! Temperature evaluation
 !
 !
 stop
 !
end subroutine RT_occupations_repair

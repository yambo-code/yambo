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
subroutine RT_occupations_repair(E,k,print_fermi_distro)
 !
 ! Andrea April 2nd 2012
 ! 
 ! The el-ph interaction and other spurious numerical effects can induce small variations
 ! of the number of electrons. This, in turns, affects all related observables
 !
 use units,          ONLY:HA2EV,HA2KEL
 use YPP,            ONLY:Fermi_fit_points,output_fname
 use pars,           ONLY:SP
 use electrons,      ONLY:Nel,n_full_bands
 use SC,             ONLY:SC_bands,RT_occupations,n_SC_descriptions,SC_description
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use vec_operate,    ONLY:sort,degeneration_finder
 use com,            ONLY:msg,of_open_close
 !
 implicit none
 !
 type(bz_samp) :: k
 type(levels)  :: E
 logical       :: print_fermi_distro
 !
 ! Work Space
 !
 integer  :: ik,ib,i_c,Eo_sorted_index(SC_bands(2)*k%nibz),n_states,i1,i2,&
&            first_el(SC_bands(2)*k%nibz),n_of_el(SC_bands(2)*k%nibz),n_deg_grp,&
&            QP_table(SC_bands(2)*k%nibz,2),inx,i3
 real(SP) :: delta_Nel,Eo_sorted(SC_bands(2)*k%nibz),sym_value,gamma_f(2),E_f(2),&
&            Eo_over_T,Eo,T_el,delta_occ,f_dist(2)
 !
 n_states=SC_bands(2)*k%nibz
 !
 call eval_delta_Nel()
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
 ! Correction of the number of electrons
 !=======================================
 !
 delta_occ=delta_Nel/real(SC_bands(2))/real(k%nibz)
 !
 if (delta_Nel>0.) delta_occ=-delta_occ
 !
 if (print_fermi_distro) call msg('s','Number viol.   :',delta_Nel)
 !
 do while (abs(delta_nel)<1.E-5) 
   !
   do ik=1,k%nibz
     do ib=SC_bands(1),SC_bands(2)
       if (RT_occupations(ib,ik)<abs(delta_occ)) cycle
       if (ib>n_full_bands) then
         RT_occupations(ib,ik)=RT_occupations(ib,ik)+delta_occ
       else
         RT_occupations(ib,ik)=RT_occupations(ib,ik)+delta_occ
       endif
     enddo
   enddo
   !
   call eval_delta_Nel()
   !
 enddo
 !
 if (.not.print_fermi_distro) return
 !
 ! Occupation distribution
 !=========================
 !
 output_fname="YPP-RT_occupation_distribution"
 call of_open_close(trim(output_fname),'ot')
 do i_c=1,n_SC_descriptions
   call msg('o distribution','#',trim(SC_description(i_c)),INDENT=0)
 enddo
 call msg('o distribution','#')
 call msg('o distribution','#',(/'E[eV]',' Occ.'/),INDENT=0,USE_TABS=.TRUE.)
 call msg('o distribution','#')
 !
 f_dist=1000.
 !
 do i2=1,2
   do i1=1,n_deg_grp
     inx = Eo_sorted_index(first_el(i1))
     ib  = QP_table(inx,1)
     ik  = QP_table(inx,2)
     if (i2==2) then
       if (ib>SC_bands(1).and.print_fermi_distro) then
         call msg('o '//trim(output_fname),'',(/E%E(ib,ik,1)*HA2EV,RT_occupations(ib,ik)/),INDENT=-2,USE_TABS=.true.)
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
   !
   if (i2==1) then
     !
     ! Temperature evaluation
     !========================
     !
     if (abs(E_f(1))<1.E-10) then
       Eo_over_T= -gamma_f(1)
       T_el     = E_f(2)/(gamma_f(2)+Eo_over_T)
     else
       Eo_over_T= (gamma_f(2)*E_f(1)-gamma_f(1)*E_f(2))/(E_f(2)-E_f(1))
       T_el     = E_f(1)/(gamma_f(1)+Eo_over_T)
     endif
     Eo       = Eo_over_T*T_el
     !
   endif
   !
 enddo
 !
 call of_open_close(trim(output_fname))
 !
 call msg('s','El. temp.[eV/K]:',T_el*(/HA2EV,HA2KEL/))
 call msg('s','Fermi level[eV]:',Eo*HA2EV)
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
         i_c=i_c+1
         Eo_sorted(i_c) =E%E(ib,ik,1)
         QP_table(i_c,:)=(/ib,ik/)
         if (ib<SC_bands(1)) then
           delta_Nel=delta_Nel+E%f(ib,ik,1)*k%weights(ik)
         else
           delta_Nel=delta_Nel+RT_occupations(ib,ik)*k%weights(ik)
         endif
       enddo
     enddo
     delta_Nel=delta_Nel-Nel
     !
   end subroutine
   !
end subroutine RT_occupations_repair

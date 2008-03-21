!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine QP_states_simmetrize(vec_2_sim,en,state_is_2do)
 !
 use pars,          ONLY:SP,HARTREE
 use vec_operate,   ONLY:sort,degeneration_finder
 use electrons,     ONLY:levels
 use QP_m,          ONLY:QP_n_states,QP_table
 !
 implicit none
 real(SP)     ::vec_2_sim(QP_n_states)
 logical      ::state_is_2do(QP_n_states)
 type(levels) ::en
 !
 ! Work Space
 !
 real(SP) ::Eo_sorted(QP_n_states),sym_value
 integer  ::Eo_sorted_index(QP_n_states),i1,inx,i2,&
&           first_el(QP_n_states),n_of_el(QP_n_states),n_deg_grp
 !
 ! Degenerate bands average.
 ! The PW gkkp m.e. are not symmetrized and, consequently,
 ! degnerate bands can be splitted.
 !
 forall(i1=1:QP_n_states) Eo_sorted(i1)=en%E(QP_table(i1,1),QP_table(i1,3),1)
 !
 call sort(Eo_sorted,indx=Eo_sorted_index)
 call degeneration_finder(Eo_sorted,QP_n_states,first_el,n_of_el,n_deg_grp,0.0001/HARTREE)
 !
 state_is_2do=.TRUE.
 !
 do i1=1,n_deg_grp
   !
   sym_value=0.
   !
   do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
     !
     inx   =Eo_sorted_index(i2)
     !
     if (i2>first_el(i1)) state_is_2do(inx)=.FALSE.
     !
     sym_value=sym_value+vec_2_sim(inx)/real( n_of_el(i1) )
     !
   enddo
   !
   do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
     !
     inx   =Eo_sorted_index(i2)
     !
     vec_2_sim(inx)=sym_value
     !
   enddo
   !
 enddo
 !
end subroutine

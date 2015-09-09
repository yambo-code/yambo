! 
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine PARALLEL_Transitions_grouping(Xk)
 !
 use pars,          ONLY:IP,SP
 use BS,            ONLY:BS_nT_at_k,BS_nT_grps,BS_nT_grps_at_k,BS_T_grps_index,BS_T_grp_1st_el
 use parallel_m,    ONLY:PAR_IND_eh,PAR_BS_nT_col_grps,PP_redux_wait,PAR_COM_eh_INDEX,&
&                        PAR_IND_T_groups,PAR_IND_Kk_ibz,PAR_BS_T_grps_index,&
&                        PAR_COM_Xk_ibz_INDEX
 use memory_m,      ONLY:mem_est
 use R_lattice,     ONLY:nXkibz,bz_samp
 use vec_operate,   ONLY:sort
 !
 implicit none
 !
 type(bz_samp) :: Xk
 !
 ! Work Space
 !
 logical              :: l_check
 integer              :: i_k,i_t_at_k,i_Tgrp,i_Tgrp_at_k,it_ref,i_counter
 !
 ! E/h pairs (k resolved)
 !
 ! In this subroutine I distribute the eh transitions within each k. The COMM for this indexing is PAR_COM_eh_INDEX.
 ! Once the group of transitions are build-up the goal is to map them in the total world of transitions groups.
 !
 BS_nT_grps_at_k=PAR_COM_eh_INDEX%n_CPU
 BS_nT_grps=nXkibz*BS_nT_grps_at_k
 !
 ! Loop to define the BS_T_grp_1st_el array
 !
 allocate(BS_T_grp_1st_el(BS_nT_grps))
 call mem_est("BS_T_grp_1st_el",(/BS_nT_grps/),(/IP/))
 !
 BS_T_grp_1st_el=0
 PAR_BS_nT_col_grps =0
 i_counter=PAR_COM_eh_INDEX%CPU_id+1+PAR_COM_Xk_ibz_INDEX%CPU_id*PAR_COM_eh_INDEX%n_CPU
 !
 do i_k=1,nXkibz
   ! 
   if (.not.PAR_IND_Kk_ibz%element_1D(i_k) ) cycle
   !
   do i_t_at_k=1,BS_nT_at_k(i_k)
     !
     if (.not.PAR_IND_eh(i_k)%element_1D(i_t_at_k)) cycle
     !
     if ( i_t_at_k==1 ) l_check= .false.
     if ( i_t_at_k> 1 ) l_check= PAR_IND_eh(i_k)%element_1D(i_t_at_k-1)
     !
     if ( l_check ) cycle
     !
     PAR_BS_nT_col_grps = PAR_BS_nT_col_grps +1
     !
     BS_T_grp_1st_el(i_counter)=i_t_at_k+sum(BS_nT_at_k(:i_k-1))
     !
     i_counter=i_counter+PAR_COM_eh_INDEX%n_CPU*PAR_COM_Xk_ibz_INDEX%n_CPU
     !
   enddo
 enddo
 !
 ! ALL k-eh transitions are diveded in BS_nT_grps groups of transitions
 ! Each CPU takes PAR_BS_nT_grps of them
 !
 call PP_redux_wait(BS_T_grp_1st_el,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(BS_T_grp_1st_el,COMM=PAR_COM_eh_INDEX%COMM)
 !
 call sort(BS_T_grp_1st_el)
 !
 ! Groups index
 !
 allocate(PAR_IND_T_groups%element_1D(BS_nT_grps))
 allocate(PAR_IND_T_groups%n_of_elements(PAR_COM_eh_INDEX%n_CPU))
 allocate(PAR_BS_T_grps_index(BS_nT_grps))
 !
 PAR_IND_T_groups%element_1D=.FALSE.
 PAR_IND_T_groups%n_of_elements=0
 PAR_BS_nT_col_grps = 0
 PAR_BS_T_grps_index = 0
 !
 do i_k=1,nXkibz
   !
   if (.not.PAR_IND_Kk_ibz%element_1D(i_k)) cycle
   !
   do i_t_at_k=1,BS_nT_at_k(i_k)
     !
     if (.not.PAR_IND_eh(i_k)%element_1D(i_t_at_k)) cycle
     !
     if ( i_t_at_k==1 ) l_check= .false.
     if ( i_t_at_k> 1 ) l_check= PAR_IND_eh(i_k)%element_1D(i_t_at_k-1)
     !
     if ( l_check ) cycle
     !
     PAR_BS_nT_col_grps = PAR_BS_nT_col_grps + 1
     !
     it_ref=i_t_at_k+sum(BS_nT_at_k(:i_k-1))
     !
     do i_Tgrp=1,BS_nT_grps
       if (BS_T_grp_1st_el(i_Tgrp)==it_ref) then
         PAR_IND_T_groups%element_1D(i_Tgrp)=.true.
         PAR_IND_T_groups%n_of_elements(PAR_COM_eh_INDEX%CPU_id+1)=&
&                                         PAR_IND_T_groups%n_of_elements(PAR_COM_eh_INDEX%CPU_id+1)+1
         !
         ! Warning: this index is modified by the sorting previously done.
         !          However it is not presently used in the code
         PAR_BS_T_grps_index(i_Tgrp)=PAR_BS_nT_col_grps
       endif
     enddo
     !
   enddo
 enddo
 !
 allocate(BS_T_grps_index(BS_nT_grps_at_k,nXkibz))
 call mem_est("BS_T_grps_index",(/BS_nT_grps/),(/IP/))
 !
 ! This is in agreement to how the transitions are ordered
 ! according to the sorting of BS_T_grp_1st_el
 ! 
 i_Tgrp=0
 do i_k=1,nXkibz
   do i_Tgrp_at_k=1,BS_nT_grps_at_k
     i_Tgrp=i_Tgrp+1
     BS_T_grps_index(i_Tgrp_at_k,i_k)=i_Tgrp
   enddo
 enddo
 !
end subroutine

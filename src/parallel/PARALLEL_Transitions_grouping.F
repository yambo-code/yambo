! 
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine PARALLEL_Transitions_grouping(Xk)
 !
 use pars,          ONLY:SP
 use BS,            ONLY:BS_nT_at_k,BS_nT_grps,BS_T_grp_1st_el,BS_res_ares_dim
 use parallel_m,    ONLY:PAR_IND_eh,PAR_BS_nT_col_grps,PP_redux_wait,PAR_COM_eh_INDEX,&
&                        PAR_IND_T_groups,PAR_IND_Kk_ibz,PAR_BS_T_grps_index,&
&                        PAR_COM_Xk_ibz_INDEX
 use R_lattice,     ONLY:nXkibz,bz_samp
 use vec_operate,   ONLY:sort
#include<memory.h>
 !
 type(bz_samp) :: Xk
 !
 ! Work Space
 !
 logical              :: l_check
 integer              :: i_k,i_k_r,i_k_s,i_t_at_k,n_GRP,i_Tgrp,it_ref,i_counter,i_res_ares
 integer, allocatable :: GRP_data(:)
 integer, allocatable :: GRP_index(:)
 !
 ! E/h pairs (k resolved)
 !
 ! In this subroutine I distribute the eh transitions within each k. The COMM for this indexing is PAR_COM_eh_INDEX.
 ! Once the group of transitions are build-up the goal is to map them in the total world of transitions groups.
 !
 n_GRP=nXkibz*PAR_COM_eh_INDEX%n_CPU*BS_res_ares_dim
 !
 YAMBO_ALLOC(BS_T_grp_1st_el,(n_GRP))
 allocate(GRP_data(n_GRP),GRP_index(n_GRP))
 !
 ! Here PAR_BS_nT_grps is the number of groups without taking into account the k-parallelization.
 ! This is done to define properly the BS_T_grp_1st_el array
 !
 BS_T_grp_1st_el=0
 PAR_BS_nT_col_grps =0
 i_counter=PAR_COM_eh_INDEX%CPU_id+1+PAR_COM_Xk_ibz_INDEX%CPU_id*PAR_COM_eh_INDEX%n_CPU
 !
 do i_res_ares=1,BS_res_ares_dim
 do i_k=1,nXkibz
   i_k_s=1+(i_res_ares-1)*nXkibz
   i_k_r=i_k+(i_res_ares-1)*nXkibz
   ! 
   if (.not.PAR_IND_Kk_ibz%element_1D(i_k) ) cycle
   !
   do i_t_at_k=1,BS_nT_at_k(i_k_r)
     !
     if (.not.PAR_IND_eh(i_k_r)%element_1D(i_t_at_k)) cycle
     !
     if ( i_t_at_k==1 ) l_check= .false.
     if ( i_t_at_k> 1 ) l_check= PAR_IND_eh(i_k)%element_1D(i_t_at_k-1)
     !
     if ( l_check ) cycle
     !
     PAR_BS_nT_col_grps = PAR_BS_nT_col_grps +1
     !
     BS_T_grp_1st_el(i_counter)=i_t_at_k+sum(BS_nT_at_k(:i_k_r-1))
     !
     i_counter=i_counter+PAR_COM_eh_INDEX%n_CPU*PAR_COM_Xk_ibz_INDEX%n_CPU
     !
   enddo
 enddo
 enddo
 !
 call PP_redux_wait(BS_T_grp_1st_el,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(BS_T_grp_1st_el,COMM=PAR_COM_eh_INDEX%COMM)
 !
 ! ALL k are diveded in BS_nT_grps groups of transitions and each CPU takes PAR_BS_nT_grps of them
 !
 GRP_data(:)=BS_T_grp_1st_el(:)
 !
 call sort(GRP_data,indx=GRP_index)
 !
 BS_nT_grps     =0
 BS_T_grp_1st_el=0
 !
 do i_Tgrp=1,n_GRP
   if (GRP_data(i_Tgrp)>0._SP) then
     BS_nT_grps=BS_nT_grps+1
     BS_T_grp_1st_el(BS_nT_grps)=GRP_data(i_Tgrp)
   endif
 enddo
 !
 ! Groups index
 !
 YAMBO_ALLOC(PAR_IND_T_groups%element_1D,(BS_nT_grps))
 YAMBO_ALLOC(PAR_IND_T_groups%n_of_elements,(PAR_COM_eh_INDEX%n_CPU))
 YAMBO_ALLOC(PAR_BS_T_grps_index,(BS_nT_grps))
 !
 PAR_IND_T_groups%element_1D=.FALSE.
 PAR_IND_T_groups%n_of_elements=0
 PAR_BS_nT_col_grps = 0
 PAR_BS_T_grps_index = 0
 !
 do i_res_ares=1,BS_res_ares_dim
 do i_k=1,nXkibz
   i_k_s=1+(i_res_ares-1)*nXkibz
   i_k_r=i_k+(i_res_ares-1)*nXkibz
   !
   if (.not.PAR_IND_Kk_ibz%element_1D(i_k)) cycle
   !
   do i_t_at_k=1,BS_nT_at_k(i_k_r)
     !
     if (.not.PAR_IND_eh(i_k_r)%element_1D(i_t_at_k)) cycle
     !
     if ( i_t_at_k==1 ) l_check= .false.
     if ( i_t_at_k> 1 ) l_check= PAR_IND_eh(i_k_r)%element_1D(i_t_at_k-1)
     !
     if ( l_check ) cycle
     !
     PAR_BS_nT_col_grps = PAR_BS_nT_col_grps + 1
     !
     it_ref=i_t_at_k+sum(BS_nT_at_k(:i_k_r-1))
     !
     do i_Tgrp=1,BS_nT_grps
       if (BS_T_grp_1st_el(i_Tgrp)==it_ref) then
         PAR_IND_T_groups%element_1D(i_Tgrp)=.true.
         PAR_IND_T_groups%n_of_elements(PAR_COM_eh_INDEX%CPU_id+1)=&
&                                         PAR_IND_T_groups%n_of_elements(PAR_COM_eh_INDEX%CPU_id+1)+1
         !
         ! Warning: this index is modified by the sorting previously done.
         !          Should be probably defined via the GRP_index or similar
         !          However it is not presently used in the code.
         PAR_BS_T_grps_index(i_Tgrp)=PAR_BS_nT_col_grps
       endif
     enddo
     !
   enddo
 enddo
 enddo
 !
 ! CLEAN UP
 !
 deallocate(GRP_data,GRP_index)
 !
end subroutine

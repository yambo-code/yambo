! 
!        Copyright (C) 2000-2014 the YAMBO team
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
 use pars,          ONLY:IP
 use BS,            ONLY:BS_nT_at_k,BS_nT_grps,BS_nT_grps_at_k,BS_T_grp_ranges
 use parallel_m,    ONLY:PAR_IND_eh,PAR_IND_Xk_bz,PAR_BS_nT_grps,PP_redux_wait,PAR_COM_eh_INDEX
 use memory_m,      ONLY:mem_est
 use R_lattice,     ONLY:nXkibz,bz_samp
 use vec_operate,   ONLY:i_bz
 implicit none
 !
 type(bz_samp) :: Xk
 !
 ! Work Space
 !
 integer :: i_k,i_t
 !
 ! E/h pairs (k resolved)
 !
 ! In this subroutine I distribute the eh transitions within each k. The COMM for this indexing is PAR_COM_eh_INDEX.
 ! Once the group of transitions are build-up the goal is to map them in the total world of transitions groups.
 !
 ! Each k is diveded in BS_nT_grps_at_k(k) groups of Transitions.
 !
 allocate(BS_nT_grps_at_k(nXkibz))
 call mem_est("BS_nT_grps_at_k",(/nXkibz/),(/IP/))
 !
 allocate(BS_T_grp_ranges( 2*nXkibz, 2))
 call mem_est("BS_T_grp_ranges",(/size(BS_T_grp_ranges)/),(/IP/))
 !
 BS_T_grp_ranges=0
 BS_nT_grps_at_k=0
 PAR_BS_nT_grps =0
 !
 do i_k=1,nXkibz
   !
   if (.not.PAR_IND_Xk_bz%element_1D(i_bz(i_k,1,Xk))) cycle
   !
   do i_t=1,BS_nT_at_k(i_k)
     if (PAR_IND_eh(i_k)%element_1D(i_t)) then
       if ( i_t==1 .or. (i_t>1.and..not.PAR_IND_eh(i_k)%element_1D(i_t-1)) ) then
          BS_nT_grps_at_k(i_k)=BS_nT_grps_at_k(i_k)+1
          PAR_BS_nT_grps = PAR_BS_nT_grps +1
          BS_T_grp_ranges(PAR_BS_nT_grps,:)=(/&
&           i_t+sum(BS_nT_at_k(:i_k-1)),&
&           i_t+sum(BS_nT_at_k(:i_k-1))+PAR_IND_eh(i_k)%n_of_elements(PAR_COM_eh_INDEX%CPU_id+1)-1/)
       endif
     endif
   enddo
   !
 enddo
 !
 call PP_redux_wait(BS_nT_grps_at_k,COMM=PAR_COM_eh_INDEX%COMM)
 !
 ! ALL k are diveded in BS_nT_grps groups of transitions and each CPU takes PAR_BS_nT_grps of them
 !
 BS_nT_grps=sum( BS_nT_grps_at_k )
 !
end subroutine

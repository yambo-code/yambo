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
subroutine RT_Lifetimes_merge_driver(what)
 !
 use RT_lifetimes,   ONLY:RT_EP_emit_life,RT_EE_life,RT_EH_life,RT_EP_abs_life,&
&                         RT_EP_emit_REF_life,RT_EE_REF_life,RT_EH_REF_life,RT_EP_abs_REF_life
 use real_time,      ONLY:RTibz
 use parallel_m,     ONLY:PAR_COM_Xk_ibz_INDEX,PAR_IND_Xk_ibz
 use parallel_int,   ONLY:PP_redux_wait
 use R_lattice,      ONLY:nkibz
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 character(*)            :: what
 !
 ! Work Space
 !
 integer :: ik,k_range(2)
 !
 call timing('RT life merge ('//what//')',OPR='start')
 !
 ! INTRA-k/INTER-k parallel merging
 !==================================
 if (what=="INTRA_k") then
   call RT_Lifetimes_merge_intra_k(RT_EE_life)
   call RT_Lifetimes_merge_intra_k(RT_EP_emit_life)
   call RT_Lifetimes_merge_intra_k(RT_EP_abs_life)
   call RT_Lifetimes_merge_intra_k(RT_EH_life)
 endif
 if (what=="INTER_k".or.what=="REF") then
   call RT_Lifetimes_merge_inter_k(RT_EE_life)
   call RT_Lifetimes_merge_inter_k(RT_EP_emit_life)
   call RT_Lifetimes_merge_inter_k(RT_EP_abs_life)
   call RT_Lifetimes_merge_inter_k(RT_EH_life)
 endif
 !
 ! REF lifetimes PARALLEL merging
 !================================
 !
 if (what=="REF") then
   !
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     k_range=RTibz%k_range(ik,:)
     !
     call ADD_to_REF(RT_EE_life,RT_EE_REF_life)
     call ADD_to_REF(RT_EP_emit_life,RT_EP_emit_REF_life)
     call ADD_to_REF(RT_EP_abs_life,RT_EP_abs_REF_life)
     !call ADD_to_REF(RT_PE_life,RT_PE_REF_life)
     call ADD_to_REF(RT_EH_life,RT_EH_REF_life)
     !
   enddo
   !
   call REDUX(RT_EE_REF_life)
   call REDUX(RT_EP_emit_REF_life)
   call REDUX(RT_EP_abs_REF_life)
   !call REDUX(RT_PE_REF_life)
   call REDUX(RT_EH_REF_life)
   !
 endif
 !
 call timing('RT life merge ('//what//')',OPR='stop')
 !
 contains
   !
   subroutine REDUX(LIFE)
     use RT_lifetimes,   ONLY:RT_lifetime
     type(RT_lifetime)      ::LIFE
     if (.not.LIFE%active) return
     call PP_redux_wait(LIFE%abs,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
     call PP_redux_wait(LIFE%emit,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   end subroutine
   !
   subroutine ADD_to_REF(LIFE,LIFE_ref)
     use RT_lifetimes,   ONLY:RT_lifetime
     type(RT_lifetime)      ::LIFE,LIFE_ref
     if (.not.LIFE%active) return
     LIFE_ref%abs(:,k_range(1):k_range(2))=LIFE%abs(:,k_range(1):k_range(2))
     LIFE_ref%emit(:,k_range(1):k_range(2))=LIFE%emit(:,k_range(1):k_range(2))
   end subroutine
   !
end subroutine RT_Lifetimes_merge_driver

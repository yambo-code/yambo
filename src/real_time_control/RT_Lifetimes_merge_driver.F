!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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

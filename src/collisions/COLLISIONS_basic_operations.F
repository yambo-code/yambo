!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine COLLISIONS_basic_operations( what, COLL )
 !
 ! Here I allocate and define some objects of the COLLISIONS_group kind.
 !
 ! type COLLISIONS_group
 !  character(schlen)     :: name="none"
 !  integer               :: kind=0
 !  integer               :: N           ! # of collisions (dimension of linked COLLISIONS_element)
 !  integer,     pointer  :: state(:,:)   => null()
 !  integer,     pointer  :: IO_status(:) => null()
 ! end type
 !
 use collision_ext, ONLY:COLLISIONS_group,COLL_bands,COLLISIONS_CV_only
 use R_lattice,     ONLY:nXkibz
 use electrons,     ONLY:n_sp_pol,n_full_bands,n_met_bands
 use wave_func,     ONLY:states_to_load
 use IO_m,          ONLY:io_COLLs
 !
#include<memory.h>
 !
 character(*)           :: what
 type(COLLISIONS_group) :: COLL
 !
 ! Work Space
 !
 integer :: i_n,i_m,i_sp,i_k,i_c,i_mix
 !
 if (trim(what)=="WF_distribute") then
   !
   do i_c=1,COLL%N
     !
     if (.not.COLL%PAR_IND%element_1D(i_c)) cycle
     !
     i_n =COLL%state(i_c,1)
     i_m =COLL%state(i_c,2)
     i_k =COLL%state(i_c,3)
     !
     states_to_load(i_n,i_k,:)=.TRUE.
     states_to_load(i_m,i_k,:)=.TRUE.
     !
   enddo
   !
   return
   !
 endif
 !
 if (trim(what)=="clean") then
   !
   if (allocated(COLL%IO_status)) then
     YAMBO_FREE(COLL%IO_status)
     YAMBO_FREE(COLL%state)
     COLL%N=0
   endif
   !
   return
   !
 endif
 !
 if (allocated(COLL%state)) then
   !
   if (io_COLLs.and..not.allocated(COLL%IO_status)) then
     YAMBO_ALLOC(COLL%IO_status,(COLL%N))
   endif
   !
 else
   !
   ! Here below I define the state component to be consistent with the QP_table.
   !
   do i_c=1,2
     !
     COLL%N=0
     !
     do i_k=1,nXkibz
       do i_n=COLL_bands(1),COLL_bands(2)
         do i_mix=0,COLL_bands(2)-COLL_bands(1)
           !
           i_m=i_n+i_mix
           !
           ! No Off-diagonal elements in the case of P collisions
           !
           if (trim(COLL%name)=="Momentum".and.i_mix>0)  cycle
           !
           if (i_m>COLL_bands(2)) cycle
           !
           do i_sp=1,n_sp_pol
             !
             if ( COLLISIONS_CV_only .and. i_n> n_met_bands(i_sp)  ) cycle
             if ( COLLISIONS_CV_only .and. i_m<=n_full_bands(i_sp) ) cycle
             !
             COLL%N=COLL%N+1
             if (i_c==2) then
               COLL%state(COLL%N,:4)=(/i_n,i_m,i_k,i_sp/)
             endif
             if (i_n/=i_m.and.trim(COLL%name)=="GW_NEQ") then
               COLL%N=COLL%N+1
               if (i_c==2) then
                 COLL%state(COLL%N,:4)=(/i_m,i_n,i_k,i_sp/)
               endif
             endif
           enddo
         enddo
       enddo
     enddo
     !
     if (i_c==1) then
       if (io_COLLs) then
         YAMBO_ALLOC(COLL%IO_status,(COLL%N))
       endif
       YAMBO_ALLOC(COLL%state,(COLL%N,4))
     endif
     !
   enddo
   !
 endif
 !
 ! QP_table map  (needed by SC & PARALLEL distribution of worload based on QP_n_states)
 !===============
 call COLLISIONS_map_to_QP_table("fill",COLL)
 !
end subroutine

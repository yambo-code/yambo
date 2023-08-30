!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_QP(comment)
 !
 ! QP_state -> input file
 !
 use pars,      ONLY:SP
 use QP_m,      ONLY:QP_nk,QP_nb,QP_state
 use parser_m,  ONLY:PARSER_array
 use it_tools,  ONLY:check_verbosity
 !
#include<memory.h>
 !
 character(*) ::comment
 ! 
 ! Work Space
 integer              :: n_states
 integer ,allocatable :: I_states(:,:)
 real(SP),allocatable :: R_states(:,:)
 !
 call PARSER_array(mode="REAL dimensions",VAR="QPkrange",N=n_states,M=4)
 if (n_states==0) then
   n_states=1
   if (allocated(QP_state)) then
     n_states=-1
     call QP_state_group(n_states,(/0,0,0,0/))
     YAMBO_ALLOC(I_states,(n_states,4)) 
     call QP_state_group(n_states,I_states)
   else
     YAMBO_ALLOC(I_states,(n_states,4)) 
     I_states(1,:)=(/1,QP_nk,1,QP_nb/)
   endif
 else
   YAMBO_ALLOC(I_states,(n_states,4)) 
   call PARSER_array(mode="read",VAR="QPkrange",N=n_states,M=4,I_v=I_states)
 endif
 call PARSER_array(mode="write",VAR="QPkrange",DESC=trim(comment)//' generalized Kpoint/Band indices',&
&                  N=n_states,M=4,I_v=I_states)
 !
 YAMBO_FREE(I_states)
 !
 if (check_verbosity("qp")) then
   call PARSER_array(mode="REAL dimensions",VAR="QPerange",N=n_states,M=4)
   if (n_states==0) then
     n_states=1
     YAMBO_ALLOC(R_states,(n_states,4)) 
     R_states(1,:)=(/1,QP_nk,0,-1/)
   else
     YAMBO_ALLOC(R_states,(n_states,4)) 
     call PARSER_array(mode="read",VAR="QPerange",N=N_states,M=4,R_v=R_states)
   endif
   call PARSER_array(mode="write",VAR="QPerange",DESC=trim(comment)//' generalized Kpoint/Energy indices',&
&                    N=n_states,M=4,R_v=R_states,Kn=(/"i","i","r","r"/))
 endif
 !
 YAMBO_FREE(R_states)
 !
end subroutine

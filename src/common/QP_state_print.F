!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QP_state_print(qp)
 !
 ! Print the contents of the QP_state logical array
 !
 use pars,          ONLY:lchlen
 use QP_m,          ONLY:QP_state,QP_t
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use descriptors,   ONLY:IO_desc_add
 !
#include<memory.h>
 !
 type(QP_t), optional :: qp    
 !
 ! Work Space
 !
 integer              :: n_states,i_s
 integer ,allocatable :: states(:,:)
 character(lchlen) :: ch
 !
 if (.not.present(qp)) call msg('r','')
 !
 if (present(qp)) then
   call IO_desc_add(qp%desc,kind="s",str=" ",S=" ")
 endif
 !
 n_states=-1
 call QP_state_group(n_states,(/0,0,0,0/))
 YAMBO_ALLOC(states,(n_states,4)) 
 call QP_state_group(n_states,states)
 !
 do i_s=1,n_states
   !
   ch='QP @ state[ '//trim(intc(i_s))//' ]'
   !
   if (present(qp)) then
     !
     call IO_desc_add(qp%desc,kind="i",SZ=2,str=trim(ch)//" K range",I=states(i_s,1:2))
     !
     call IO_desc_add(qp%desc,kind="i",SZ=2,str=trim(ch)//" b range",I=states(i_s,3:4))
     !
   else
     !
     call msg('r',trim(ch)//" K range",states(i_s,1:2))
     call msg('r',trim(ch)//" b range",states(i_s,3:4))
     !
   endif
   !
 enddo  
 !
 YAMBO_FREE(states)
 !
 if (.not.present(qp)) call msg('r','')
 !
end subroutine

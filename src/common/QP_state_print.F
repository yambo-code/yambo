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
subroutine QP_state_print(qp)
 !
 ! Print the contents of the QP_state logical array
 !
 use pars,      ONLY:lchlen
 use QP_m,      ONLY:QP_state,QP_t
 use com,       ONLY:msg
 use stderr,    ONLY:intc
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
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=""
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
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=trim(ch)//" K range"
     qp%desc%size(qp%desc%n)=2
     qp%desc%kind(qp%desc%n)='i'
     qp%desc%ival(:2,qp%desc%n)=states(i_s,1:2)
     !
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=trim(ch)//" b range"
     qp%desc%size(qp%desc%n)=2
     qp%desc%kind(qp%desc%n)='i'
     qp%desc%ival(:2,qp%desc%n)=states(i_s,3:4)
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

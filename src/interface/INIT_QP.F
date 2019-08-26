!
!        Copyright (C) 2000-2019 the YAMBO team
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
 !
 integer              :: n_states,state(4),i_c
 integer,    external :: QP_state_extract
 integer ,allocatable :: I_states(:,:)
 real(SP),allocatable :: R_states(:,:)
 !
 call PARSER_array(mode="REAL dimensions",VAR="QPkrange",N=n_states,M=4)
 if (n_states==0) then
   if (allocated(QP_state)) then
     do i_c=1,2
       state=(/1,1,0,0/)
       n_states=0
       do while(QP_state_extract(state)>0)
         n_states=n_states+1
         if (i_c==2) I_states(n_states,:)=state
       enddo
       if (i_c==1) then
         YAMBO_ALLOC(I_states,(n_states,4)) 
       endif
     enddo
   else
     n_states=1
     YAMBO_ALLOC(I_states,(n_states,4)) 
     I_states(1,:)=(/1,QP_nk,1,QP_nb/)
   endif
 else
   YAMBO_ALLOC(I_states,(n_states,4)) 
   call PARSER_array(mode="read",VAR="QPkrange",N=n_states,M=4,I_v=I_states)
 endif
 call PARSER_array(mode="write",VAR="QPkrange",DESC='# '//trim(comment)//' generalized Kpoint/Band indices',&
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
   call PARSER_array(mode="write",VAR="QPerange",DESC='# '//trim(comment)//' generalized Kpoint/Energy indices',&
&                    N=n_states,M=4,R_v=R_states,Kn=(/"i","i","r","r"/))
 endif
 !
 YAMBO_FREE(R_states)
 !
end subroutine

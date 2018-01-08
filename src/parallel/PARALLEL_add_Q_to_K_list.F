!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine PARALLEL_add_Q_to_K_list(ZONE,IND_in,IND_in_ID,IND_k_out,IND_k_out_ID,&
&                                   IND_q,COMM,q_range,k,q)
 !
 use parallel_m,    ONLY:PP_indexes,MPI_comm
 use R_lattice,     ONLY:qindx_X,nkbz,nkibz,bz_samp,nqbz,qindx_S,qindx_B
 use QP_m,          ONLY:QP_n_states,QP_table
#include<memory.h>
 !
 character(*)           ::ZONE
 type(PP_indexes)       ::IND_in,IND_k_out,IND_q
 integer                ::IND_in_ID,IND_k_out_ID,q_range(2)
 type(MPI_comm)         ::COMM
 type(bz_samp)          ::k,q
 !
 integer :: iq,ikp,ikbz,ik,ikpbz,iqp,iqbz
 !
 IND_k_out_ID=IND_in_ID
 !
 YAMBO_ALLOC(IND_k_out%n_of_elements,(COMM%n_CPU))
 YAMBO_ALLOC(IND_k_out%element_1D,(nkibz))
 !
 IND_k_out%n_of_elements(IND_k_out_ID+1)=0
 IND_k_out%element_1D(:)=.FALSE.
 !
 if (ZONE=="k_bz_q_ibz") then
   !
   do ikbz=1,nkbz
     !
     if (.not.IND_in%element_1D(ikbz)) cycle
     !
     ik  =k%sstar(ikbz,1)
     !
     if (.not.IND_k_out%element_1D(ik)) then
       IND_k_out%element_1D(ik)=.TRUE.
       IND_k_out%n_of_elements(IND_k_out_ID+1)=IND_k_out%n_of_elements(IND_k_out_ID+1)+1
     endif
     !
     do iq=q_range(1),q_range(2)
       !
       if (.not.IND_q%element_1D(iq)) cycle
       !
       ikpbz=qindx_X(iq,ikbz,1)
       ikp  =k%sstar(ikpbz,1)
       !
       if (.not.IND_k_out%element_1D(ikp)) then
         IND_k_out%element_1D(ikp)=.TRUE.
         IND_k_out%n_of_elements(IND_k_out_ID+1)=IND_k_out%n_of_elements(IND_k_out_ID+1)+1
       endif
       !
     enddo
     !
   enddo
   !
 else if (ZONE=="k_bz_q_bz".or.ZONE=="k_bz_q_bz_p_bz") then
   !
   do ikbz=1,nkbz
     !
     if (.not.IND_in%element_1D(ikbz)) cycle
     !
     ik=k%sstar(ikbz,1)
     if (ZONE=="k_bz_q_bz_p_bz") ik=ikbz
     !
     if (.not.IND_k_out%element_1D(ik)) then
       IND_k_out%element_1D(ik)=.TRUE.
       IND_k_out%n_of_elements(IND_k_out_ID+1)=IND_k_out%n_of_elements(IND_k_out_ID+1)+1
     endif 
     !
     do iq=1,nqbz
       !
       if (.not.IND_q%element_1D(iq)) cycle
       !
       ikpbz=qindx_B(ikbz,iq,1)
       ikp  =k%sstar(ikpbz,1)
       if (ZONE=="k_bz_q_bz_p_bz") ikp=ikpbz
       !
       if (.not.IND_k_out%element_1D(ikp)) then
         IND_k_out%element_1D(ikp)=.TRUE.
         IND_k_out%n_of_elements(IND_k_out_ID+1)=IND_k_out%n_of_elements(IND_k_out_ID+1)+1
       endif
       !
     enddo
     !
   enddo
   !
 else if (ZONE=="k_qp_q_ibz".or.ZONE=="k_qp_q_bz") then
   !
   do iqp=1,QP_n_states
     !
     if (.not.IND_in%element_1D(iqp)) cycle
     !
     ik=QP_table(iqp,3)
     !
     if (.not.IND_k_out%element_1D(ik)) then
       IND_k_out%element_1D(ik)=.TRUE.
       IND_k_out%n_of_elements(IND_k_out_ID+1)=IND_k_out%n_of_elements(IND_k_out_ID+1)+1
     endif 
     !
     do iqbz=1,nqbz
       !
       iq=q%sstar(iqbz,1)
       if (ZONE=="k_qp_q_ibz") then
         if(.not.IND_q%element_1D(iq)) cycle
       endif
       if (ZONE=="k_qp_q_bz") then
         if(.not.IND_q%element_1D(iqbz)) cycle
       endif
       !
       ikpbz=qindx_S(ik,iqbz,1)
       ikp  =k%sstar(ikpbz,1)
       !
       if (.not.IND_k_out%element_1D(ikp)) then
         IND_k_out%element_1D(ikp)=.TRUE.
         IND_k_out%n_of_elements(IND_k_out_ID+1)=IND_k_out%n_of_elements(IND_k_out_ID+1)+1
       endif 
       !
     enddo
     !
   enddo
   !
 endif
 !
end subroutine PARALLEL_add_Q_to_K_list

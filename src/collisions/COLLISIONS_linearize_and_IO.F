!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
subroutine COLLISIONS_linearize_and_IO(what,i_qp,COLL,N)
 !
 use pars,            ONLY:SP
 use QP_m,            ONLY:QP_n_states
 use collision,       ONLY:COLLISIONS_element,COLLISIONS_cutoff
 use IO_m,            ONLY:io_control,REP,OP_WR_CL,OP_APP_WR_CL,IO_and_Messaging_switch,io_COLLs
 use drivers,         ONLY:l_elel_scatt
 use interfaces,      ONLY:io_COLLISIONS
 use parallel_m,      ONLY:PAR_QP_index,myid
 !
 implicit none                                              
 !
 character(*)                            :: what
 integer,                  intent(in)    :: i_qp,N
 type(COLLISIONS_element), intent(inout) :: COLL(N)
 !
 ! Work Space
 !
 integer   :: ic,i1,i2,i3,i_cycle,io_err,ID,i_ref
 logical   :: local_condition,l_COHSEX,l_GW_NEQ,l_HF
 real(SP)  :: COLLISIONS_max
 !
 l_HF     =index(what,"HF")>0
 l_COHSEX =index(what,"COHSEX")>0
 l_GW_NEQ =index(what,"GW_NEQ")>0
 !
 ! Reduction of tiny elements of tmp_sx.
 !
 i_ref=1
 if (.not.io_COLLs) i_ref=PAR_QP_index(i_qp)
 !
 if(l_COHSEX.or.l_HF) COLLISIONS_max=maxval(abs(COLL(1)%v3c(:,:,:)))        
 if(l_GW_NEQ        ) COLLISIONS_max=maxval(abs(COLL(1)%v3r(:,:,:)))        
 COLL(i_ref)%table(:,:,:)=0
 !
 do i_cycle=1,2
   ic=0
   do i1=COLL(i_ref)%D(1,1),COLL(i_ref)%D(1,2)
     do i2=COLL(i_ref)%D(2,1),COLL(i_ref)%D(2,2)
       do i3=COLL(i_ref)%D(3,1),COLL(i_ref)%D(3,2)
         if(l_COHSEX.or.l_HF) local_condition=abs(COLL(1)%v3c(i1,i2,i3))>=COLLISIONS_max*COLLISIONS_cutoff.or.&
&                        COLLISIONS_cutoff<0.0.or.l_elel_scatt
         if(l_GW_NEQ) local_condition=abs(COLL(1)%v3r(i1,i2,i3))>=COLLISIONS_max*COLLISIONS_cutoff.or.&
&                        COLLISIONS_cutoff<0.0.or.l_elel_scatt
         if (local_condition) then
           ic=ic+1
           if (i_cycle==2) then
             COLL(i_ref)%table(i1,i2,i3)=1
             if(l_COHSEX.or.l_HF) COLL(i_ref)%v(ic)=COLL(1)%v3c(i1,i2,i3) 
             if(l_GW_NEQ) COLL(i_ref)%v(ic)=cmplx(COLL(1)%v3r(i1,i2,i3),0._SP)
           endif
         endif
       enddo
     enddo
   enddo
   COLL(i_ref)%N=ic
   if (i_cycle==1) call COLLISIONS_alloc_and_free(what,"v1","alloc",i_ref)
 enddo
 !
 if (.not.IO_colls) return
 !
 call IO_and_Messaging_switch("+DB_is_fragmented")
 !
 if (i_qp==1) call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=ID)
 if (i_qp/=1) call io_control(ACTION=OP_APP_WR_CL,SEC=(/2,3/),ID=ID)
 io_err=io_COLLISIONS(i_qp,ID,COLL=COLL(1))
 !
 if (i_qp==QP_n_states) call IO_and_Messaging_switch("-DB_is_fragmented")
 !
 call COLLISIONS_alloc_and_free(what,"v1","clean",1)
 !
end subroutine COLLISIONS_linearize_and_IO

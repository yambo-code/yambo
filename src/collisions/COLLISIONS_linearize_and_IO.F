!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA AM
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
 use collision,       ONLY:COLLISIONS_element,COLLISIONS_cutoff
 use IO_m,            ONLY:io_control,REP,OP_WR_CL,OP_APP_WR_CL,io_COLLs
 use interfaces,      ONLY:io_COLLISIONS
 use parallel_m,      ONLY:PAR_QP_index
 !
 implicit none                                              
 !
 character(*)                            :: what
 integer,                  intent(in)    :: i_qp,N
 type(COLLISIONS_element), intent(inout) :: COLL(N)
 !
 ! Work Space
 !
 integer   :: ic,i1,i2,i3,i_cycle,io_err,ID,i_ref,state(3)
 logical   :: store_it,l_exchange,l_GW_NEQ,l_QED_P
 real(SP)  :: COLLISIONS_max
 !
 l_exchange = (index(what,"Fock")>0).or.(index(what,"SEX")>0)
 l_GW_NEQ   =  index(what,"GW_NEQ")>0
 l_QED_P    =  index(what,"Momentum")>0
 !
 i_ref=1
 if (.not.io_COLLs) i_ref=PAR_QP_index(i_qp)
 !
 ! Reference value for the reduction of the tiny elements of COHSEX/HF collision
 ! integrals.
 !
 if(l_exchange   ) COLLISIONS_max=maxval(abs(COLL(1)%v3_c(:,:,:)))        
 !
 COLL(i_ref)%table(:,:,:)=0
 !
 do i_cycle=1,2
   !
   ic=0
   !
   do i3=COLL(i_ref)%D(3,1),COLL(i_ref)%D(3,2)
     do i2=COLL(i_ref)%D(2,1),COLL(i_ref)%D(2,2)
       do i1=COLL(i_ref)%D(1,1),COLL(i_ref)%D(1,2)
         !
         ! In the COHSEX/HF case a cutoff can be applied
         !
         if (l_exchange) store_it=abs(COLL(1)%v3_c(i1,i2,i3))>=COLLISIONS_max*COLLISIONS_cutoff.or.COLLISIONS_cutoff<0.0
         !
         if (l_GW_NEQ .or. l_QED_P ) store_it=.TRUE.
         !
         if (store_it) then
           ic=ic+1
           if (i_cycle==2) then
             !
             COLL(i_ref)%table(i1,i2,i3)=1
             if (l_exchange          ) COLL(i_ref)%v_c(ic) = COLL(1)%v3_c(i1,i2,i3)
             if (l_GW_NEQ.or.l_QED_P ) COLL(i_ref)%v_r(ic) = COLL(1)%v3_r(i1,i2,i3)
             !
           endif
         endif
         !
       enddo
     enddo
   enddo
   !
   COLL(i_ref)%N=ic
   !
   if (i_cycle==1) call COLLISIONS_alloc_and_free(what,"v1","alloc",i_ref)
   !
 enddo
 !
 if (.not.IO_colls) return
 !
 if (i_qp==1) call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=ID)
 if (i_qp/=1) call io_control(ACTION=OP_APP_WR_CL,SEC=(/2,3/),ID=ID)
 io_err=io_COLLISIONS(i_qp,ID,COLL=COLL(1))
 !
 call COLLISIONS_alloc_and_free(what,"v1","clean",1)
 !
end subroutine COLLISIONS_linearize_and_IO

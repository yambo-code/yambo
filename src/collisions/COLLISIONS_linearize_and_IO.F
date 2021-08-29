!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine COLLISIONS_linearize_and_IO(what,i_coll,last_coll,COLL,COLL_element,COLL_ID,COMM)
 !
 use pars,            ONLY:SP,cZERO
 use parallel_m,      ONLY:master_cpu
 use parallel_int,    ONLY:PP_redux_wait
 use collision_ext,   ONLY:COLLISIONS_element,COLLISIONS_cutoff,COLLISIONS_group
 use IO_int,          ONLY:io_control
 use IO_m,            ONLY:REP,OP_APP_CL,OP_WR_CL,OP_APP,WR,WR_CL,io_COLLs
 use COLL_interfaces, ONLY:io_COLLISIONS
 use timing_m,        ONLY:timing
 !
 implicit none                                              
 !
#include<collisions.h>
 !
 character(*)                            :: what
 integer,                  intent(in)    :: i_coll,last_coll,COMM
 integer,                  intent(inout) :: COLL_ID
 type(COLLISIONS_group),   intent(inout) :: COLL
 type(COLLISIONS_element), intent(inout) :: COLL_element(COLL%N)
 !
 ! Work Space
 !
 integer   :: ic,i1,i2,i3,i_cycle,io_err,i_ref,IO_ACT
 logical   :: store_it,l_HXC,l_GW_NEQ,l_QED_P,l_COH
 real(SP)  :: COLLISIONS_max
 !
#if defined _PAR_IO
 integer, external :: io_collisions_header
#endif
 !
 if (i_coll==0) then
   if(io_COLLs) then
     if(master_cpu) then
       call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=COLL_ID)
       io_err=io_COLLs_header(COLL_ID,COLL)
     endif
#if defined _PAR_IO
     call io_control(ACTION=OP_APP,COM=REP,SEC=(/1,2/),ID=COLL_ID)
     io_err=io_COLLISIONS(COLL_ID,COLL,COLL_element=COLL_element(1),i_coll=i_coll,COMM=COMM)
#endif
   endif
   call PP_redux_wait()
   return
 endif
 !
 call timing('COLLISIONS_linearize_and_IO',OPR='start')
 !
 l_COH      = index(what,"COH")     >0 
 l_HXC      = index(what,"HXC")     >0
 l_GW_NEQ   = index(what,"GW_NEQ")  >0
 l_QED_P    = index(what,"Momentum")>0
 !
 i_ref=1
 if (.not.io_COLLs) i_ref=COLL%PAR_map(i_coll)
 !
 ! Reference value for the reduction of the tiny elements of COHSEX/HF collision
 ! integrals.
 !
 if ( l_HXC .or. l_COH ) COLLISIONS_max=maxval(abs(COLL_element(1)%v3_c(:,:,:)))        
 !
 COLL_element(i_ref)%table(:,:,:)='f'
 store_it=.true.
 !
 do i_cycle=1,2
   !
   ic=0
   !
   do i3=COLL%D(3,1),COLL%D(3,2)
     do i2=COLL%D(2,1),COLL%D(2,2)
       do i1=COLL%D(1,1),COLL%D(1,2)
         !
         ! In the COHSEX/HF case a cutoff can be applied
         !
         if (l_HXC.or.l_COH) then
           store_it=abs(COLL_element(1)%v3_c(i1,i2,i3))>=COLLISIONS_max*COLLISIONS_cutoff .or. COLLISIONS_cutoff<0.0
         endif
#if defined _PAR_IO
         if (.not.store_it) then
           COLL_element(1)%v3_c(i1,i2,i3)=cZERO
           store_it=.true.
         endif
#endif
         !
         if (store_it) then
           ic=ic+1
           if (i_cycle==2) then
             !
             COLL_element(i_ref)%table(i1,i2,i3)='t'
             if (l_HXC   .or.l_COH   ) COLL_element(i_ref)%v_c(ic) = COLL_element(1)%v3_c(i1,i2,i3)
             if (l_GW_NEQ.or.l_QED_P ) COLL_element(i_ref)%v_r(ic) = COLL_element(1)%v3_r(i1,i2,i3)
             !
           endif
         endif
         !
       enddo
     enddo
   enddo
   !
   COLL_element(i_ref)%N=ic
   !
   if (i_cycle==1) call COLLISIONS_alloc_and_free(what,"v1","alloc",i_ref)
   !
 enddo
 !
 if (IO_colls) then
   !
#if defined _PAR_IO
   call io_control(ACTION=WR,SEC=(/3,4/),ID=COLL_ID)
#else
   call io_control(ACTION=OP_APP_CL,SEC=(/2,3/),ID=COLL_ID)
#endif
   io_err=io_COLLISIONS(COLL_ID,COLL,COLL_element=COLL_element(1),i_coll=i_coll)
   !
   call COLLISIONS_alloc_and_free(what,"v1","clean",1)
   !
#if defined _PAR_IO
   if(i_coll==last_coll) then
     call io_control(ACTION=WR_CL,SEC=(/5/),ID=COLL_ID)
     io_err=io_COLLISIONS(COLL_ID,COLL,COLL_element=COLL_element(1),i_coll=i_coll)
   endif
#endif
   !
 endif
 !
 call timing('COLLISIONS_linearize_and_IO',OPR='stop')
 !
end subroutine COLLISIONS_linearize_and_IO

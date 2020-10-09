!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine COLLISIONS_linearize_and_IO(what,i_coll,COLL,COLL_element)
 !
 use pars,            ONLY:SP
 use parallel_m,      ONLY:master_cpu,PP_redux_wait
 use collision_ext,   ONLY:COLLISIONS_element,COLLISIONS_cutoff,COLLISIONS_group
 use IO_m,            ONLY:io_control,REP,OP_APP_CL,OP_WR_CL,io_COLLs
 use COLL_interfaces, ONLY:io_COLLISIONS
 use timing_m,        ONLY:timing
 !
 implicit none                                              
 !
 character(*)                            :: what
 integer,                  intent(in)    :: i_coll
 type(COLLISIONS_group),   intent(inout) :: COLL
 type(COLLISIONS_element), intent(inout) :: COLL_element(COLL%N)
 !
 ! Work Space
 !
 integer   :: ic,i1,i2,i3,i_cycle,io_err,i_ref,ID
 logical   :: store_it,l_HXC,l_GW_NEQ,l_QED_P,l_COH
 real(SP)  :: COLLISIONS_max
 !
 if (i_coll==0) then
   if(io_COLLs.and.master_cpu) then
     call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
     io_err=io_COLLISIONS(ID,COLL,COLL_element=COLL_element(1),i_coll=i_coll)
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
         !
         if (l_GW_NEQ .or. l_QED_P ) store_it=.TRUE.
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
   call io_control(ACTION=OP_APP_CL,SEC=(/2,3/),ID=ID)
   io_err=io_COLLISIONS(ID,COLL,COLL_element=COLL_element(1),i_coll=i_coll)
   !
   call COLLISIONS_alloc_and_free(what,"v1","clean",1)
   !
 endif
 !
 call timing('COLLISIONS_linearize_and_IO',OPR='stop')
 !
end subroutine COLLISIONS_linearize_and_IO

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
integer function io_RT_components(what,ID)
 !
 ! What can be E WF V JPSM G
 !
 use pars,           ONLY:SP,lchlen,schlen
 use drivers,        ONLY:l_elph_scatt,l_real_time
 use R_lattice,      ONLY:nqbz
 use stderr,         ONLY:intc
 use electrons,      ONLY:n_spin
 use IO_m,           ONLY:io_connect,io_disconnect,io_header,io_elemental,io_status,        &
&                         io_sec,io_bulk,read_is_on,io_fragment, &
&                         manage_OP_IF_START_and_CL_IF_END,&
&                         frag_RT
 use real_time,      ONLY:NE_i_time,REF_lifetime,RT_step,l_NE_with_fields,NE_steps,RT_nk, &
&                         dG_lesser,RT_carriers,RT_obj_alloc,&
&                         G_lesser_reference,G_MEM_steps,THETA_matrix,q_weight_RT,&
&                         RT_bands,RT_nbands,MEM_pointer,RT_init_occupation_reference
 use RT_control,     ONLY:J_cache,P_cache,M_cache,JPC_RT_IO_t,RT_carriers_object,RT_IO_t,&
&                         GF_RT_IO_t,TIME_string,IO_JPSM_cache_steps,SAVE_G_history
 use fields,         ONLY:A_vecpot
#if defined _ELPH
 use ELPH,           ONLY:ph_modes,elph_branches
#endif
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#include<memory.h>
 !
 integer,      intent(in) :: ID
 character(*), intent(in) :: what
 !
 ! Work Space
 !
 integer, external     :: variables_RT
 character(lchlen)     :: local_description(100),db_name
 character(schlen)     :: what_
 !
 integer               :: nbands,ik,i_spin,i_memory,i_time,db_type
 logical               :: l_G,l_JPSM,l_THETA,l_REF,l_carriers,l_track_history
 !
 ! DB kind and name
 !==================
 l_carriers   =what=="carriers"
 l_THETA      =what=="THETA"
 l_JPSM       =what=="JPSM"
 l_G          =what=="G"
 l_REF        =what=="REF"
 !
#if defined _TIMING
 call timing('io_RT_components',OPR='start')
#endif
 !
 db_name="RT_"//trim(what)
 if (l_REF) db_name="RT_reference_components"
 !
 what_=what
 db_type=2
 if (.not.any((/l_carriers,l_THETA,l_JPSM,l_G,l_REF/))) then
   db_name=trim(what)
   what_='carriers'
   l_carriers=.TRUE.
   db_type=-3
 endif
 !
 if (l_THETA.or.l_G) ik = maxval(io_sec(ID,:))-1
 !
 ! ACTION
 !========
 !
 if (l_JPSM.or.l_carriers) call manage_OP_IF_START_and_CL_IF_END(ID,NE_i_time,NE_steps)
 if (l_G   .or.l_THETA   ) call manage_OP_IF_START_and_CL_IF_END(ID,ik,RT_nk)
 !
 l_track_history=.false.
 if (l_JPSM)     l_track_history=io_sec(ID,1)> 1
 if (l_carriers) l_track_history=io_sec(ID,1)> 1
 if (l_G)        l_track_history=io_sec(ID,1)> 1 .and. SAVE_G_history
 !
 if (l_track_history) db_name=trim(db_name)//"_Time_"//trim(TIME_string( (NE_i_time-1)*RT_step))
 !
 io_RT_components=io_connect(desc=trim(db_name),type=db_type,ID=ID)
 if (io_RT_components/=0) goto 1
 !
 ! 1st SECTION
 !=============
 if (any((/io_sec(ID,:)==1/))) then
   !
   if ( db_type==2 .or. (db_type==-3.and..not.l_real_time) ) then
     !
     io_RT_components=io_header(ID,IMPOSE_SN=(.not.l_carriers),CUTOFF=.true.,T_EL=.true.,FRAG=frag_RT)
     if (io_RT_components/=0) goto 1
     !
     io_RT_components=variables_RT(ID,local_description,trim(what_))
     if (io_RT_components/=0) goto 1
     !
   endif
   !
   !
   if (l_G) then
     call io_elemental(ID,VAR="Save_G_history",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,L0=SAVE_G_history)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     call io_bulk(ID,VAR="MEMORY_TIME_POINTS",VAR_SZ=(/G_MEM_steps/))
     call io_bulk(ID,I1=MEM_pointer)
   endif
   !
   ! I/O time points
   !
   if ((l_JPSM.or.l_carriers).and.(db_type==2.or..not.l_real_time)) call LOCAL_IO(JPC_RT_IO_t)
   !
   if (l_G)                                                         call LOCAL_IO(GF_RT_IO_t)
   !
   io_RT_components=io_status(ID)
   if (io_RT_components/=0) goto 1
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)>1/))) goto 1
 !
 ! === no K no Time ===
 !#####################
 !
 !
 ! Current and Polarization
 !==========================
 if ( l_JPSM) then
   !        
   call io_elemental(ID,VAR="JPSM_CACHE_TIME_N_points",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,I0=IO_JPSM_cache_steps)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_bulk(ID,VAR="Current_TIME"//trim(TIME_string( (NE_i_time-1)*RT_step )),VAR_SZ=(/6,IO_JPSM_cache_steps/))
   call io_bulk(ID,R2=J_cache)
   !
   call io_bulk(ID,VAR="Polarization_TIME"//trim(TIME_string( (NE_i_time-1)*RT_step )),VAR_SZ=(/6,IO_JPSM_cache_steps/))
   call io_bulk(ID,R2=P_cache)
   !
   if(n_spin>1) then
     call io_bulk(ID,VAR="Magnetization_TIME"//trim(TIME_string( (NE_i_time-1)*RT_step )),VAR_SZ=(/6,IO_JPSM_cache_steps/))
     call io_bulk(ID,R2=M_cache)
   endif
   !
 endif       
 !
1 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_RT_components',OPR='stop')
#endif
 !
 contains
   !
   subroutine LOCAL_IO(TYP)
     type(RT_IO_t) :: TYP
     !
     call io_elemental(ID,VAR="IO_TIME_N_points",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,I0=TYP%N)
     call io_elemental(ID,VAR="IO_TIME_LAST_POINT",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,I0=TYP%last_point)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     if (.not.allocated(TYP%Time)) then
       YAMBO_ALLOC(TYP%Time,(NE_steps))
     endif
     !
     call io_bulk(ID,VAR="IO_TIME_points",VAR_SZ=(/TYP%N/))
     call io_bulk(ID,R1=TYP%Time(:TYP%N))
     !
   end subroutine
   !
end function

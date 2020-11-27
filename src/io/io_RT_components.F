!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
 ! What can be E WF V OBSERV(J,P,M,...) G
 !
 ! def/io_variable_bulk VAR_ID table
 ! VAR_ID=1 is used by io_bulk and should never be used by def/io_variable_bulk
 !
 ! ndb.RT_References
 !  3 Q_WEIGHTS,
 !  4 G_lesser_reference,
 !  10 - [..] REF_lifetimes
 !
 ! ndb.RT_carriers
 !  3 IO_TIME_points
 !  4 RT_kpt,  
 !  5 RT_k_weight,  
 !  6 RT_table,
 !  7 - 9 ref_io,
 !  10 - [..] RT_carriers_objects,
 !
 ! ndb.RT_G
 !  2 MEMORY_TIME_POINTs,
 !  3 IO_TIME_points,  
 !  4 dG_lesser, 
 !  6 H_rotation
 !
 ! ndb.RT_Obs
 !  3 IO_TIME_points,
 !  4 Current,  
 !  5 Polarization,  
 !  6 Spin_magnetization,  
 !  7 Orbital Magnetization,  
 !  8 A_tot
 !
 ! ndb.RT_THETA
 !  3 IO_TIME_points,  
 !  2 MEMORY_TIME_POINTs,
 !  3 Q_weight,  
 !  4 THETA
 !
 !
 use pars,           ONLY:SP,lchlen
 use drivers,        ONLY:l_real_time
 use R_lattice,      ONLY:nqbz
 use stderr,         ONLY:intc
 use timing_m,       ONLY:timing
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,read_is_on,io_fragment,write_is_on,&
&                         def_variable_bulk,io_variable_bulk,io_file
 use real_time,      ONLY:NE_i_time,RT_step,RT_dyn_step, &
&                         RT_carriers,RT_nbands,G_MEM_steps
 use RT_control,     ONLY:RT_carriers_object,CARR_RT_IO_t,GF_RT_IO_t,OBS_RT_IO_t,CACHE_OBS_steps_now,&
&                         SAVE_G_history,CACHE_OBS_steps,RT_DB_file_name
#if defined _PAR_IO
 use parallel_m,     ONLY:PAR_COM_Xk_ibz_INDEX,mpi_comm_world
#endif
#if defined _ELPH_ITERATIVE
 use RT_iterative,   ONLY:THETA_matrix
#endif
#if defined _ELPH
 use ELPH,           ONLY:ph_modes
#endif
 !
 implicit none
 !
 integer,      intent(in) :: ID
 character(*), intent(in) :: what
 !
 integer, external     :: variables_RT_dynamic
 !
 ! Work Space
 !
 character(lchlen)     :: db_name
 !
#if defined _PAR_IO
 integer               :: local_COMM
#endif
 integer               :: ik,db_type,T_size,G_IO_steps
 logical               :: l_G,l_OBS,l_THETA,l_REF,l_carriers,l_JP,l_M,l_D,&
&                         loading_carr_before_neq,par_IO
 !
 ! DB kind and name...
 !=====================
 l_carriers   =what=="carriers"
 l_THETA      =what=="THETA"
 l_G          =what=="G"
 l_REF        =what=="REF"
 !
 !...OBSERVABLES
 !===============
 l_JP         =what=="JP".or.what=="OBSERVABLES"
 l_M          =what=="M".or.what=="OBSERVABLES"
 l_D          =what=="D".or.what=="OBSERVABLES"
 l_OBS        =l_JP.or.l_M.or.l_D
 !
 call timing('io_RT_components',OPR='start')
 !
 db_name="RT_"//trim(what)
 if (l_OBS)    db_name="RT_OBSERVABLES"
 if (l_REF)    db_name="RT_reference_components"
 !
 db_type=2
 if (.not.any((/l_OBS,l_carriers,l_THETA,l_G,l_REF/))) then
   db_name=trim(what)
   l_carriers=.TRUE.
   db_type=-3
 endif
 !
 loading_carr_before_neq = (db_type==-3.and.l_real_time) 
 !
 par_IO=.false.
#if defined _PAR_IO
 if (l_G) then
   if (write_is_on(ID)) local_COMM=PAR_COM_Xk_ibz_INDEX%COMM
   if ( read_is_on(ID)) local_COMM=mpi_comm_world
   par_IO=.true.
 endif
#endif
 !
 ! ACTION
 !========
 !
 if(l_G.and.any((/io_sec(ID,:)>=3/))) then
#if defined _PAR_IO
   io_RT_components=io_connect(desc=trim(db_name)//"_PAR",type=db_type,ID=ID,COMM=local_COMM)
#else
   io_RT_components=io_connect(desc=trim(db_name),type=db_type,ID=ID,FORCE_READ_MODE=.true.)
#endif
   if (io_RT_components/=0) goto 1
 else
   io_RT_components=io_connect(desc=trim(db_name),type=db_type,ID=ID)
   if (io_RT_components/=0) goto 1
 endif
 !
 if (read_is_on(ID)) then
   if (l_OBS     ) db_type=4
   if (l_G       ) db_type=5
   if (l_carriers) db_type=6
   if (l_THETA   ) db_type=7
   if (l_REF     ) db_type=8
   RT_DB_file_name(db_type)=io_file(ID)
 endif
 !
 ! 1st SECTION
 !=============
 if (any((/io_sec(ID,:)==1/))) then
   !
   call io_RT_components_header(ID,what,loading_carr_before_neq,l_carriers,l_OBS,l_G,l_THETA,l_REF,io_RT_components)
   if (io_RT_components/=0) goto 1
   !
   if (l_OBS)  call io_RT_components_OBS(ID,l_JP,l_M,"DEF")
   !
   if ( l_carriers .and. .not.loading_carr_before_neq) call RT_carriers_object(RT_carriers,DB_id=ID,WHAT="ref_io")
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)>1/))) goto 1
 !
 ! REFERENCE Lifetimes and Green function
 !========================================
 if ( l_REF ) call io_RT_components_Reference(ID)
 !
 ! 2nd SECTION
 !=============
 !
 ! Time size
 !
 T_size=1
 if ( (write_is_on(ID) .or. l_real_time) .and. l_G ) then
   G_IO_steps=GF_RT_IO_t%INTERVAL_steps/nint(RT_dyn_step/RT_step)
   if(NE_i_time> 1 .and. .not.SAVE_G_history) T_size=    G_MEM_steps
   if(NE_i_time> 1 .and.      SAVE_G_history) T_size=min(G_MEM_steps,G_IO_steps)
 endif
 if(l_OBS .and. NE_i_time> 1) T_size=CACHE_OBS_steps_now
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   if(any((/l_OBS,l_G,l_carriers/)) .and. .not.loading_carr_before_neq ) then
     io_RT_components=variables_RT_dynamic(ID)
     if (io_RT_components/=0) goto 1
   endif
   !
   if (l_G)        call io_RT_components_TIME_points(ID,T_size,.false.,SAVE_G_history,l_G,GF_RT_IO_t)
   !
   if (l_carriers) call io_RT_components_TIME_points(ID,1,.not.loading_carr_before_neq,.true.,l_G,CARR_RT_IO_t)
   !
   if (l_OBS)      call io_RT_components_TIME_points(ID,T_size,.false.,.true.,l_G,OBS_RT_IO_t)
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)>2/))) goto 1
 !
 ! 3rd SECTION
 !=============
 !
 ! GPL_EXCLUDE_START
 !
#if !defined _PAR_IO
 !
 ! === deal with K-fragments ===
 !
 if (l_THETA.or.l_G) then
   ik = max(1,maxval(io_sec(ID,:))-2)
   call io_fragment(ID,i_fragment=ik)
 endif
 !
#endif
 !
#if defined _ELPH_ITERATIVE
 !
 ! ELPH Kernel THETA Matrix 
 !==========================
 if ( l_THETA ) then
   !
   ! This should be replaced by call THETA_IO
   !
   call def_variable_bulk(ID,"RT_THETA_K"//trim(intc(ik)),4,(/2,2*nqbz*ph_modes,RT_nbands,RT_nbands/),SP)
   call  io_variable_bulk(ID,4,C3=THETA_matrix(:,:,:,ik))
 endif
 !
#endif
 !
 ! lesser Green Function
 !=======================
 if ( l_G ) call io_RT_components_G_lesser(ID,T_size)
 !
 ! Carrier components
 !====================
 if ( l_carriers) then
   !
   if (CARR_RT_IO_t%first_IO(3).or.read_is_on(ID)) then
     call RT_carriers_object(RT_carriers,DB_id=ID,WHAT="def_io_variables")
   endif
   call RT_carriers_object(RT_carriers,DB_id=ID,WHAT="do_io_variables")
   if (read_is_on(ID)) call RT_carriers_object(RT_carriers,WHAT="dump")
   !
   if (write_is_on(ID)) CARR_RT_IO_t%first_IO(3)=.FALSE.
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 ! Current and Polarization
 !==========================
 if (l_OBS) call io_RT_components_OBS(ID,l_JP,l_M,"FILL")
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_RT_components',OPR='stop')
 !
end function

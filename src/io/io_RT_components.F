!
! License-Identifier: GPL
!
! Copyright (C) 2008 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
integer function io_RT_components(what,ID)
 !
 ! What can be E WF V OBSERV(J,P,M,...) G_lesser G_lesser_RESTART
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
 use pars,           ONLY:SP,schlen
 use drivers,        ONLY:l_real_time
 use R_lattice,      ONLY:nqbz
 use stderr,         ONLY:intc
 use timing_m,       ONLY:timing
 use IO_int,         ONLY:io_connect,io_disconnect,io_fragment,def_variable_bulk,io_variable_bulk
 use IO_m,           ONLY:io_sec,read_is_on,write_is_on,io_file
 use real_time,      ONLY:NE_i_time,RT_step,RT_dyn_step,RT_carriers,RT_nbands,G_MEM_steps
 use RT_control,     ONLY:RT_carriers_object,CARR_RT_IO_t,Gless_RESTART_RT_IO_t,OBS_RT_IO_t,CACHE_OBS_steps_now,&
&                         CACHE_OBS_steps,RT_DB_file_name,Gless_RT_IO_t,&
&                         RT_DB_name,RT_define_the_IO_DB_identifier,RT_do_IO,RT_current_DB_identifier,RT_return_db_ID
#if defined _PAR_IO
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
 character(*), intent(in) ::what
 integer,        optional ::ID
 !
 ! Work Space
 character(schlen)     ::db_name
 integer               ::ik,db_kind,T_size,G_IO_steps,ID_frag
 logical               ::loading_carr_before_neq
 integer, external     ::variables_RT_dynamic
 !
 call timing('io_RT_components',OPR='start')
 !
 ! Datbase Name and identifier 
 !=============================
 ! .. at this point it is crucial to define the RT_current_DB_identifier 
 !
 !... using what
 call RT_define_the_IO_DB_identifier(what)
 !
 !... carriers specific procedure (RT_apply related)
 db_kind=2
 if (.not.RT_do_IO("ANY")) then
   if (index(what,"_carriers")>0) then
     RT_current_DB_identifier=RT_return_db_ID("carriers")
     RT_DB_name(RT_current_DB_identifier)="carriers"
   endif
   db_kind=-3
 endif
 loading_carr_before_neq = (db_kind==-3.and.l_real_time) 
 !
 ! ACTION
 !========
 if (RT_do_IO("ANY_G")) then 
   io_RT_components=io_connect(desc="RT_"//trim(RT_DB_name(RT_current_DB_identifier)),type=db_kind,ID=ID)
   if (io_RT_components/=0) goto 1
 else
   if (db_kind>0) db_name="RT_"//trim(RT_DB_name(RT_current_DB_identifier))
   if (db_kind<0) db_name=trim(what)
   io_RT_components=io_connect(desc=trim(db_name),type=db_kind,ID=ID)
   if (io_RT_components/=0) goto 1
 endif
 !
 if (read_is_on(ID)) RT_DB_file_name(RT_current_DB_identifier)=io_file(ID)
 !
 ! 1st SECTION
 !=============
 if (any((/io_sec(ID,:)==1/)).and..not.RT_do_IO("ANY_K_section")) then
   !
   call io_RT_components_header(ID,what,loading_carr_before_neq,io_RT_components)
   if (io_RT_components/=0) goto 1
   !
   if (RT_do_IO("OBSERVABLES")) call io_RT_components_OBS(ID,"DEF")
   !
   if (RT_do_IO("carriers").and..not.loading_carr_before_neq) call RT_carriers_object(RT_carriers,DB_id=ID,WHAT="ref_io")
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)>1/))) goto 1
 !
 ! REFERENCE Lifetimes and Green function
 !========================================
 if (RT_do_IO("REFERENCE")) call io_RT_components_Reference(ID)
 !
 ! 2nd SECTION
 !=============
 !
 ! Time size
 !
 T_size=1
 if ((write_is_on(ID).or.l_real_time).and. RT_do_IO("ANY_G") ) then
   G_IO_steps=Gless_RESTART_RT_IO_t%INTERVAL_steps/nint(RT_dyn_step/RT_step)
   if(NE_i_time> 1 .and. (RT_do_IO("G_lesser_RESTART").or.RT_do_IO("G_lesser_RESTART_K_section"))) T_size= G_MEM_steps
   if(NE_i_time> 1 .and. (RT_do_IO("G_lesser")        .or.RT_do_IO("G_lesser_K_section")        )) T_size= 1
 endif
 if(RT_do_IO("OBSERVABLES") .and. NE_i_time> 1) T_size=CACHE_OBS_steps_now
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   if(RT_do_IO("DYN_VARS") .and. .not.loading_carr_before_neq ) then
     io_RT_components=variables_RT_dynamic(ID)
     if (io_RT_components/=0) goto 1
   endif
   !
   if (RT_do_IO("G_lesser"))          call io_RT_components_TIME_points(ID,T_size,.FALSE.,.TRUE.,Gless_RT_IO_t)
   !
   if (RT_do_IO("G_lesser_RESTART"))  call io_RT_components_TIME_points(ID,T_size,.FALSE.,.FALSE.,Gless_RESTART_RT_IO_t)
   !
   if (RT_do_IO("carriers"))          call io_RT_components_TIME_points(ID,1,.not.loading_carr_before_neq,.TRUE.,CARR_RT_IO_t)
   !
   if (RT_do_IO("OBSERVABLES"))       call io_RT_components_TIME_points(ID,T_size,.FALSE.,.TRUE.,OBS_RT_IO_t)
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)>2/))) goto 1
 !
 ! 3rd SECTION
 !=============
 if (RT_do_IO("K_SECTION")) then
#if defined _PAR_IO
   ID_frag=ID
#else
   ! === deal with K-fragments ===
   ik = max(1,maxval(io_sec(ID,:))-2)
   call io_fragment(ID,ID_frag,i_fragment=ik)
#endif
 endif
 !
#if defined _ELPH_ITERATIVE
 !
 ! ELPH Kernel THETA Matrix 
 !==========================
 if (RT_do_IO("THETA")) then
   !
   ! This should be replaced by call THETA_IO
   !
   call def_variable_bulk(ID_frag,"RT_THETA_K"//trim(intc(ik)),4,(/2,2*nqbz*ph_modes,RT_nbands,RT_nbands/),SP)
   call  io_variable_bulk(ID_frag,4,C3=THETA_matrix(:,:,:,ik))
 endif
 !
#endif
 !
 ! Lesser Green Function
 !=======================
 if (RT_do_IO("ANY_G")) call io_RT_components_G_lesser(ID_frag,T_size)
 !
 ! Carrier components
 !====================
 if (RT_do_IO("carriers")) then
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
 ! Current and Polarization
 !==========================
 if (RT_do_IO("OBSERVABLES")) call io_RT_components_OBS(ID,"FILL")
 !
 ! End
 !=====
#if !defined _PAR_IO
 if (RT_do_IO("K_SECTION")) call io_fragment_disconnect(ID,ID_frag)
#endif
1 call io_disconnect(ID=ID)
 call timing('io_RT_components',OPR='stop')
 !
end function

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
 ! VAR__ID=1 is used by io_bulk and should never be used by def/io_variable_bulk
 !
 ! ndb.RT_References
 ! 3 Q_WEIGHTS
 ! 4 G_lesser_reference
 ! 10 - 17 REF_lifetimes
 !
 ! ndb.RT_carriers
 ! 3 IO_TIME_points
 ! 4 RT_kpt,  5 RT_k_weight,  6 RT_table
 ! 7 - 9 ref_io 
 ! 10 - 21 RT_carriers_objects
 !
 ! ndb.RT_G
 ! 3 IO_TIME_points,  2 MEMORY_TIME_POINTs,
 ! 4 dG_lesser, 6 H_rotation
 !
 ! ndb.RT_Obs
 ! 3 IO_TIME_points
 ! 4 Current,  5 Polarization,  6 Spin_magnetization,  7 Orbital Magnetization,  8 A_tot
 !
 ! ndb.RT_THETA
 ! 3 IO_TIME_points,  2 MEMORY_TIME_POINTs,
 ! 3 Q_weight,  4 THETA
 !
 !
 use pars,           ONLY:SP,lchlen
 use drivers,        ONLY:l_real_time
 use R_lattice,      ONLY:nqbz
 use stderr,         ONLY:intc
 use timing_m,       ONLY:timing
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,read_is_on,io_fragment,write_is_on,&
&                         def_variable_bulk,io_variable_bulk
 use real_time,      ONLY:NE_i_time,RT_step,RT_dyn_step,NE_steps,RT_nk, &
&                         RT_carriers,RT_nbands,G_MEM_steps
 use RT_control,     ONLY:RT_carriers_object,CARR_RT_IO_t,GF_RT_IO_t,OBS_RT_IO_t,CACHE_OBS_steps_now,&
&                         NE_i_start_time,SAVE_G_history
#if defined _PAR_IO
 use parallel_m,     ONLY:PAR_COM_Xk_ibz_INDEX,mpi_comm_world,myid
#endif
#if defined _ELPH
 use real_time,      ONLY:THETA_matrix
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
 integer               :: ik,db_type,T_size,i_start,G_IO_steps
 logical               :: l_G,l_OBS,l_THETA,l_REF,l_carriers,l_JP,l_M,l_D,&
&                         loading_carr_before_neq,par_IO,first_IO
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
 ! The restart time is the last time I wrote G and eequal or before the last time I wrote OBS
 if(l_G       ) first_IO=(NE_i_time==  GF_RT_IO_t%last_point).or.(NE_i_time==1).or.  GF_RT_IO_t%last_point< 0
 if(l_OBS     ) first_IO=(NE_i_time== OBS_RT_IO_t%last_point).or.(NE_i_time==1).or. OBS_RT_IO_t%last_point< 0
 ! it is possible that the last time I wrote the carriers was before the restart time of the run
 if(l_carriers) first_IO=(NE_i_time== OBS_RT_IO_t%last_point).or.(NE_i_time==1).or. OBS_RT_IO_t%last_point< 0 .or. &
 &                       (NE_i_time-NE_i_start_time)<=CARR_RT_IO_t%INTERVAL_steps ! (NE_i_time==CARR_RT_IO_t%last_point).or.(NE_i_time==1).or.CARR_RT_IO_t%last_point< 0
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
 ! 1st SECTION
 !=============
 if (any((/io_sec(ID,:)==1/))) then
   !
   call RT_header(ID,what,loading_carr_before_neq,l_carriers,l_OBS,l_G,l_THETA,l_REF,io_RT_components)
   if (io_RT_components/=0) goto 1
   !
   if (l_OBS)  call RT_OBS_IO(ID,l_JP,l_M,"DEF")
   !
   if ( l_carriers .and. .not.loading_carr_before_neq) call RT_carriers_object(RT_carriers,DB_id=ID,WHAT="ref_io")
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)>1/))) goto 1
 !
 ! REFERENCE Lifetimes and Green function
 !========================================
 if ( l_REF ) call Reference_IO(ID)
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
#if defined _PAR_IO
   if(l_G)        call TIME_points_IO(ID,T_size,first_IO,.false.,SAVE_G_history,l_G,GF_RT_IO_t)
#else
   if(l_G)        call TIME_points_IO(ID,T_size,.true.,.false.,SAVE_G_history,l_G,GF_RT_IO_t)
#endif
   if(l_carriers) call TIME_points_IO(ID,1,first_IO,.not.loading_carr_before_neq,.true.,l_G,CARR_RT_IO_t)
   !
   if(l_OBS)      call TIME_points_IO(ID,T_size,first_IO,.false.,.true.,l_G,OBS_RT_IO_t)
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
#if defined _ELPH
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
 if ( l_G ) call G_lesser_IO(ID,T_size)
 !
 ! Carrier components
 !====================
 if ( l_carriers) then
   if (first_IO.or.read_is_on(ID)) &
   &  call RT_carriers_object(RT_carriers,DB_id=ID,WHAT="def_io_variables")
   call RT_carriers_object(RT_carriers,DB_id=ID,WHAT="do_io_variables")
   if (read_is_on(ID)) call RT_carriers_object(RT_carriers,WHAT="dump")
 endif
 !
 ! GPL_EXCLUDE_END
 !
 ! Current and Polarization
 !==========================
 if (l_OBS) call RT_OBS_IO(ID,l_JP,l_M,"FILL")
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_RT_components',OPR='stop')
 !
end function
!
! RT header
!=============
!
subroutine RT_header(ID,what,loading_carr_before_neq,l_carriers,l_OBS,l_G,l_THETA,l_REF,io_RT_components)
 !
 use pars,           ONLY:IP,LP,SP,lchlen,schlen
 use drivers,        ONLY:l_real_time
 use R_lattice,      ONLY:nqbz
 use IO_m,           ONLY:io_header,io_status,frag_RT,read_is_on,&
&                         def_variable_elemental,io_variable_elemental,&
&                         def_variable_bulk,io_variable_bulk
 use real_time,      ONLY:q_weight_RT,RT_carriers,G_MEM_steps,RT_obj_alloc
 use RT_control,     ONLY:SAVE_G_history
 !
 implicit none
 !
 character(*), intent(in) :: what
 integer, intent(in)      :: ID
 logical, intent(in)      :: loading_carr_before_neq,l_carriers,l_OBS,l_G,l_THETA,l_REF
 integer, intent(out)     :: io_RT_components
 !
 integer, external     :: variables_RT_static
 !
 character(schlen)     :: what_
 !
 if ( .not. loading_carr_before_neq ) then
   !
   what_=what
   if (l_OBS) then
     what_='OBSERVABLES'
   else if (.not.any((/l_carriers,l_THETA,l_G,l_REF/))) then
     what_='carriers'
   endif
   !
   io_RT_components=io_header(ID,IMPOSE_SN=(.not.l_carriers),CUTOFF=.true.,T_EL=.true.,FRAG=frag_RT)
   if (io_RT_components/=0) return
   !
   io_RT_components=variables_RT_static(ID,trim(what_))
   if (io_RT_components/=0) return
   !
 endif
 !
 ! GPL_EXCLUDE_START
 !
 if (l_THETA) then
   call def_variable_bulk(ID,"Q_weight",3,(/nqbz/),SP)
   call  io_variable_bulk(ID,3,R1=q_weight_RT(:,1,1))
 endif
 !
 ! Here I dump all RELEVANT RT_carriers% components
 ! that make the RT_carriers databases independent
 !
 if (l_carriers) then
   call def_variable_elemental(ID,"RT_carriers_dimensions",4,IP+100,0) 
   call  io_variable_elemental(ID,I0=RT_carriers%nk)
   call  io_variable_elemental(ID,I1=RT_carriers%nb)
   call  io_variable_elemental(ID,I0=RT_carriers%nstates)
   !call  io_variable_elemental(ID,"",0,0)
   !
   if (read_is_on(ID)) call RT_obj_alloc(RT_carriers)
   !
   call def_variable_bulk(ID,"RT_kpt",4,(/RT_carriers%nk,3/),SP)
   call  io_variable_bulk(ID,4,R2=RT_carriers%kpt)
   call def_variable_bulk(ID,"RT_k_weight",5,(/RT_carriers%nk/),SP)
   call  io_variable_bulk(ID,5,R1=RT_carriers%k_weight)
   call def_variable_bulk(ID,"RT_table",6,(/RT_carriers%nstates,4/),IP+100)
   call  io_variable_bulk(ID,6,I2=RT_carriers%table)
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 if (l_G) then
   call def_variable_elemental(ID,"Save_G_history",1,LP+100,0) 
   call  io_variable_elemental(ID,L0=SAVE_G_history)
   !call io_variable_elemental(ID,"",0,0)
 endif
 !
 io_RT_components=io_status(ID)
 !
end subroutine RT_header
!
!
! Time Points
!=============
!
subroutine TIME_points_IO(ID,VAR_SZ,def_var,load_rest_info,unlim,l_G,TYP)
 !
 use pars,           ONLY:IP,SP,schlen
 use drivers,        ONLY:l_real_time
 use units,          ONLY:AUT2FS
 use RT_output_m,    ONLY:RT_desc
 use RT_control,     ONLY:RT_IO_t
 use real_time,      ONLY:NE_steps,MEM_pointer,G_MEM_steps,NE_i_time,i_MEM_now,i_MEM_prev,i_MEM_old,&
&                         RT_dyn_step,RT_step
 use IO_m,           ONLY:def_variable_bulk,io_variable_bulk,def_variable_elemental,io_variable_elemental,&
&                         NF90_unlimited,&
&                         write_is_on,read_is_on
 !
#include<memory.h>
 !
 integer,       intent(in)    :: ID,VAR_SZ
 logical,       intent(in)    :: def_var,load_rest_info,unlim,l_G
 type(RT_IO_t), intent(inout) :: TYP
 !
 integer                      :: IPOS(1),IO_LIM(2),i_min_index,i_mem
 character(schlen)            :: dim_name(1)
 !
 if(write_is_on(ID)) TYP%last_point=NE_i_time
 !
 ! ... for RESTART purposes...
 if(load_rest_info) then
   call def_variable_elemental(ID,"IO_TIMEs",2,SP,0) 
   call io_variable_elemental(ID,VAR=" [RT] I/O time          (input) [fs]",&
                        R0=TYP%INTERVAL_time_SAVE,CHECK=.TRUE.,OP=(/"=="/),  &
&                       DESCRIPTOR=RT_desc,UNIT=AUT2FS)
   call io_variable_elemental(ID,VAR=" [RT] I/O time            (now) [fs]",&
                        R0=TYP%INTERVAL_time     ,CHECK=.FALSE.,             &
&                       DESCRIPTOR=RT_desc,UNIT=AUT2FS)
 endif
 !
 !
 call def_variable_elemental(ID,"IO_TIME_steps_last_nsteps",3,IP+100,0) 
 call io_variable_elemental(ID,I0=TYP%INTERVAL_steps)
 call io_variable_elemental(ID,I0=TYP%last_point)
 call io_variable_elemental(ID,I0=TYP%N)
 !
 if (l_G .and. l_real_time) then
   if(def_var.or.read_is_on(ID)) then
     dim_name="G_MEM_steps"
     call def_variable_bulk(ID,"MEMORY_TIME_POINTS",2,(/G_MEM_steps/),IP+100,dim_name,silent=.true.)
   endif
   call io_variable_bulk(ID,2,I1=MEM_pointer)
   if (read_is_on(ID)) then
     i_min_index=minval(MEM_pointer)
     do i_mem=1,G_MEM_steps
       if (MEM_pointer(i_mem)==NE_i_time  )                         i_MEM_now =i_mem
       if (MEM_pointer(i_mem)==NE_i_time-nint(RT_dyn_step/RT_step)) i_MEM_prev=i_mem
       if (G_MEM_steps==2) cycle
       if (MEM_pointer(i_mem)==i_min_index)                         i_MEM_old =i_mem
     enddo
   endif
 endif
 !
 if (.not.allocated(TYP%Time)) then
   YAMBO_ALLOC(TYP%Time,(TYP%N))
 endif
 !
 IPOS=TYP%N-VAR_SZ+1
 IO_LIM=(/TYP%N-VAR_SZ+1,TYP%N/)
 if(def_var.or.read_is_on(ID)) then
   dim_name="IO_TIME_n_points"
   if(     unlim) call def_variable_bulk(ID,"IO_TIME_points",3,(/NF90_unlimited/),SP,dim_name,silent=.true.)
   if(.not.unlim) call def_variable_bulk(ID,"IO_TIME_points",3,(/G_MEM_steps/),   SP,dim_name,silent=.true.)
 endif
 !
 if(write_is_on(ID)) call io_variable_bulk(ID,3,R1=TYP%Time(IO_LIM(1):IO_LIM(2)),IPOS=IPOS)
 if( read_is_on(ID)) call io_variable_bulk(ID,3,R1=TYP%Time(:TYP%N))
 !
end subroutine TIME_points_IO
!
!
! lesser Green Function
!=======================
!
subroutine G_lesser_IO(ID,T_size)
 !
 use pars,           ONLY:SP,schlen
 use drivers,        ONLY:l_real_time
 use stderr,         ONLY:intc
 use IO_m,           ONLY:def_variable_bulk,io_variable_bulk,&
&                         variable_exist,write_is_on,io_sec
 use hamiltonian,    ONLY:H_rotation
 use RT_control,     ONLY:GF_RT_IO_t,SAVE_G_history,NE_i_start_time
 use real_time,      ONLY:dG_lesser,G_MEM_steps,RT_bands,RT_nbands,RT_nk,l_RT_rotate_DM,&
&                         NE_i_time,RT_dyn_step,RT_step,i_MEM_now,i_MEM_prev,i_MEM_old
 use netcdf,         ONLY:NF90_UNLIMITED
#if defined _PAR_IO
 use parallel_m,     ONLY:PAR_G_kpts
#endif
 !
 implicit none
 !
 integer, intent(in)  :: ID,T_size
 !
 logical           :: l_H_rot
 integer           :: ib,ik,i_sp_pol,i_time,i_mem,i_mem_sorted
 integer           :: VAR_SZ(5),IPOS(5)
 real(SP)          :: time(2)
 character(schlen) :: dim_names(5),var_name
 !
 i_sp_pol=1
 !
 VAR_SZ(1:3)=(/2,RT_nbands,RT_nbands/)
 dim_names(1:3)  =[character(schlen) :: 'complex', 'RT_nbands','RT_nbands']
 IPOS(1:3)=(/1,1,1/)
 !
 if(.not.SAVE_G_history) then
   VAR_SZ(5)    = G_MEM_steps
   dim_names(5) ='G_MEM_steps'
 else
   VAR_SZ(5)    = NF90_UNLIMITED
   dim_names(5) ='G_IO_time_step'
 endif
 !
#if defined _PAR_IO
 !
 VAR_SZ(4)    = RT_nk
 dim_names(4) ='RT_nk'
 !
 l_H_rot=( l_RT_rotate_DM .and. (variable_exist(ID,"H_rotation").or.write_is_on(ID)) )
 !
 if(NE_i_time==NE_i_start_time.or..not.l_real_time) then
   call def_variable_bulk(ID,"dG_lesser", 4,VAR_sz,SP,dim_names=dim_names,par_io_kind='collective',silent=.true.)
   if(l_H_rot) call def_variable_bulk(ID,"H_rotation",6,VAR_sz(1:4),SP,&
   &                                             dim_names=dim_names(1:4),par_io_kind='collective',silent=.true.)
 endif
 !
 IPOS(4)=PAR_G_kpts(1)
 if(l_real_time) then
   do i_mem_sorted=G_MEM_steps-T_size+1,G_MEM_steps
     if(     SAVE_G_history) IPOS(5)=GF_RT_IO_t%N+(i_mem_sorted-G_MEM_steps)
     if(.not.SAVE_G_history) IPOS(5)=      T_size+(i_mem_sorted-G_MEM_steps)
     !i_mem=mod(i_MEM_now+i_mem_sorted-1,G_MEM_steps)+1
     if(i_mem_sorted==G_MEM_steps  ) i_mem=i_MEM_now
     if(i_mem_sorted==G_MEM_steps-1) i_mem=i_MEM_prev
     if(i_mem_sorted==G_MEM_steps-2) i_mem=i_MEM_old
     call io_variable_bulk(ID,4,C4=dG_lesser(:,:,PAR_G_kpts(1):PAR_G_kpts(2),i_mem:i_mem),IPOS=IPOS)
   enddo
 else
   IPOS(5)=GF_RT_IO_t%N
   call io_variable_bulk(ID,4,C4=dG_lesser(:,:,PAR_G_kpts(1):PAR_G_kpts(2),1:1),IPOS=IPOS)
 endif
 if(l_H_rot) call io_variable_bulk(ID,6,C3=H_rotation(:,:,PAR_G_kpts(1):PAR_G_kpts(2),i_sp_pol),IPOS=IPOS(1:4))
 !
#else
 !
 ik = maxval(io_sec(ID,:))-2
 !
 VAR_SZ(4)    = 1
 dim_names(4) ='k_fragment'
 IPOS(4)      = 1
 !
 time(1)=(NE_i_time-(T_size-1)*nint(RT_dyn_step/RT_step))*RT_step
 time(2)=(NE_i_time-1)*RT_step
 !
 var_name="dG_lesser_K"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol))
 call def_variable_bulk(ID,trim(var_name),4,VAR_SZ,SP,dim_names=dim_names,silent=.true.)
 !
 if(l_real_time) then
   do i_mem_sorted=G_MEM_steps-T_size+1,G_MEM_steps
     if(     SAVE_G_history) IPOS(5)=GF_RT_IO_t%N+(i_mem_sorted-G_MEM_steps)
     if(.not.SAVE_G_history) IPOS(5)=      T_size+(i_mem_sorted-G_MEM_steps)
     !i_mem=mod(i_MEM_now+i_mem_sorted-1,G_MEM_steps)+1
     if(i_mem_sorted==G_MEM_steps  ) i_mem=i_MEM_now
     if(i_mem_sorted==G_MEM_steps-1) i_mem=i_MEM_prev
     if(i_mem_sorted==G_MEM_steps-2) i_mem=i_MEM_old
     call io_variable_bulk(ID,4,C4=dG_lesser(:,:,ik:ik,i_mem:i_mem),IPOS=IPOS)
   enddo
 else
   IPOS(5)=GF_RT_IO_t%N
   call io_variable_bulk(ID,4,C4=dG_lesser(:,:,ik:ik,1:1),IPOS=IPOS)
 endif
 !
 var_name="H_rotation_K"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol))
 l_H_rot=( l_RT_rotate_DM .and. (variable_exist(ID,trim(var_name)).or.write_is_on(ID)) )
 !
 if ( l_H_rot ) then
   call def_variable_bulk(ID,trim(var_name),6,VAR_SZ(1:3),SP,dim_names=dim_names(1:3))
   call  io_variable_bulk(ID,6,C2=H_rotation(:,:,ik,i_sp_pol))
 endif
 !
#endif
 !
end subroutine G_lesser_IO
!
!
! Current and Polarization
!==========================
!
subroutine RT_OBS_IO(ID,l_JP,l_M,what)
 !
 use pars,        ONLY:SP,schlen
 use IO_m,        ONLY:io_variable_bulk,def_variable_bulk,NF90_unlimited,&
&                      read_is_on,write_is_on
 use electrons,   ONLY:n_spin
 use real_time,   ONLY:l_NE_with_fields,NE_i_time
 use RT_control,  ONLY:J_cache,P_cache,Ms_cache,Ml_cache,A_cache,&
&                      CACHE_OBS_steps_now,OBS_RT_IO_t
 !
#include<memory.h>
 !
 integer,      intent(in) :: ID
 logical,      intent(in) :: l_JP,l_M
 character(*), intent(in) :: what
 !
 integer           :: VAR_SZ(4),CACHE_SZ,IPOS
 character(schlen) :: dim_names(4)
 !
 select case(WHAT)
   !
 case('DEF')
   !
   if (l_JP) then
     VAR_SZ(1:3)    = (/2,3,NF90_UNLIMITED/)
     dim_names(1:3) = [character(schlen) :: 'complex', 'xyz','time']
     call def_variable_bulk(ID,"Current",     4,VAR_SZ(1:3),SP,dim_names=dim_names(1:3),silent=.true.)
     call def_variable_bulk(ID,"Polarization",5,VAR_SZ(1:3),SP,dim_names=dim_names(1:3),silent=.true.)
   endif
   !
   if(n_spin>1.and.l_M) then
     VAR_SZ    = (/2,3,3,NF90_UNLIMITED/)
     dim_names = [character(schlen) :: 'complex', 'xyz','tot_c_v','time']
     call def_variable_bulk(ID,"Spin_Magnetization",6,VAR_SZ,SP,dim_names=dim_names,silent=.true.)
     !
     VAR_SZ    = (/2,3,2,NF90_UNLIMITED/)
     dim_names = [character(schlen) :: 'complex', 'xyz','orb_itin','time']
     call def_variable_bulk(ID,"Orbital_Magnetization",7,VAR_SZ,SP,dim_names=dim_names,silent=.true.)
   endif
   !
   if(l_NE_with_fields) then
     VAR_SZ    = (/2,3,3,NF90_UNLIMITED/)
     dim_names = [character(schlen) :: 'complex', 'xyz','A_kind','time']
     call def_variable_bulk(ID,"A_tot",8,VAR_SZ,SP,dim_names=dim_names,silent=.true.)
   endif
   !
 case('FILL')
   !
   if(write_is_on(ID)) then
     if(NE_i_time==1) CACHE_SZ=1
     if(NE_i_time> 1) CACHE_SZ=CACHE_OBS_steps_now
     IPOS    =OBS_RT_IO_t%N-CACHE_OBS_steps_now+1
   else if(read_is_on(ID)) then
     CACHE_SZ=CACHE_OBS_steps_now
     IPOS    =1
   endif
   !
   if (l_JP) then
     call io_variable_bulk(ID,4,C2=J_cache(:,1:CACHE_SZ),IPOS=(/1,1,IPOS/))
     call io_variable_bulk(ID,5,C2=P_cache(:,1:CACHE_SZ),IPOS=(/1,1,IPOS/))
   endif
   !
   if(n_spin>1.and.l_M) then
     call io_variable_bulk(ID,6,C3=Ms_cache(:,:,1:CACHE_SZ),IPOS=(/1,1,1,IPOS/))
     call io_variable_bulk(ID,7,C3=Ml_cache(:,:,1:CACHE_SZ),IPOS=(/1,1,1,IPOS/))
   endif
   !
   if(l_NE_with_fields) then
     call io_variable_bulk(ID,8,C3=A_cache(:,:,1:CACHE_SZ),IPOS=(/1,1,1,IPOS/))
   endif
   !
 end select
 !
end subroutine
!
!
! REFERENCE Lifetimes and Green function
!========================================
!
subroutine Reference_IO(ID)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elph_scatt
 use R_lattice,      ONLY:nqbz
 use IO_m,           ONLY:def_variable_bulk,io_variable_bulk,read_is_on
 use real_time,      ONLY:RT_init_occupation_reference,q_weight_RT,REF_lifetime,&
&                         G_lesser_reference,RT_nk,RT_nbands
#if defined _ELPH
 use ELPH,           ONLY:elph_branches
#endif

 !
#include<memory.h>
 !
 integer, intent(in)  :: ID
 !
#if defined _ELPH
 !
 if (l_elph_scatt) then
   !
   if (read_is_on(ID)) then
     YAMBO_FREE(q_weight_RT)
     YAMBO_ALLOC(q_weight_RT,(nqbz,elph_branches(1):elph_branches(2),2))
   endif
   !
   call def_variable_bulk(ID,"Q_WEIGHT",3,(/nqbz,elph_branches(2)-elph_branches(1)+1,2/),SP)
   call  io_variable_bulk(ID,3,R3=q_weight_RT(:,:,:))
   !
 endif
 !
#endif
 !
 if (allocated(REF_lifetime)) then
   !
   call def_variable_bulk(ID,"RT_E_E_REF_Hole_lifetime",10,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,10,R2=REF_lifetime(:,:,1))
   !
   call def_variable_bulk(ID,"RT_E_E_REF_Electron_lifetime",11,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,11,R2=REF_lifetime(:,:,2))
   !
   call def_variable_bulk(ID,"RT_E_P_REF_Hole_lifetime",12,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,12,R2=REF_lifetime(:,:,3))
   !
   call def_variable_bulk(ID,"RT_E_P_REF_Electron_lifetime",13,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,13,R2=REF_lifetime(:,:,4))
   !
   call def_variable_bulk(ID,"RT_E_P_REF_Ho_abs_lifetime",14,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,14,R2=REF_lifetime(:,:,5))
   !
   call def_variable_bulk(ID,"RT_E_P_REF_El_abs_lifetime",15,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,15,R2=REF_lifetime(:,:,6))
   !
   call def_variable_bulk(ID,"RT_E_PHOT_REF_Hole_abs_lifetime",16,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,16,R2=REF_lifetime(:,:,7))
   !
   call def_variable_bulk(ID,"RT_E_PHOT_REF_El_abs_lifetime",17,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,17,R2=REF_lifetime(:,:,8))
   !
 endif
 !
 if (allocated(G_lesser_reference)) then
   !
   call def_variable_bulk(ID,"G_lesser_reference",4,(/2,RT_nbands,RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,4,C3=G_lesser_reference)
   !
#if !defined _YPP_RT
   if (read_is_on(ID)) call RT_init_occupation_reference( )
#endif
   !
 endif
 !
end subroutine Reference_IO

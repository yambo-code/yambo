!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use pars,           ONLY:SP,lchlen
 use drivers,        ONLY:l_real_time
 use R_lattice,      ONLY:nqbz
 use stderr,         ONLY:intc
 use timing_m,       ONLY:timing
 use parallel_m,     ONLY:PAR_IND_Xk_ibz
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,read_is_on,io_fragment, &
&                         manage_OP_IF_START_and_CL_IF_END,io_bulk,write_is_on
 use real_time,      ONLY:NE_i_time,RT_step,NE_steps,RT_nk, &
&                         RT_carriers,RT_nbands,RT_all2ibz
 use RT_control,     ONLY:RT_carriers_object,OBS_RT_IO_t
#if defined _PAR_IO
 use parallel_m,     ONLY:PAR_COM_Xk_ibz_INDEX,mpi_comm_world
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
 ! Work Space
 !
 character(lchlen)     :: db_name
 !
#if defined _PAR_IO
 integer               :: k_range(2),local_COMM
#endif
 real(SP)              :: time_now
 integer               :: ik,db_type
 logical               :: l_G,l_OBS,l_THETA,l_REF,l_carriers,l_JP,l_M,l_D,par_IO
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
 par_IO=.false.
 if (l_THETA.or.l_G) ik = maxval(io_sec(ID,:))-1
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
 if (l_OBS.or.l_carriers.or.( (l_G.or.l_THETA) .and. par_IO ) ) then
   call manage_OP_IF_START_and_CL_IF_END(ID,NOW=NE_i_time,LAST=NE_steps)
 endif
 !
 if ( (l_G.or.l_THETA) .and. (.not.par_IO) ) then
   if (ik>0.and.allocated(PAR_IND_Xk_ibz%element_1D)) then
     call manage_OP_IF_START_and_CL_IF_END(ID,NOW=RT_all2ibz(ik),PAR_index=PAR_IND_Xk_ibz)
   else
     call manage_OP_IF_START_and_CL_IF_END(ID,NOW=ik,LAST=RT_nk)
   endif
 endif
 !
 time_now=real(NE_i_time-1,SP)*RT_step
 !
#if defined _PAR_IO
 if(l_G.and.io_sec(ID,1)> 1) then
   io_RT_components=io_connect(desc=trim(db_name),type=db_type,ID=ID,COMM=local_COMM)
   if (io_RT_components/=0) goto 1
 else
#endif
 !
 io_RT_components=io_connect(desc=trim(db_name),type=db_type,ID=ID)
 if (io_RT_components/=0) goto 1
 !
#if defined _PAR_IO
 endif
#endif
 !
 ! 1st SECTION
 !=============
 if (any((/io_sec(ID,:)==1/))) then
   !
   call RT_header(ID,what,db_type,l_carriers,l_OBS,l_G,l_THETA,l_REF,io_RT_components)
   if (io_RT_components/=0) goto 1
   !
   if (l_OBS)       call RT_OBS_IO(ID,l_JP,l_M,"DEF")
   !
   if ( l_carriers) then
     !
     call RT_carriers_object(RT_carriers,DB_id=ID,WHAT="def_io_variables")
     !
     if (db_type==2.or..not.l_real_time) call TIME_points_IO(ID,(/13,14/),(NE_i_time==1),OBS_RT_IO_t)
     !
   endif
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)>1/))) goto 1
 !
 ! === no K no Time ===
 !#####################
 !
 ! GPL_EXCLUDE_START
 !
 ! REFERENCE Lifetimes and Green function
 !========================================
 if ( l_REF ) call Reference_IO(ID)
 !
 ! === K-dependent ===
 !####################
 !
#if defined _ELPH
 !
 ! ELPH Kernel THETA Matrix 
 !==========================
 if ( l_THETA ) then
   call io_fragment(ID,i_fragment=ik)
   call io_bulk(ID,VAR="RT_THETA_K"//trim(intc(ik)),VAR_SZ=(/2,2*nqbz*ph_modes,RT_nbands,RT_nbands/))
   call io_bulk(ID,C3=THETA_matrix(:,:,:,ik))
 endif
 !
#endif
 !
 ! lesser Green Function
 !=======================
 if ( l_G ) call G_lesser_IO(ID)
 !
 ! === Time-dependent ===
 !#######################
 !
 ! Carrier components
 !====================
 if ( l_carriers) then
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
subroutine RT_header(ID,what,db_type,l_carriers,l_OBS,l_G,l_THETA,l_REF,io_RT_components)
 !
 use pars,           ONLY:lchlen,schlen
 use drivers,        ONLY:l_real_time
 use R_lattice,      ONLY:nqbz
 use IO_m,           ONLY:io_header,io_status,io_elemental,io_bulk,frag_RT,&
&                         read_is_on
 use real_time,      ONLY:q_weight_RT,RT_carriers,G_MEM_steps,MEM_pointer,RT_obj_alloc
 use RT_control,     ONLY:SAVE_G_history
 !
 implicit none
 !
 character(*), intent(in) :: what
 integer, intent(in)      :: ID,db_type
 logical, intent(in)      :: l_carriers,l_OBS,l_G,l_THETA,l_REF
 integer, intent(out)     :: io_RT_components
 !
 integer, external     :: variables_RT
 !
 character(lchlen)     :: local_description(100)
 character(schlen)     :: what_
 !
 if ( db_type==2 .or. (db_type==-3.and..not.l_real_time) ) then
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
   io_RT_components=variables_RT(ID,local_description,trim(what_))
   if (io_RT_components/=0) return
   !
 endif
 !
 ! GPL_EXCLUDE_START
 !
 if (l_THETA) then
   call io_bulk(ID,VAR="Q_weight",VAR_SZ=(/nqbz/))
   call io_bulk(ID,R1=q_weight_RT(:,1,1))
 endif
 !
 ! Here I dump all RELEVANT RT_carriers% components
 ! that make the RT_carriers databases independent
 !
 if (l_carriers) then
   call io_elemental(ID,VAR="RT_carriers_dimensions",VAR_SZ=4,MENU=0) 
   call io_elemental(ID,I0=RT_carriers%nk)
   call io_elemental(ID,I1=RT_carriers%nb)
   call io_elemental(ID,I0=RT_carriers%nstates)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   if (read_is_on(ID)) call RT_obj_alloc(RT_carriers)
   !
   call io_bulk(ID,VAR="RT_kpt",VAR_SZ=(/RT_carriers%nk,3/))
   call io_bulk(ID,R2=RT_carriers%kpt)
   call io_bulk(ID,VAR="RT_k_weight",VAR_SZ=(/RT_carriers%nk/))
   call io_bulk(ID,R1=RT_carriers%k_weight)
   call io_bulk(ID,VAR="RT_table",VAR_SZ=(/RT_carriers%nstates,4/))
   call io_bulk(ID,I2=RT_carriers%table)
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 if (l_G) then
   call io_elemental(ID,VAR="Save_G_history",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,L0=SAVE_G_history)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   call io_bulk(ID,VAR="MEMORY_TIME_POINTS",VAR_SZ=(/G_MEM_steps/))
   call io_bulk(ID,I1=MEM_pointer)
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
subroutine TIME_points_IO(ID,VAR_ID,def_var,TYP)
 !
 use pars,           ONLY:IP,SP,schlen
 use RT_control,     ONLY:RT_IO_t
 use real_time,      ONLY:NE_steps
 use IO_m,           ONLY:def_variable_bulk,io_variable_bulk,NF90_unlimited
 !
#include<memory.h>
 !
 integer,       intent(in)    :: ID,VAR_ID(2)
 logical,       intent(in)    :: def_var
 type(RT_IO_t), intent(inout) :: TYP
 !
 integer                      :: IPOS(1)
 character(schlen)            :: dim_name(1)
 !
 if(def_var) call def_variable_bulk(ID,"IO_TIME_LAST_POINT",VAR_ID(1),(/1/),IP+100) 
 call io_variable_bulk(ID,VAR_ID(1),I0=TYP%last_point)
 !
 if (.not.allocated(TYP%Time)) then
   YAMBO_ALLOC(TYP%Time,(NE_steps))
 endif
 !
 IPOS=TYP%N
 if(def_var) then
   dim_name="IO_TIME_n_points"
   call def_variable_bulk(ID,"IO_TIME_points",VAR_ID(2),(/NF90_unlimited/),SP,dim_name)
   IPOS=IPOS-TYP%N+1
 endif
 call io_variable_bulk(ID,VAR_ID(2),R1=TYP%Time(TYP%N:TYP%N),IPOS=IPOS)
 !
end subroutine TIME_points_IO
!
!
! lesser Green Function
!=======================
!
subroutine G_lesser_IO(ID)
 !
 use pars,           ONLY:SP,schlen
 use stderr,         ONLY:intc
 use com,            ONLY:error
 use IO_m,           ONLY:def_variable_bulk,io_variable_bulk,&
&                         variable_exist,write_is_on,io_fragment,io_sec
 use hamiltonian,    ONLY:H_rotation
 use RT_control,     ONLY:GF_RT_IO_t,SAVE_G_history,TIME_string
 use real_time,      ONLY:dG_lesser,G_MEM_steps,RT_nbands,RT_nk,l_RT_rotate_DM,&
&                         NE_i_time,RT_step
 !
 implicit none
 !
 integer, intent(in)  :: ID
 !
 integer           :: ik,i_spin,i_memory,i_time
 integer           :: VAR_SZ(5),IPOS(5)
 character(schlen) :: dim_names(5),var_name
#if defined _PAR_IO
 integer           :: k_range(2)
#endif
 !
 i_spin=1
 VAR_SZ(1:4)=(/2,RT_nbands,RT_nbands,RT_nk/)
 dim_names  =[character(schlen) :: 'complex', 'RT_nbands','RT_nbands','RT_nk','G_IO_time_step']
 !
#if defined _PAR_IO
 !
 if(.not.SAVE_G_history) VAR_SZ(5)=G_MEM_steps
 if(     SAVE_G_history) VAR_SZ(5)=NF90_UNLIMITED
 l_H_rot=( l_RT_rotate_DM .and. (variable_exist(ID,"H_rotation").or.write_is_on(ID)) )
 !
 if(NE_i_time==1) then
   call def_variable_bulk(ID,"dG_lesser", 1,VAR_sz,SP,dim_names=dim_names,par_io_kind='independent')
   if(l_H_rot) call def_variable_bulk(ID,"H_rotation",2,VAR_sz(1:4),SP,dim_names=dim_names(1:4),par_io_kind='independent')
 endif
 !
 k_range(1)=io_sec(ID,1)-1
 k_range(2)=io_sec(ID,2)-1
 IPOS(1:4)=(/1,1,1,k_range(1)/)
 if(.not.SAVE_G_history) IPOS(5)=1
 if(     SAVE_G_history) IPOS(5)=((NE_i_time-1)/GF_RT_IO_t%N*G_MEM_steps+1)
 call io_variable_bulk(ID,1,C4=dG_lesser(:,:,k_range(1):k_range(2),:),IPOS=IPOS)
 if(l_H_rot) io_variable_bulk(ID,2,C3=H_rotation(:,:,k_range(1):k_range(2),i_spin),IPOS=IPOS(1:4)/))
 !
 call TIME_points_IO(ID,(/3,4/),(NE_i_time==1),GF_RT_IO_t)
 !
#else
 !
 if(SAVE_G_history) call error(" G history can be saved only with parallel IO")
 !
 ik = maxval(io_sec(ID,:))-1
 call io_fragment(ID,i_fragment=ik)
 !
 do i_memory=1,G_MEM_steps
   !
   i_time=NE_i_time-G_MEM_steps+i_memory
   if(i_time<=0) cycle
   !
   var_name="dG_lesser_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin))//"_TIME"//trim(TIME_string( (i_time-1)*RT_step ))
   call def_variable_bulk(ID,trim(var_name),i_memory,(/2,RT_nbands,RT_nbands/),SP)
   call  io_variable_bulk(ID,i_memory,C2=dG_lesser(:,:,ik,i_memory))
   !
 enddo
 !
 var_name="H_rotation_TIME"//trim(TIME_string( (i_time-1)*RT_step ))
 if ( l_RT_rotate_DM .and. (variable_exist(ID,trim(var_name)).or.write_is_on(ID)) ) then
   call def_variable_bulk(ID,trim(var_name),G_MEM_steps+1,(/2,RT_nbands,RT_nbands/),SP)
   call  io_variable_bulk(ID,G_MEM_steps+1,C2=H_rotation(:,:,ik,i_spin))
 endif
 !
 call TIME_points_IO(ID,(/G_MEM_steps+2,G_MEM_steps+3/),.true.,GF_RT_IO_t)
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
 use IO_m,        ONLY:io_variable_bulk,def_variable_bulk,NF90_unlimited
 use electrons,   ONLY:n_spin
 use real_time,   ONLY:l_NE_with_fields,NE_i_time
 use RT_control,  ONLY:J_cache,P_cache,Ms_cache,Ml_cache,A_cache,OBS_RT_IO_t
 !
#include<memory.h>
 !
 integer,      intent(in) :: ID
 logical,      intent(in) :: l_JP,l_M
 character(*), intent(in) :: what
 !
 integer           :: VAR_SZ(4)
 character(schlen) :: dim_names(4)
 !
 select case(WHAT)
   !
 case('DEF')
   !
   if (l_JP) then
     VAR_SZ(1:3)    = (/2,3,NF90_UNLIMITED/)
     dim_names(1:3) = [character(schlen) :: 'complex', 'xyz','time']
     call def_variable_bulk(ID,"Current",     1,VAR_SZ(1:3),SP,dim_names=dim_names)
     call def_variable_bulk(ID,"Polarization",2,VAR_SZ(1:3),SP,dim_names=dim_names)
   endif
   !
   if(n_spin>1.and.l_M) then
     VAR_SZ    = (/2,3,3,NF90_UNLIMITED/)
     dim_names = [character(schlen) :: 'complex', 'xyz','tot_c_v','time']
     call def_variable_bulk(ID,"Spin_Magnetization",3,VAR_SZ,SP,dim_names=dim_names)
     !
     VAR_SZ    = (/2,3,2,NF90_UNLIMITED/)
     dim_names = [character(schlen) :: 'complex', 'xyz','orb_itin','time']
     call def_variable_bulk(ID,"Orbital_Magnetization",4,VAR_SZ,SP,dim_names=dim_names)
   endif
   !
   if(l_NE_with_fields) then
     VAR_SZ    = (/2,3,3,NF90_UNLIMITED/)
     dim_names = [character(schlen) :: 'complex', 'xyz','A_kind','time']
     call def_variable_bulk(ID,"A_tot",5,VAR_SZ,SP,dim_names=dim_names)
   endif
   !
 case('FILL')
   !
   if (l_JP) then
     call io_variable_bulk(ID,1,C2=J_cache,IPOS=(/1,1,NE_i_time/))
     call io_variable_bulk(ID,2,C2=P_cache,IPOS=(/1,1,NE_i_time/))
   endif
   !
   if(n_spin>1.and.l_M) then
     call io_variable_bulk(ID,3,C3=Ms_cache,IPOS=(/1,1,1,NE_i_time/))
     call io_variable_bulk(ID,4,C3=Ml_cache,IPOS=(/1,1,1,NE_i_time/))
   endif
   !
   if(l_NE_with_fields) then
     call io_variable_bulk(ID,5,C3=A_cache,IPOS=(/1,1,1,NE_i_time/))
   endif
   !
   call TIME_points_IO(ID,(/6,7/),(NE_i_time==1),OBS_RT_IO_t)
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
 use drivers,        ONLY:l_elph_scatt
 use R_lattice,      ONLY:nqbz
 use IO_m,           ONLY:io_bulk,read_is_on
 use RT_control,     ONLY:RT_carriers_object
 use real_time,      ONLY:RT_init_occupation_reference,q_weight_RT,REF_lifetime,&
&                         G_lesser_reference,RT_nk,RT_nbands,RT_carriers
#if defined _ELPH
 use ELPH,           ONLY:elph_branches
#endif

 !
#include<memory.h>
 !
 integer, intent(in)  :: ID
 !
 call RT_carriers_object(RT_carriers,DB_id=ID,WHAT="ref_io")
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
   call io_bulk(ID,VAR="Q_WEIGHT",VAR_SZ=(/nqbz,elph_branches(2)-elph_branches(1)+1,2/))
   call io_bulk(ID,R3=q_weight_RT(:,:,:))
   !
 endif
 !
#endif
 !
 if (allocated(REF_lifetime)) then
   !
   call io_bulk(ID,VAR="RT_E_E_REF_Hole_lifetime",VAR_SZ=(/RT_nbands,RT_nk/))
   call io_bulk(ID,R2=REF_lifetime(:,:,1))
   !
   call io_bulk(ID,VAR="RT_E_E_REF_Electron_lifetime",VAR_SZ=(/RT_nbands,RT_nk/))
   call io_bulk(ID,R2=REF_lifetime(:,:,2))
   !
   call io_bulk(ID,VAR="RT_E_P_REF_Hole_lifetime",VAR_SZ=(/RT_nbands,RT_nk/))
   call io_bulk(ID,R2=REF_lifetime(:,:,3))
   !
   call io_bulk(ID,VAR="RT_E_P_REF_Electron_lifetime",VAR_SZ=(/RT_nbands,RT_nk/))
   call io_bulk(ID,R2=REF_lifetime(:,:,4))
   !
   call io_bulk(ID,VAR="RT_E_P_REF_Ho_abs_lifetime",VAR_SZ=(/RT_nbands,RT_nk/))
   call io_bulk(ID,R2=REF_lifetime(:,:,5))
   !
   call io_bulk(ID,VAR="RT_E_P_REF_El_abs_lifetime",VAR_SZ=(/RT_nbands,RT_nk/))
   call io_bulk(ID,R2=REF_lifetime(:,:,6))
   !
   call io_bulk(ID,VAR="RT_E_PHOT_REF_Hole_abs_lifetime",VAR_SZ=(/RT_nbands,RT_nk/))
   call io_bulk(ID,R2=REF_lifetime(:,:,7))
   !
   call io_bulk(ID,VAR="RT_E_PHOT_REF_El_abs_lifetime",VAR_SZ=(/RT_nbands,RT_nk/))
   call io_bulk(ID,R2=REF_lifetime(:,:,8))
   !
 endif
 !
 if (allocated(G_lesser_reference)) then
   !
   call io_bulk(ID,VAR="G_lesser_reference",VAR_SZ=(/2,RT_nbands,RT_nbands,RT_nk/))
   call io_bulk(ID,C3=G_lesser_reference)
   !
#if !defined _YPP_RT
   if (read_is_on(ID)) call RT_init_occupation_reference( )
#endif
   !
 endif
 !
end subroutine Reference_IO

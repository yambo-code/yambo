!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function INIT_ph(en,q,k,X,Xw,Dip,instr,lnstr,CLOSE_Gs,FINALIZE)
 !
#if defined  _PHEL
 use PHEL,           ONLY:PH_Q
 use ELPH,           ONLY:PH_W_debye
#endif
 use ELPH,           ONLY:elph_nQ,elph_nQ_used
 !
#include<y_memory.h>
 !
 type(levels)    ::en
 type(bz_samp)   ::q,k
 type(X_t)       ::X(5)
 type(DIPOLE_t)  ::Dip
 type(w_samp)    ::Xw(5)
 integer         ::lnstr
 character(lnstr)::instr
 logical         ::CLOSE_Gs,FINALIZE
 !
 if (.not.FINALIZE.and..not.CLOSE_Gs) then
   !
   ! Global Logicals
   !
   call SET_logicals_ph()
   !
 endif
 !
 ! Presets (local)
 !
 if (FINALIZE) then
   call call_INIT_load('Game_Over')
   call initinfio(defs,repfile_unit)
   call ofiles_append(defs=defs)
   close(unit=repfile_unit)
   if(log_as_a_file) close(unit=logfile_unit)
#if defined _YAML_OUTPUT
   call yaml_close_stream(unit=of_yaml_IDs(repfile_index))
   if(log_as_a_file) call yaml_close_stream(unit=of_yaml_IDs(logfile_index))
   call f_lib_finalize()
#endif
   return
 endif
 if (CLOSE_Gs) then
   call INIT_barriers_ph( )
   call LOCAL_from_runlevels_to_logicals()
   return
 endif
 !
 ! First vars loading
 !
 call call_INIT_load('create_shadow_vars')
 !
 ! Command line reading
 !
 ! Dump the input file
 !
 if (file_exists(trim(infile))) then
   !
   call infile_dump()
   !
   call INIT_input_file( )
   !
 else if (.not.infile_editing) then
   infile='(none)'
 endif
 !
 call INIT_check_databases_ph(io_ELPH_err)
 !
 if (list_dbs) then
   call msg('s','')
   return
 endif
 !
 ! Runlevels variables activation (Logicals from stdin)
 call LOCAL_from_runlevels_to_logicals()
 !
 ! Given the stding vars here I fix/check the 
 ! corr/scatt logicals
 call LOCAL_scatt_and_correlation_logicals( )
 !
 !Setup on the basis of the DB read/stdin variables
 !Here I can propose values to be written in the input file
 call LOCAL_setup_before_input_IO( )
 !
 ! Scattering/correlation logicals
 !
 !Variables(read from DB files) -> local cache
 !
 !Note that here ('load_defaults') is the latest chance to change
 !a predefined var verbosity and to force its appearance
 !in the input file.
 !
 call call_INIT_load('load_defaults')
 !
 !Input file/local cache -> local cache/Variables
 !
 call call_INIT_load('parser_input_file')
 !
 !RUNLEVELS VARIABLES ACTIVATION (LOGICALS FROM INPUT FILE)
 !
 if (.not.any(rstatus>0)) then
   do i_r=1,nrnlvls
     if (trim(rnlvls(i_r,1))=="setup") rstatus(i_r)=-1
   enddo
 endif
 call LOCAL_from_runlevels_to_logicals()
 call LOCAL_from_runlevels_to_logicals()
 !
 call INIT_activate()
 !
 ! Q-points (from DB)
 !
 if (l_setup.and.io_QINDX_err/=0) call INIT_q_points_ph( )
 !
 QP_field=l_phel_corr
 !
 if (QP_field) then
   !
   ! The QP_state can be read from DB's in DUMP mode or from the input file.
   ! The DB value is used when a new input file is created or when the previous input file
   ! has no QP fields (see inside INIT_QP)
   !
   ! QP_state from DB ?
   !
   if (allocated(QP_state)) call QP_state_table_setup(en,1,1,.true.)
   !
   call INIT_QP("[GW] QP")
   !
   ! I use it to propose the value in the input file ...
   !
   ! ... but afterword I must deallocate it to use user defined values
   !
   YAMBO_FREE(QP_state)
   YAMBO_FREE(QP_table)
   !
 endif
 !
 ! Are we editing the input file ?
 !
 if (infile_editing) then
   inpfile_unit=12
   open(unit=inpfile_unit,file=trim(infile))
   call initinfio(defs,inpfile_unit)
   close(inpfile_unit)
   call PP_wait()
   return
 endif
 !
 ! Update Logicals (especially the write_to_log)
 !
 call SET_logicals_ph()
 !
 ! Finalize
 !
 call PP_wait()
 !
 contains
   !
   subroutine call_INIT_load(mode)
     character(*)::mode
     if (mode=='create_shadow_vars') initmode=0
     if (mode=='load_defaults') initmode=1
     if (mode=='Close_G_vectors') initmode=2
     if (mode=='Game_Over') initmode=3
     if (mode=='parser_input_file') initmode=4
     call INIT_load_ph(defs,en,q,k,X,Xw,Dip)
   end subroutine
   !
   subroutine LOCAL_from_runlevels_to_logicals()
     !
     integer     ::i1
     !
     do i1=1,2
       !
       l_elph_Hamiltonian=runlevel_is_on('ElPhHam')
       l_elph_corr=runlevel_is_on('el_ph_corr')
       l_phel_corr=runlevel_is_on('ph_el_corr')
       !
       ! Check if this runlevel is allowed in the
       ! present configuration
       !
       if (i1==1) call INIT_barriers_ph( )
       !
     enddo
     !
   end subroutine LOCAL_from_runlevels_to_logicals
   !
   subroutine LOCAL_setup_before_input_IO()
     !
     elph_nQ_used=elph_nQ
#if defined _PHEL
     PH_Q=(/1,elph_nQ/)
     QP_G_er=(/0._SP,PH_W_debye/)
#endif
     if (any((/l_elph_corr,l_phel_corr/))) call initactivate(1,'em1d dipoles')
     !
   end subroutine LOCAL_setup_before_input_IO
   !
   subroutine LOCAL_scatt_and_correlation_logicals( )
     !
     if (l_cohsex) call initactivate(1,'el_el_corr')
     if (.not.l_life) call switch_off_runlevel('el_el_corr')
     !
     if (io_ELPH_err(1)<0.and.io_ELPH_err(2)<0) call switch_off_runlevel('el_ph_corr ph_el_corr',except=' ')
     if (io_ELPH_err(2)<0                     ) call switch_off_runlevel('el_ph_scatt ph_el_scatt',except=' ')
     !
   end subroutine LOCAL_scatt_and_correlation_logicals
   !
end function
!
!
subroutine INIT_check_databases_ph(io_ELPH_err)
 !
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,DUMP,NONE
 !
 !
 integer          :: io_ELPH_err(2)
 !
 ! Workspace
 !
 integer           :: io_ID,io_err
 !
 integer, external :: io_Double_Grid
 integer, external :: io_ELPH
 !
 ! PH double grid
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_err=io_Double_Grid(en,k,io_ID,'phonons','')
 !
 ! ELPH 
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_ELPH_err=io_ELPH(io_ID,'gkkp')
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_ELPH_err=io_ELPH(io_ID,'gkkp_expanded')
 !
end subroutine

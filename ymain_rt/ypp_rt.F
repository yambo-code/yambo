!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function ypp_rt(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 !
 use drivers,             ONLY:infile_editing
 use stderr,              ONLY:write_to_log,STRING_same
 use parallel_m,          ONLY:ncpu,myid
 use electrons,           ONLY:levels,E_reset,n_bands,default_nel
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nkibz,ng_vec,nXkibz,nqibz,k_pt,q_pt, &
&			       X_scattering,sigma_scattering      
 use D_lattice,           ONLY:input_GS_Tel,alat,nsym
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:OP_RD_CL,NONE,DUMP
 use com,                 ONLY:msg
 use it_tools,            ONLY:it_reset
 use it_m,                ONLY:infile
 use wave_func,           ONLY:io_WF
 use parallel_int,        ONLY:PARALLEL_global_indexes
 use YPPm,                ONLY:l_bz_grids,l_dos,l_electrons,l_fix_syms,l_map_kpts,l_SOC_map,&
&                              l_QP_DBs_create_and_modify,l_QP_DBs_manipulate,l_excitons,l_wannier,&
&                              alat_used_for_output,l_QP_DB_expand,coo_out,coo_in,l_dipoles,&
&                              DIPs_kind,l_sp_wf,l_mean_potential
 use YPP_real_time,       ONLY:l_RealTime,l_RT_DBs,l_NL_X,l_NL_exc,l_RT_abs
 !
 implicit none
 !
 type(levels)                 :: en,Xen,Ken
 type(bz_samp)                :: k,Xk,q
 integer,          intent(in) :: lnstr,iind,iod,ijs,np,pid,icd
 integer,       intent(inout) :: iinf
 character(lnstr), intent(in) :: instr
 character(iinf),  intent(in) :: inf
 character(iind),  intent(in) :: ind
 character(iod),   intent(in) :: od
 character(ijs),   intent(in) :: js
 character(icd),   intent(in) :: com_dir
 ! 
 ! ... I/O 
 logical           :: l_flag
 integer           :: ID,i_err
 integer, external :: io_DB1,io_QINDX,io_GROT
 integer           :: i_err_DbGd
 integer, external :: io_Double_Grid
 ! 
 ! Presets
 !
 iinf   =0
 ypp    =0
 ncpu   =np
 myid   =pid
 infile =inf
 !
 ! ... Internal Defaults
 !
 call SET_defaults(instr,ind,od,com_dir)
 call YPP_SET_defaults()
 !
 ! ... C_driver -> YPP information transfer
 !
 call C_driver_transfer()
 !
 call bz_samp_reset(k)
 call bz_samp_reset(q)
 call bz_samp_reset(Xk)
 call E_reset(en)
 call E_reset(Xen)
 call E_reset(Ken)
 call it_reset(1)
#if defined _MEM_CHECK 
 call MEM_defaults( )
#endif
 call SET_job_strings_and_dirs(js)
 !
 ! DB1
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=ID)
 iinf=io_DB1(en,k,ID)
 if (iinf/=0) then
   call ypp_finalize("Where is the SAVE?",instr)
   return
 else
   call IO_make_directories("MORE LOG")
 endif
 !
#if defined _MEM_CHECK
 call MEM_treshold_estimate( )
#endif
 !
 ! WF
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=NONE,MODE=DUMP,ID=ID)
 iinf=io_WF(ID) 
 if (iinf/=0) then
   call ypp_finalize("Where is the SAVE?",instr)
   return
 endif
 !  
 ! G operations table and shells
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=ID)
 i_err=io_GROT(ID) 
 if (i_err/=0) then
   write_to_log=.TRUE.
   call ypp_finalize("Missing post-setup databases. Please run yambo as it is first.",instr)
   return
 endif
 !
 ! INPUT FILE parsing
 !
 call INIT_ypp(en,instr,.false.)
 if (infile_editing) then
   iinf=1
   call ypp_finalize(" ",instr)
   return
 endif
 !
 ! PARALLEL and I/O Setup 
 !=======================
 !
 call PARALLEL_and_IO_Setup(en,k)
 !
 call section('*','Y(ambo) P(ost)/(re) P(rocessor)')
 !
 call section('*','Core DB')
 call msg('s',':: Electrons             :',default_nel)
 call msg('s',':: Temperature           :',input_GS_Tel,"[eV]")
 call msg('s',':: Lattice factors       :',alat,"[a.u.]")
 call msg('s',':: K points              :',nkibz)
 call msg('s',':: Bands                 :',n_bands)
 call msg('s',':: Symmetries            :',nsym)
 call msg('s',':: RL vectors            :',ng_vec)
 !
 if (STRING_same(trim(coo_out),"alat").or.STRING_same(trim(coo_in),"alat")) then
   if (alat_used_for_output==0.) then
     call warning("Input alat undefined. Using the DB value")
     alat_used_for_output=alat(1)
   endif
   call msg('s',':: Lattice constant (used to define "alat" unit) [a.u.]:',alat_used_for_output)
 endif
 !
 ! BZ sampling index 
 !
 call section('*','K-point grid')

 X_scattering    =.TRUE.
 if(l_sp_wf.or.l_mean_potential) then  
   sigma_scattering=.TRUE.
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,3,4/),MODE=DUMP,ID=ID)
 else
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,3/),MODE=DUMP,ID=ID)
 endif
 i_err=io_QINDX(k,q,ID)
 if(i_err/=0) call error(' Error reading K-point grid, please run setup again!')
 !
 ! Rim energies and kpts
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
 i_err_DbGd=io_Double_Grid(en,k,ID,'electrons')
 ! 
 ! Allocate and fill the k_pt/q_pt array for the DBs header
 !
 allocate(k_pt(k%nibz,3))
 k_pt=k%pt
 !
 allocate(q_pt(q%nibz,3))
 q_pt=q%pt
 !
 if (i_err/=0) then
   call ypp_finalize(" ",instr)
   return
 endif
 !
 call msg('s',':: Q-points   (IBZ):',nqibz)
 call msg('s',':: X K-points (IBZ):',nXkibz)
 !
 ! Main SETUP
 !============
 !
 call setup(en,Xen,Ken,k,Xk)
 !
 call PARALLEL_global_indexes(Xen,Xk,q," ",RESET=.TRUE.)
 !
 ! Specific SECTIONS
 !===================
 !
 if (l_dipoles.and.trim(DIPs_kind)=="IP") call DIPOLE_ypp_driver(Xk,Xen,q)
 !
 if (l_bz_grids) call k_grids(en,k,Xk,q)
 !
 if (l_map_kpts) call k_map_fine_to_coarse_driver(Xk,Xen)
 !
 if (l_SOC_map) call WF_map_perturbative_SOC(Xk,Xen)
 !
 if (l_QP_DBs_create_and_modify)  call QP_DBs_create_and_modify(en,k)
 !
 if (l_QP_DB_expand)  call QP_DB_expand( k )
 !
 if (l_QP_DBs_manipulate)  call QP_DBs_manipulate( en )
 !
 if (l_wannier)  call WANNIER_driver(k,en)
 !
 if (l_fix_syms) call symmetries_driver(en,k)
 !
 if (l_RT_DBs)   call RT_DBs_carriers_setup(en,k,q)
 !
 if (l_RealTime) call RT_ypp_driver(en,k,q)
 !
 if (l_RT_abs)   call RT_transient_absorption_driver(k,Xk,en,Xen,q)
 !
 l_flag=l_electrons
 if (l_flag)    call electrons_driver(Xk,Xen,q)
 !
 ! CLOSING
 !=========
 !
 call CLOSE_the_run(.TRUE.,.TRUE.,.FALSE.)
 !
 call INIT_ypp(en,instr,.TRUE.)
 !
 return
 !
contains
  !
  subroutine ypp_finalize(STRING,instr)
    character(*) :: STRING
    character(*) :: instr
    if (.not.infile_editing) then
      if (len_trim(STRING)> 0) call msg("nl",trim(STRING))
      if (len_trim(STRING)==0) call section('X','Game Over')
      call msg("nln"," ")
    endif
    if (.not.infile_editing) call INIT_ypp(en,instr,.TRUE.)
  end subroutine
  !
end function ypp_rt

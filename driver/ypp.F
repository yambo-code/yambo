!
!        Copyright (C) 2000-2020 the YAMBO team
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
integer function ypp(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 !
 use drivers,             ONLY:infile_editing
 use stderr,              ONLY:write_to_log
 use parallel_m,          ONLY:ncpu,myid
 use electrons,           ONLY:levels,E_reset,n_bands,default_nel
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nkibz,ng_vec,&
&                              nXkibz,nqibz,k_pt,q_pt
 use D_lattice,           ONLY:input_GS_Tel,alat,nsym
 use IO_m,                ONLY:io_control,OP_RD_CL,NONE,DUMP,mk_dir
 use com,                 ONLY:msg,more_io_path,com_path
 use it_tools,            ONLY:it_reset
 use it_m,                ONLY:infile
 use wave_func,           ONLY:io_WF
 use interfaces,          ONLY:PARALLEL_global_indexes
 use YPPm,                ONLY:l_bxsf,l_bz_grids,l_dos,l_electrons,l_fix_syms,l_map_kpts,l_map_WFs,&
&                              l_QP_DBs_create_and_modify,l_QP_DBs_manipulate,l_excitons,l_wannier,&
&                              l_fix_save,alat_used_for_output,l_QP_DB_expand
#if defined _YPP_RT
 use YPP_real_time,       ONLY:l_RealTime,l_RT_abs,l_RT_DBs,l_NL_X,l_NL_exc
#endif
#if defined _YPP_ELPH
 use YPP_ELPH,            ONLY:l_atomic_amplitude,l_eliashberg,l_phonons,l_gkkp,ELPH_general_gFsq
#endif
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
 ! I/O 
 !
 logical           :: l_flag
 integer           :: ID,i_err
 integer, external :: io_DB1,io_QINDX,io_GROT
 integer           :: i_err_DbGd
 integer, external :: io_Double_Grid
#if defined _YPP_ELPH
 integer, external :: ELPH_databases
#endif
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
   call mk_dir(more_io_path)
   call mk_dir(com_path)
   if (ncpu>1) call mk_dir(trim(com_path)//"/LOG")
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
 call msg('s',':: Temperature       [ev]:',input_GS_Tel)
 call msg('s',':: Lattice factors [a.u.]:',alat)
 call msg('s',':: K points              :',nkibz)
 call msg('s',':: Bands                 :',n_bands)
 call msg('s',':: Symmetries            :',nsym)
 call msg('s',':: RL vectors            :',ng_vec)
 !
 alat_used_for_output=alat(1)
 !
 ! BZ sampling index 
 !
 call section('*','K-point grid')
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,3/),MODE=DUMP,ID=ID)
 i_err=io_QINDX(k,q,ID)
 !
 ! Rim energies and kpts
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,ID=ID)
 i_err_DbGd=io_Double_Grid(en,Xk,ID) 
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
 if (l_bz_grids) call k_grids(en,k,Xk,q)
 !
 if (l_map_kpts) call k_map_fine_to_coarse(Xk,Xen)
 !
 if (l_map_WFs) call WF_map_perturbative_SOC(Xk,Xen)
 !
 if (l_bxsf)  call bxsf_read(Xk,Xen)
 !
 if (l_QP_DBs_create_and_modify)  call QP_DBs_create_and_modify(en,k)
 !
 if (l_QP_DB_expand)  call QP_DB_expand( k )
 !
 if (l_QP_DBs_manipulate)  call QP_DBs_manipulate( en )
 !
 if (l_wannier) call WANNIER_driver(k,en)
 !
 if (l_fix_syms) call symmetries_driver(en,k)
 !
 if (l_fix_save) call WF_format_converter(en,k,q)
 !
#if defined _YPP_RT
 !
 ! GPL_EXCLUDE_START
 !
 if (l_RT_DBs)   call RT_DBs_carriers_setup(en,k,q)
 !
 ! GPL_EXCLUDE_END
 !
 if (l_RealTime) call RT_driver(en,k,q)
 !
#endif
 !
#if defined _YPP_NL
 !
 if (l_NL_X) call NL_driver(en,k)
 if (l_NL_exc) call NL_Exc_driver(en,k)
 !   ======
#endif
 !
 l_flag=l_excitons
#if defined _YPP_RT
 l_flag=l_flag.or.l_RT_abs
#endif
 if (l_flag) call excitons_driver(k,Xk,en,Xen,q)
 !
 l_flag=l_electrons
#if defined _YPP_ELPH && !defined _YPP_RT
 l_flag=l_flag.and.(.not.l_eliashberg)
#endif
 !
 if (l_flag) call electrons_driver(Xk,Xen,q)
 !
#if defined _YPP_ELPH
 !
 if ((l_gkkp.and..not.l_excitons).or.l_phonons) then
   ! =====     ========
   if (l_gkkp)                i_err=ELPH_databases(k,en,q)
   if (l_eliashberg.or.l_dos) call ELPH_eliashberg_dos(k,en,q)
   ! GPL_EXCLUDE_START
   if (l_atomic_amplitude)    call ELPH_atomic_amplitude(q)
   ! GPL_EXCLUDE_END
 endif
 !
 if (l_electrons.and.l_eliashberg) call ELPH_general_gFsq(k,en,Xk,Xen,q)
 !
#endif
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
end function ypp

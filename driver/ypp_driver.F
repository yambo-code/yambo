
!        Copyright (C) 2000-2017 the YAMBO team
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
integer function ypp_driver(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 !
 use drivers,             ONLY:infile_editing
 use parallel_m,          ONLY:ncpu,myid
 use electrons,           ONLY:levels,E_reset,n_bands,default_nel
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nkibz,ng_vec,&
&                              nXkibz,nqibz,k_pt,q_pt
 use D_lattice,           ONLY:input_GS_Tel,alat,nsym
 use IO_m,                ONLY:io_control,OP_RD_CL,NONE,DUMP,mk_dir
 use com,                 ONLY:msg,write_to_report,more_io_path,com_path
 use it_m,                ONLY:it_reset,infile
 use wave_func,           ONLY:io_WF
 use interfaces,          ONLY:PARALLEL_global_indexes
#if defined _MEM_CHECK 
 use memory,              ONLY:MEM_manager
#endif
 use LIVE_t,              ONLY:live_timing_is_on
 use stderr,              ONLY:write_to_log
 use YPP,                 ONLY:l_bxsf,l_bz_grids,l_dos,l_electrons,l_fix_syms,l_map_kpts,l_map_WFs,&
&                              l_QP_DBs_create_and_modify,l_QP_DBs_manipulate,l_excitons,l_wannier
#if defined _YPP_RT
 use YPP_real_time,       ONLY:l_RealTime,l_RT_abs,l_RT_DBs
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
 iinf       =0
 ypp_driver =0
 ncpu       =np
 myid       =pid
 infile     =inf
 call SET_defaults(instr,ind,od,js,com_dir)
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
 !
 ! Switch off report file support
 !
 write_to_report   =.FALSE.
 live_timing_is_on =myid==0
 write_to_log      =myid==0
 !
 ! DB1
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=ID)
 iinf=io_DB1(en,k,ID)
 if (iinf/=0) then
   call ypp_finalize("Where is the SAVE?")
   return
 else
   call mk_dir(more_io_path)
   call mk_dir(com_path)
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
   call ypp_finalize("Where is the SAVE?")
   return
 endif
 !  
 ! G operations table and shells
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=ID)
 i_err=io_GROT(ID) 
 if (i_err/=0) then
   call ypp_finalize("Missing post-setup databases. Please run yambo as it is first.")
   return
 endif
 !
 ! INPUT FILE
 !
 call INIT_ypp(en,instr,lnstr,.false.)
 if (infile_editing) then
   iinf=1
   call ypp_finalize(" ")
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
   call ypp_finalize(" ")
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
 !   =========
 !
 if (l_map_kpts) call k_map_fine_to_coarse(Xk,Xen)
 !
 ! GPL_EXCLUDE_START
 !
 if (l_map_WFs) call WF_map_perturbative_SOC(Xk,Xen)
 !
 ! GPL_EXCLUDE_END
 !
 if (l_bxsf)  call bxsf_read(Xk,Xen)
 !
 if (l_QP_DBs_create_and_modify)  call QP_DBs_create_and_modify(en,k)
 !
 if (l_QP_DBs_manipulate)  call QP_DBs_manipulate( )
 !
 if (l_wannier) call WANNIER_driver(k,en)
 !   =========
 !
 if (l_fix_syms) call symmetries_driver(en,k)
 !  ==========
 !
 ! GPL_EXCLUDE_START
 !
#if defined _YPP_RT
 !
 if (l_RT_DBs)   call RT_DBs_carriers_setup(en,k,q)
 !
 if (l_RealTime) call RT_driver(en,k,q)
 !
#endif
 !
 ! GPL_EXCLUDE_END
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
 ! Memory Overview
 !=================
#if defined _MEM_CHECK 
 !
 call section('*','Memory Overview')
 !
 call MEM_manager("",REPORT="final")
 !
#endif
 !
 ! END
 !
 call ypp_finalize(" ")
 !
 return
 !
contains
  !
  subroutine ypp_finalize(STRING)
    character(*) :: STRING
    if (.not.infile_editing) then
      if (len_trim(STRING)> 0) call msg("nl",trim(STRING))
      if (len_trim(STRING)==0) call section('X','Game Over')
      call msg("nln"," ")
    endif
    call INIT_ypp(en,instr,lnstr,.TRUE.)
  end subroutine
  !
end function ypp_driver

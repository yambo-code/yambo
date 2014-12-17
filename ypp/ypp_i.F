
!        Copyright (C) 2000-2014 the YAMBO team
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
integer function ypp_i(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 !
 use drivers,             ONLY:infile_editing
 use parallel_m,          ONLY:ncpu,myid,MPI_close
 use electrons,           ONLY:levels,E_reset,n_bands,default_nel
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nkibz,ng_vec,&
&                              nXkibz,nqibz,k_pt,q_pt
 use D_lattice,           ONLY:input_GS_Tel,alat,nsym
 use IO_m,                ONLY:io_control,OP_RD_CL,NONE,DUMP
 use com,                 ONLY:msg,write_to_report
 use it_m,                ONLY:it_reset,infile
 use wave_func,           ONLY:io_WF
 use YPP
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
 integer           :: i_err_RIM
 integer, external :: io_RIM_E_and_k
#if defined _YPP_ELPH
 integer, external :: ELPH_databases
#endif
 !
 ! Presets
 !
 iinf  =0
 ypp_i =0
 ncpu  =np
 myid  =pid
 infile=inf
 call SET_defaults(instr,ind,od,js,com_dir)
 call bz_samp_reset(k)
 call bz_samp_reset(q)
 call bz_samp_reset(Xk)
 call E_reset(en)
 call E_reset(Xen)
 call E_reset(Ken)
 call it_reset(1)
 !
 ! Switch off report file support
 !
 write_to_report=.FALSE.
 !
 ! PARALLEL and I/O Setup 
 !=======================
 !
 call PARALLEL_and_IO_Setup(en,k)
 !
 ! DB1
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=ID)
 iinf=io_DB1(en,k,ID)
 if (iinf/=0) then
   call ypp_finalize()
   return
 endif
 !
 ! WF
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=NONE,MODE=DUMP,ID=ID)
 iinf=io_WF(ID) 
 if (iinf/=0) then
   call ypp_finalize()
   return
 endif
 !  
 ! G operations table and shells
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=ID)
 i_err=io_GROT(ID) 
 if (i_err/=0) then
   call ypp_finalize()
   return
 endif
 !
 ! INPUT FILE
 !
 call ypp_init(en,instr,lnstr,.false.)
 if (infile_editing) then
   iinf=1
   call ypp_finalize()
   return
 endif
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
 i_err_RIM=io_RIM_E_and_k(en,Xk,ID) 
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
   call ypp_finalize()
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
 ! Specific SECTIONS
 !===================
 !
 if (l_bz_grids) call k_grids(en,k,Xk,q)
 !   =========
 !
 if (l_bzrim) call k_random
 !
 if (l_map_kpts) call k_map_fine_to_coarse(Xk,Xen)
 !
 if (l_bxsf)  call read_bxsf(Xk,Xen)
 !
 if (l_QP_DBs_create_and_modify)  call QP_DBs_setup(en,k)
 !
 if (l_QP_DBs_merge)  call QP_DBs_merge(en,k)
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
 if (l_RT_X.or.l_RT_occ_Plot.or.l_RT_occ_Fit) call RT_driver(en,k)
 !   ======
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
 if (l_excitons.or.l_RT_abs) call excitons_driver(k,Xk,en,Xen,q)
 !   ===========
 !
 l_flag=l_electrons
#if defined _YPP_ELPH && !defined _YPP_RT
 l_flag=l_flag.and.(.not.l_eliashberg)
#endif
#if defined _YPP_RT
 l_flag=l_flag.or.l_RT_dos.or.l_RT_life_Bands.or.l_RT_occ_Bands
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

#if defined _YPP_SURF
 !
 if (lras) call ras_ypp
 if (lreels) call reels_ypp
!if (lcelleps) call ras_generate_epsdb(en,k)
 if (lloc) call ras_loc_ypp
 if (ltrans) call ras_trans_ypp(en,k)
 !
#endif

 !
 ! END
 !
 call ypp_finalize()
 return
 !
contains
  !
  subroutine ypp_finalize()
    if (.not.infile_editing) then
      call section('X','Game Over')
      call ypp_init(en,instr,lnstr,.TRUE.)
      call msg("nl","")
      call MPI_close()
    else
      call ypp_init(en,instr,lnstr,.TRUE.)
    endif
  end subroutine
  !
end function

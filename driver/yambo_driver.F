!
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
integer function yambo_driver(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 !
 !Y88b    /   e           e    e      888~~'    ,88~-_   
 ! Y88b  /   d8b         d8b  d8b     888   |  d888   '  
 !  Y88b/   /Y88b       d888bdY88b    888 _/  88888    | 
 !   Y8Y   /  Y88b     / Y88Y Y888b   888  '  88888    | 
 !    Y   /____Y88b   /   YY   Y888b  888   |  Y888   /  
 !   /   /      Y88b /          Y888b 888__/    `88_-~   
 !
 use drivers,       ONLY:l_HF_and_locXC,l_sc_run,l_eval_collisions,l_real_time,&
&                        l_optics,l_chi,l_em1s,l_ppa,l_cohsex,l_bss,l_bse,l_em1d,&
&                        l_gw0,l_elel_corr,l_life,l_acfdt,l_elphoton_corr,l_dipoles
#if defined _SCALAPACK
 use drivers,       ONLY:l_slk_test
#endif
 use X_m,           ONLY:X_t
 use DIPOLES,       ONLY:DIPOLE_t
 use parallel_m,    ONLY:myid,ncpu
 use it_m,          ONLY:it_reset,infile
 use QP_m,          ONLY:QP_ng_Sx,QP_ng_SH
 use wave_func,     ONLY:wf_ng
 use electrons,     ONLY:levels,E_reset
#if defined _MEM_CHECK 
 use memory,        ONLY:MEM_manager
#endif
#if defined _TIMING
 use timing_m,      ONLY:timing_overview,timing_deallocate,global_list
#endif
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use frequency,     ONLY:w_samp
#if defined _ELPH
 use drivers,       ONLY:l_elph_Hamiltonian
#endif
 !
 implicit none
 !
 type(levels) ::en,Xen,Ken
 type(bz_samp)::q,k,Xk
 ! 
 ! 4 response function & energies types: x s d p
 !
 type(X_t)    ::X(4) 
 type(w_samp) ::Xw(4)
 !
 type(DIPOLE_t) :: Dip
 !
 integer,          intent(in)    :: lnstr,iind,iod,ijs,np,pid,icd
 integer,          intent(inout) :: iinf
 character(lnstr), intent(in)    :: instr
 character(iinf),  intent(in)    :: inf
 character(iind),  intent(in)    :: ind
 character(iod),   intent(in)    :: od
 character(ijs),   intent(in)    :: js
 character(icd),   intent(in)    :: com_dir
 !
 ! Local Variables
 !
 integer           ::i_err,i_X_kind
 logical           ::l_sta_X,l_dyn_X,driver_now
 !
 ! Externals
 !
 integer, external ::X_dielectric_matrix,INIT
 !
 ! Presets
 !
 ncpu     =np
 myid     =pid
 infile   =inf
 !
 call SET_defaults(instr,ind,od,js,com_dir)
 call it_reset(1)
#if defined _MEM_CHECK 
 call MEM_defaults( )
#endif
 !
 ! not resetted in INIT call
 !
 call bz_samp_reset(Xk)  
 call E_reset(Xen) 
 call E_reset(Ken) 
 !
 ! INPUT FILE PARSING
 !====================
 !
 yambo_driver=0
 !
 ! iinf=0 => No error message from driver.c 
 !
 iinf=0 
 iinf=INIT(en,q,k,X,Xw,Dip,instr,lnstr,.false.,.false.)
 !
 if (iinf/=0) return
 !
 ! PARALLEL and I/O Setup 
 !=======================
 !
 call PARALLEL_and_IO_Setup(en,k)
 !
 ! SETUPS ...
 !============
 !
 ! Basical 
 !
 call setup(en,Xen,Ken,k,Xk)
 !
 ! Update the default variables
 !
 i_err=INIT(en,q,k,X,Xw,Dip,instr,lnstr,.true.,.false.)
 !
 ! BZ sampling
 !
 call bz_samp_indexes(k,Xk,q) 
 !
 ! COLOUMB DRIVER
 !================
 !
 call col_driver(maxval((/wf_ng,QP_ng_Sx,QP_ng_SH/)),q)
 !
 ! reset the section numbering
 !
 call section('*','') 
 !
 ! RESPONSE FUNCTIONS QP CORRECTIONS (FROM INPUT FILE)
 !======================================================
 !
 call External_corrections(en,Xen,Ken,Xk,k,X)
 !
 ! Logicals for \eps^-1
 !======================
 !
 l_dyn_X = l_em1d .or. (l_optics.and.l_chi) .or. (l_gw0.and.l_elel_corr.and..not.l_cohsex) .or. l_life
 l_sta_X = l_em1s .or. (l_bse.and..not.l_ppa)
 !
 if ( l_real_time .or. l_sc_run) i_X_kind=1
 if ( l_sta_X .and. .not.l_ppa ) i_X_kind=2
 if ( l_dyn_X .and. .not.l_ppa ) i_X_kind=3
 if (                    l_ppa ) i_X_kind=4
 !
 ! DIPOLES
 !=========
 !
 driver_now= l_sta_X.or.l_dyn_X.or.l_optics.or.l_life.or.l_dipoles.or.l_real_time.or.l_sc_run
 !
 ! DS: 2017/08/22 
 ! Problem here: the dipoles depend on the exernal corrections, both QP (DIP_P) and RT (size)
 ! (RT) Possible workarounds is to consider the minimum of nbf / maximum of nbm
 ! (QP) I changed what is stored in DIP_P / P_square
 !      Now always the KS P / P2 are used. P_eff is defined at IO time
 !      Still to fix the case with io_DIP=.false.
 !
 if (driver_now) call DIPOLE_driver(Xen, Xk, q, X(i_X_kind), Dip)
 if (driver_now) call mem_manager_report
 !
 ! OPTICS
 !========
 !
 driver_now=l_optics.and.l_chi
 !
 if (driver_now) call OPTICS_driver(Xen,Xk,q,Xw(3),X(3),Dip)
 if (driver_now) call mem_manager_report
 !
 ! INVERSE DIELECTRIC FUNCTIONS
 !===============================
 !
 driver_now=l_em1s.or.l_em1d.or.l_ppa
 !
 if (driver_now) i_err=X_dielectric_matrix(Xen,Xk,q,X(i_X_kind),Xw(i_X_kind),Dip,.false.)
 if (driver_now) call mem_manager_report
 !
 ! HARTREE-FOCK AND VXC
 !======================
 !
 driver_now=l_HF_and_locXC.and..not.any((/l_sc_run,l_eval_collisions,l_real_time/))
 !
 if (driver_now) call XCo_driver(en,k,Xk,q)
 if (driver_now) call mem_manager_report
 !
 ! EXTENDED COLLISIONS (HF & COHSEX)
 !===================================
 !
 driver_now=l_eval_collisions
 !
#if defined _SC || defined _RT
 if (driver_now) call COLLISIONS_eval(en,X,k,Xk,q,Xw(:2),Dip)
 if (driver_now) call mem_manager_report
#endif
 !
 ! SELF-ENERGIES
 !===============
 !
 driver_now=l_gw0.or.l_life
 !
 if (driver_now) call QP_driver(X(i_X_kind),Xen,Xk,en,k,q,Xw(i_X_kind),Dip)
 if (driver_now) call mem_manager_report
 !
 ! BETHE-SALPETER
 !================
 !
 driver_now=(l_optics.and.l_bse).or.l_bss
 !
 if (driver_now) call K_driver(Xen,Ken,Xk,q,X(i_X_kind),Xw(i_X_kind),Dip)
 if (driver_now) call mem_manager_report
 !
 ! GPL_EXCLUDE_START
 !
 ! ACFDT
 !=======
 !
 driver_now=l_acfdt
 !
 if (driver_now) call acfdt_tot_energy(Xen,Xk,En,k,q,X(1))
 if (driver_now) call mem_manager_report
 !
#if defined _SC
 !
 ! SC
 !====
 !
 driver_now=l_sc_run
 !
 if (driver_now) call SC_driver(X(:2),Xw(:2),Xk,en,k,q,Dip)
 if (driver_now) call mem_manager_report
 !
#endif
 !
#if defined _RT
 !
 ! RT
 !====
 !
 driver_now=l_real_time
 !
 if (driver_now) call RT_driver(en,X(:2),k,q,Dip)
 if (driver_now) call mem_manager_report
 !
#endif
 !
#if defined _ELPH
 !
 ! Electron-Phonon Hamiltonian
 !=============================
 !
 driver_now=l_elph_hamiltonian
 !
 if (driver_now) call ELPH_Hamiltonian(en,k,q)
 if (driver_now) call mem_manager_report
 !
#endif
 !
#if defined _SCALAPACK 
 !
 ! SLK TESTER
 !============
 !
 driver_now=l_slk_test
 !
 if (driver_now) call SLK_test(en,k,q)
 if (driver_now) call mem_manager_report
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
#if defined _TIMING
 !
 ! Timing Overview
 !=================
 call section('*','Timing Overview')
 call timing_overview(LIST=global_list,MAIN_NAME="yambo")
#endif
 !
#if defined _MEM_CHECK 
 !
 ! Memory Overview
 !=================
 call section('*','Memory Overview')
 call MEM_manager("",REPORT="final")
 !
#endif
 !
 ! GAME OVER :(
 !==============
 call section('X','Game Over & Game summary')
 !
 ! Update INIT defs
 !
 i_err=INIT(en,q,k,X,Xw,Dip,instr,lnstr,.false.,.true.)
 !
 ! Finalize
#if defined _TIMING
 call timing_deallocate( )
#endif
 call it_reset(-1)
 !
 contains
  !
  subroutine mem_manager_report
#if defined _MEM_CHECK 
    call MEM_manager("",REPORT="check")
#endif
  end subroutine mem_manager_report
  !
end function

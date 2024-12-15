!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
integer function yambo_sc(np,pid,lnstr,iinf,iind,iod,icd,ijs,instr,inf,ind,od,com_dir,js)
 !
 !Y88b    /   e           e    e      888~~'    ,88~-_   
 ! Y88b  /   d8b         d8b  d8b     888   |  d888   '  
 !  Y88b/   /Y88b       d888bdY88b    888 _/  88888    | 
 !   Y8Y   /  Y88b     / Y88Y Y888b   888  '  88888    | 
 !    Y   /____Y88b   /   YY   Y888b  888   |  Y888   /  
 !   /   /      Y88b /          Y888b 888__/    `88_-~   
 !
 use drivers,       ONLY:l_HF_and_locXC,l_sc_run,l_eval_collisions,l_real_time,&
&                        l_optics,l_chi,l_em1s,l_ppa,l_mpa,l_bss,l_bse,l_em1d,l_X,&
&                        l_gw0,l_life,l_acfdt,l_dipoles,l_nl_optics,l_elel_corr,l_gpu_test,&
&                        l_slk_test
 use drivers,       ONLY:l_elph_Hamiltonian
 use gpu_m,         ONLY:GPU_test_dim
 use X_m,           ONLY:X_t,i_X_kind_existing_DB
 use DIPOLES,       ONLY:DIPOLE_t
 use parallel_m,    ONLY:myid,ncpu
 use it_tools,      ONLY:it_reset
 use it_m,          ONLY:infile
 use QP_m,          ONLY:QP_ng_Sx,QP_ng_SH
 use wave_func,     ONLY:wf_ng
 use electrons,     ONLY:levels,E_reset
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use frequency,     ONLY:w_samp
 !
 implicit none
 !
 type(levels) ::en,Xen,Ken,DIPen
 type(bz_samp)::q,k,Xk
 ! 
 ! 4 response function & energies types: x s d p
 !
 type(X_t)    ::X(5) 
 type(w_samp) ::Xw(5)
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
 integer           ::i_err,i_X_kind,ID,VB(2),CB(2)
 logical           ::launch_me
 character(len=20) ::tmp_char
 !
 ! Externals
 !
 integer, external ::X_dielectric_matrix,INIT_sc
 !
 ! Presets
 !
 ncpu    = np
 myid    = pid
 infile  = inf
 yambo_sc= 0
 iinf    = 0 
 !
 ! ... Internal Defaults
 !
 call SET_defaults_sc(instr,ind,od,com_dir)
 !
 ! ... C_driver -> YPP information transfer
 !
 call C_driver_transfer()
 !
 call it_reset(1)
#if defined _MEM_CHECK 
 call MEM_defaults( )
#endif
 call SET_job_strings_and_dirs(js)
 !
 ! not resetted in INIT call
 !
 call bz_samp_reset(Xk) 
 call E_reset(Xen) 
 call E_reset(Ken) 
 call E_reset(DIPen) 
 !
 ! INPUT FILE PARSING
 !====================
 !
 iinf=INIT_sc(en,q,k,X,Xw,Dip,instr,lnstr,.false.,.false.)
 !
 ! iinf=0 => No error message from driver.c 
 !
 if (iinf/=0) then
   yambo_sc=iinf
   return
 endif
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
 launch_me= .not.(l_slk_test.or.l_gpu_test)
 if(launch_me) call setup_sc(en,Xen,Ken,k,Xk)
 !
 ! Update the default variables
 !
 if(launch_me) i_err=INIT_sc(en,q,k,X,Xw,Dip,instr,lnstr,.true.,.false.)
 !
 ! BZ sampling
 !==============
 !
 if(launch_me) call bz_samp_indexes(en,k,Xk,q,.false.,.true.,.true.) 
 !
 ! COLOUMB DRIVER
 !================
 ! In non-linear optics we do not need bare_qpg
 ! it will be loaded only if required
 !
 launch_me= .not.(l_slk_test.or.l_gpu_test.or.l_nl_optics)
 if(launch_me) call col_driver(maxval((/wf_ng,QP_ng_Sx,QP_ng_SH/)),q)
 !
 ! MAGN and DEN
 !=============
 !
 !launch_me= .not.(l_slk_test.or.l_gpu_test)
 !if(launch_me) call eval_Mtot(en,k,q)
 !
 ! reset the section numbering
 !=============================
 !
 call section('*','') 
 !
 ! Check USER band selection degeneracy breaking
 !===============================================
 !
 call LEVELS_respect_degenerations_sc(en,Dip,X)
 !
 ! QP and RT CORRECTIONS
 !=======================
 !
 launch_me= .not.(l_slk_test.or.l_gpu_test)
 if(launch_me) call External_corrections(en,Xen,Ken,DIPen,Xk,k,X)
 !
 ! DIPOLES
 !=========
 !
 launch_me= any((/l_em1d,l_em1s,l_ppa,l_mpa,l_optics,l_life.and.l_elel_corr,l_dipoles,l_real_time,&
&                    l_sc_run,l_nl_optics/)).and.Dip%iq(1)==1
 !
 if (launch_me) call DIPOLE_driver(DIPen, Xk, q, Dip)
 if (launch_me) call mem_manager_report
 !
 ! OPTICS
 !========
 !
 launch_me=l_optics.and.l_chi
 !
 if (launch_me) call OPTICS_driver(Xen,Xk,q,Xw(3),X(3),Dip)
 if (launch_me) call mem_manager_report
 !
 ! INVERSE DIELECTRIC FUNCTIONS
 !===============================
 !
 ! ... select the response function kind
 i_X_kind=0
 if ( l_real_time.or.l_sc_run.or.l_X      ) i_X_kind=1
 if ( l_em1s                              ) i_X_kind=2
 if ( l_em1d .or.(l_life.and.l_elel_corr) ) i_X_kind=3
 if ( l_ppa                               ) i_X_kind=4
 if ( l_mpa                               ) i_X_kind=5
 if ( i_X_kind==0 ) then
   i_X_kind=1
   if ( i_X_kind_existing_DB >0 ) i_X_kind=i_X_kind_existing_DB
 endif
 !
 launch_me=l_em1s.or.(l_em1d.and..not.l_life).or.l_ppa.or.l_X.or.l_mpa
 !
 if (l_nl_optics.and.launch_me) call error('Please run the calculation of em1s/d/x in a separate run')
 !
 if (launch_me) i_err=X_dielectric_matrix(Xen,Xk,q,X(i_X_kind),Xw(i_X_kind),Dip,.FALSE.,.FALSE.)
 if (launch_me) call mem_manager_report
 !
 ! HARTREE-FOCK AND VXC
 !======================
 !
 launch_me=l_HF_and_locXC.and..not.any((/l_sc_run,l_eval_collisions,l_real_time/))
 !
 if (launch_me) call XCo_driver(en,k,Xk,q,.true.,.true.,.false.,-1)
 if (launch_me) call mem_manager_report
 !
 ! EXTENDED COLLISIONS 
 !=====================
 !
 if (l_eval_collisions) then
  tmp_char="self-consistent"
  call COLLISIONS_eval(en,X,k,Xk,q,Xw(:2),Dip,tmp_char)
  call mem_manager_report
endif
!
 ! SELF-ENERGIES
 !===============
 !
 launch_me=l_gw0.or.l_life
 if (launch_me) call QP_driver(X(i_X_kind),Xen,Xk,en,k,q,Xw(i_X_kind),Dip)
 if (launch_me) call mem_manager_report
 !
 ! BETHE-SALPETER
 !================
 !
 launch_me=(l_optics.and.l_bse).or.l_bss
 !
 if (launch_me) then
  call K_set_bands_ranges(Ken,VB,CB)
  call K_driver(Xen,Ken,Xk,q,X(i_X_kind),Xw(i_X_kind),Dip,VB,CB,"none",.false.)
  call mem_manager_report
endif
!
 ! SC
 !====
 !
 if (l_sc_run) call SC_driver(X(:2),Xw(:2),Xk,en,k,q,Dip)
 if (l_sc_run) call mem_manager_report
 !
 ! CLOSING
 !=========
 !
 call CLOSE_the_run(.TRUE.,.TRUE.,.FALSE.)
 !
 i_err=INIT_sc(en,q,k,X,Xw,Dip,instr,lnstr,.false.,.true.)
 !
 call it_reset(-1)
 !
 contains
  !
  subroutine mem_manager_report
#if defined _MEM_CHECK 
    call MEM_report("check")
#endif
  end subroutine mem_manager_report
  !
end function yambo_sc

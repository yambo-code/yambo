
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine SC_start_and_restart(X,E,Xk)
 !
 use electrons,      ONLY:levels,n_sp_pol
 use parser_m,       ONLY:parser 
 use com,            ONLY:msg,warning
 use drivers,        ONLY:l_sc_exx,l_sc_hf,l_sc_cohsex,l_sc_magnetic,l_collisions_IO 
 use units,          ONLY:HARTREE
 use R_lattice,      ONLY:bz_samp,qindx_S,nkibz,G_m_G
 use QP_m,           ONLY:QP_ng_Sx,QP_t,QP_reset,SC_E_threshold,SC_band_mixing,COHSEX_use_empties
 use SC,             ONLY:SC_ng_Sx,SC_X_bands,SC_X_size,l_SC_RESTART,l_NSC_shot,SC_bands,&
&                         SC_potential,E_bare,local_V,SC_iterations,l_TF_precond,&
&                         rho_convergence,E_convergence,SC_rho_threshold,&
&                         SC_iterations,l_SC_cycle,SC_cycle_mixing,WF_G_max,WF_Go_indx,&
&                         SC_fft_size
 use memory_m,       ONLY:mem_est
 use X_m,            ONLY:X_t
 use IO_m,           ONLY:OP_RD_CL,io_control,REP,VERIFY,NONE
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:WF_load,WF_free
 use global_XC,      ONLY:SC_HF,SC_COHSEX,SC_HF
 use collisions_IO,  ONLY:V_kind  
 !
 implicit none
 type(bz_samp)::Xk
 type(levels) ::E
 type(X_t)    ::X(2)  ! Xx Em1s
 !
 ! Workspace I/O
 !
 integer                 ::ID(4),io_E,io_V,io_WF
 integer, external       :: io_SC_components,io_COLLISIONS
 integer, external       :: eval_G_minus_G
 type(QP_t)              :: qp_dummy
 !
 ! Extended collisions
 ! 
 integer                 :: io_COLLISIONS_err(2)
 ! 
 ! Parsering
 !============
 !
 call parser('TF_precondition',l_TF_precond) 
 call parser('UseEbands',      COHSEX_use_empties)
 !
 ! Magnetic logical setup
 !========================
 !
#if defined _MAGNETIC
 !
 if (l_sc_magnetic) call MAG_Hamiltonian(.TRUE.)
 !
#endif
 !
 ! Switch to the user defined potential (if appropriate)
 !========
 !
 call XC_switch()
 !
 ! Define SC internal variables
 !==============================
 !
 SC_ng_Sx  =QP_ng_Sx
 SC_X_bands=X(2)%ib
 SC_X_size =X(2)%ng
 !
 call QP_reset(qp_dummy)
 !
 ! Sectioning
 !============
 !
 if (trim(SC_potential)/='none'.and.trim(SC_potential)/='default') call section('*','Self-Consistent '//trim(SC_potential))
 !
 ! Reference energies
 !====================
 !
 allocate(E_bare(SC_bands,nkibz,n_sp_pol))
 call mem_est("E_bare ",(/size(E_bare)/))
 E_bare=E%E(:SC_bands,:,:)
 !
 ! Perform a first I/O to check which DBs are present. In this VERIFY call
 !=====================
 ! even if the fft_size is not set it is verified anyway as it is defined
 ! by the Sigma_x/Chi RL vectors.
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(1)) ! Energies
 io_E=io_SC_components('E' ,E,ID(1))
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2)) ! WFs
 io_WF=io_SC_components('WF',E,ID(2))
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID(3))  ! Local potential
 io_V=io_SC_components('V' ,E,ID(3))
 !
 ! CORE LOGICALS
 !================
 !
 ! RESTART + SC_CYCLE: when convergence is not reached  in the DBs
 ! RESTART           : when convergence is reached in the DBs to simply report the final energies (Jumping all the SC cycle)
 ! NSC_shot          : to use the DB's LOCAL potential
 ! SC_CYLE           : DEFAULT
 !
 l_SC_RESTART=all((/io_E==0,io_WF==0,io_V==0/))
#if defined _MAGNETIC
 l_SC_RESTART=.FALSE.
#endif
 !
 if (l_SC_RESTART) then
   !
   call msg('nr','[SC] Found   accuracy on density      :',rho_convergence)
   call msg('rn','[SC]                  on energies [ev]:',E_convergence*HARTREE)
   l_SC_cycle=.not.(E_convergence<SC_E_threshold.and.rho_convergence<SC_rho_threshold)
   !
   ! Even if the accuracy on DB is not small enough with 0 iterations
   ! I can skeep the SC cycle
   !
   if (l_SC_cycle) l_SC_cycle=SC_iterations>0
   !
 else
   !
   l_NSC_shot=SC_iterations==1.and.all((/io_E/=0,io_WF/=0,io_V==0.or.io_V==-2/)).and.local_V.and..not.l_SC_cycle
   !
 endif
#if defined _MAGNETIC
 l_NSC_shot=.FALSE.
#endif
 !
 if (.not.l_SC_RESTART.and..not.l_NSC_shot) l_SC_cycle=SC_iterations>0
 !
 if (.not.any((/l_SC_RESTART,l_SC_cycle,l_NSC_shot/))) return
 !
 ! Reporting
 !===========
 !
 call msg('nr','[SC] Self-Consistency (SC)            :',l_SC_cycle)
 call msg('r' ,'[SC] Restart (quick rebuild)          :',l_SC_RESTART.and..not.l_SC_cycle)
 call msg('r' ,'[SC] Restart + SC                     :',l_SC_RESTART.and.l_SC_cycle)
 call msg('r' ,'[SC] Non SC single shot               :',l_NSC_shot)
 call msg('r' ,'[SC] Bands                            :',SC_bands)
 call msg('r' ,'[SC] Bands mixing                     :',SC_band_mixing)
 call msg('r' ,'[SC] Use Thomas-Fermi precondition    :',l_TF_precond) 
 if (l_SC_cycle) then
   call msg('r' ,'[SC] N (N-1) order mixing             :',SC_cycle_mixing)
   call msg('r' ,'[SC] Imposed accuracy on density      :',SC_rho_threshold)
   call msg('rn','[SC]                  on energies [ev]:',SC_E_threshold*HARTREE)
 endif
 !
 if (.not.l_SC_cycle.and..not.l_NSC_shot) return
 !
 ! Dipoles
 !========
 !
 ! before loading the WF in R-space I check and/or calculate
 ! the optical collisions that are done in G-space and would
 ! force the reloading of the WFs
 !
 if (l_sc_exx.and.l_SC_cycle) then
   X(1)%ib(2)=SC_bands
   X(1)%ng=QP_ng_Sx
   call Dipole_driver(E, Xk, X(1), X(1)%q0)
 endif
 !
 ! WF SIZE
 !=========
 !
 if (l_SC_cycle.and.(l_sc_hf.or.l_sc_exx.or.l_sc_cohsex)) then
   WF_Go_indx=maxval(qindx_S(:,:,2)) ! Sigma_x/c
   if (l_sc_cohsex) then
     X(2)%ng=eval_G_minus_G(X(2)%ng,0)
     WF_G_max=max(QP_ng_Sx,maxval(G_m_G))
   endif
   if (l_sc_exx)  then
     WF_G_max=max(QP_ng_Sx,X(1)%ng)
   endif
 else
   WF_G_max=0
   WF_Go_indx=1
 endif
 !
#if defined _MAGNETIC
 !
 if (l_sc_magnetic) then
   !
   ! MAG_Hamiltonian
   !=================
   ! - call WF_derivative which computes the derivative in space G
   ! - load the wf in real space
   ! - compute the magnetic part of the hamiltonian, 
   !   allocate and initialise Ho, write H_magnetic in Ho 
   !
   call MAG_Hamiltonian(.FALSE.)
   !
 endif
 !
#endif
 ! 
 ! WF Loading
 !============
 !
 call WF_load(WF_G_max,WF_Go_indx,(/1,max(SC_bands,E%nbm)/),(/1,nkibz/),title='-SC')
 !
 ! Check FFT size to be consistent with local V in NSC shot
 !       --------
 !
 if (l_NSC_shot.and.SC_fft_size/=fft_size) then
   call warning(' FFT size of local SC potential does not fit. Skipping NSC run.')
   call WF_free()
   l_NSC_shot=.FALSE.
   return
 endif
 !
 SC_fft_size=fft_size
 !
 ! Checking extended COLLISIONS
 !==============================
 !
 if(l_collisions_IO) then
   !
   V_kind=SC_HF
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID(1))
   io_COLLISIONS_err(1)=io_COLLISIONS(0,ID(1))
   V_kind=SC_COHSEX
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID(1))
   io_COLLISIONS_err(2)=io_COLLISIONS(0,ID(1))
   !
   if (l_sc_hf)     l_collisions_IO=io_COLLISIONS_err(1)==0
   if (l_sc_cohsex) l_collisions_IO=all(io_COLLISIONS_err==0) 
   !
 endif
 !
 if (l_collisions_IO) call msg('s' ,'[SC] Extented collisions I/O Used')
 call msg('r' ,'[SC] Extented collisions I/O          :',l_collisions_IO)
 !
end subroutine SC_start_and_restart

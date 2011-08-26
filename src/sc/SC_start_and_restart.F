
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
 use pars,           ONLY:schlen
 use electrons,      ONLY:levels,n_sp_pol
 use parser_m,       ONLY:parser 
 use com,            ONLY:msg,warning
 use drivers,        ONLY:l_sc_exx,l_sc_hf,l_sc_cohsex,l_sc_magnetic,l_collisions_IO,&
&                         l_oep_exact,l_oep_kli,l_oep_slater, l_oep_ceda
 use units,          ONLY:HA2EV
 use R_lattice,      ONLY:bz_samp,nkibz
 use QP_m,           ONLY:SC_E_threshold,SC_band_mixing,COHSEX_use_empties
 use SC,             ONLY:l_SC_RESTART,l_NSC_shot,SC_bands,&
&                         SC_potential,E_bare,local_V,SC_iterations,l_TF_precond,l_oep_iterative,&
&                         rho_convergence,E_convergence,SC_rho_threshold,&
&                         SC_iterations,l_SC_cycle,SC_cycle_mixing,WF_G_max,WF_Go_indx,&
&                         SC_fft_size,E_bare_Ef,l_oep_EWeight
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
 integer                 :: ID(4),io_E,io_V,io_WF
 character(schlen)       :: ch,ch1
 integer, external       :: io_SC_components
 !
 ! Parsering
 !============
 !
 call parser('TF_precondition',l_TF_precond) 
 call parser('OEPItSolver',l_oep_iterative) 
 call parser('UseEbands', COHSEX_use_empties)
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
 ! Dipoles & dimensions
 !======================
 ! 
 call SC_WF_and_dipole_dimensions(X)
 !
 ! Sectioning
 !============
 !
 if (trim(SC_potential)/='none'.and.trim(SC_potential)/='default') call section('*','Self-Consistent '//trim(SC_potential))
 !
 ! Reference energies & Fermi Level
 !==================================
 !
 allocate(E_bare(SC_bands(2),nkibz,n_sp_pol))
 call mem_est("E_bare ",(/size(E_bare)/))
 E_bare=E%E(:SC_bands(2),:,:)
 E_bare_Ef=E%Efermi(1)
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
 !
 if (l_SC_RESTART) then
   !
   call msg('nr','[SC] Found   accuracy on density      :',rho_convergence)
   call msg('rn','[SC]                  on energies [ev]:',E_convergence*HA2EV)
   l_SC_cycle=.not.(E_convergence<SC_E_threshold.and.rho_convergence<SC_rho_threshold)
   !
   ! Even if the accuracy on DB is not small enough with 0 iterations
   ! I can skip the SC cycle
   !
   if (l_SC_cycle) l_SC_cycle=SC_iterations>0
   !
 else
   !
   l_NSC_shot=SC_iterations==1.and.(io_V==0.or.io_V==-2).and.local_V.and..not.l_SC_cycle
   !
 endif
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
   call msg('rn','[SC]                  on energies [ev]:',SC_E_threshold*HA2EV)
 endif
 if (l_sc_exx) then
   if (.not.l_oep_slater) &
&     call msg('r' ,'[SC] Iterative solution of OEP        :',l_oep_iterative)
   if (.not.l_oep_exact) then
     if (l_oep_kli) ch = 'KLI'
     if (l_oep_slater) ch = 'SLATER'
     if (l_oep_ceda) ch = 'CEDA'
     ch1 = trim(ch)
     if (l_oep_EWeight) write(ch,'(a,a)') 'W-',trim(ch1)
     call msg('rn' ,'[SC] Approximate solution of OEP      : ',ch)
   end if
 end if 
 !
 ! Reset WF sizes if not doing an SC_cycle
 !
 if (.not.l_SC_cycle) then
   WF_G_max=0
   WF_Go_indx=1
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
 if (l_sc_exx.and.l_SC_cycle) call Dipole_driver(E, Xk, X(1), X(1)%q0)
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
   ! If exist the non local part of the pseudo then construct kbv
   call MAG_H_pseudo(Xk)
   !
 endif
 !
#endif
 ! 
 ! WF Loading
 !============
 !
 call WF_load(WF_G_max,WF_Go_indx,(/1,max(SC_bands(2),E%nbm)/),(/1,nkibz/),title='-SC')
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
 call COLLISIONS_IO_load(.TRUE.)
 !
 if (l_collisions_IO) call msg('s' ,'[SC] Extented collisions I/O Used')
 call msg('r' ,'[SC] Extented collisions I/O          :',l_collisions_IO)
 !
end subroutine SC_start_and_restart


!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine SC_start_and_restart(X,E,Xk)
 !
 use pars,           ONLY:schlen
 use electrons,      ONLY:levels,n_spin
 use parser_m,       ONLY:parser 
 use com,            ONLY:msg
 use electrons,      ONLY:E_reset,E_duplicate
 use drivers,        ONLY:l_sc_exx,l_sc_magnetic,l_use_Hxc_collisions,&
&                         l_oep_exact,l_oep_kli,l_oep_slater, l_oep_ceda
 use units,          ONLY:HA2EV
 use R_lattice,      ONLY:bz_samp
 use QP_m,           ONLY:SC_E_threshold,SC_band_mixing,COHSEX_use_empties
 use hamiltonian,    ONLY:H_potential,H_kind,H_xc_functional,H_perturbation,    &
&                         WF_G_max,WF_Go_indx,E_reference,l_sc_V_is_local,      &
&                         H_QP_table_setup,E_reference
 use SC,             ONLY:SC_potential,SC_kind,SC_xc_functional,SC_perturbation,&
&                         l_SC_RESTART,l_NSC_shot,SC_bands,l_SC_diagonal,       &
&                         SC_iterations,SC_preconditioning,                     &
&                         l_oep_iterative,rho_convergence,E_convergence,        &
&                         SC_rho_threshold,SC_iterations,l_SC_cycle,            &
&                         SC_fft_size,l_oep_EWeight,SC_cycle_mixing,  &
&                         l_Mean_Potential,l_SC_nl_mix,magn_convergence,l_froze_density
 use X_m,            ONLY:X_t
 use IO_m,           ONLY:OP_RD_CL,io_control,REP,VERIFY,NONE,io_COLLs
 use FFT_m,          ONLY:fft_size
 use collision_ext,  ONLY:COLLISIONS_naming,COLLISIONS_have_HARTREE
 !
#include<memory.h>
 !
 type(levels) ::E
 type(X_t)    ::X(2)  ! Xx Em1s
 type(bz_samp)::Xk
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
 call parser('OEPItSolver',    l_oep_iterative) 
 call parser('UseEbands',      COHSEX_use_empties)
 call parser('Mean_Potential', l_Mean_Potential)
 call parser('SCdiag',         l_SC_diagonal)
 call parser('SCnlMix',        l_SC_nl_mix)
 call parser('FrozeDensity',   l_froze_density)
 !
 ! Preconditioning
 !=================
 if (trim(SC_preconditioning)/="simple".and. &
&    trim(SC_preconditioning)/="none".and. &
&    trim(SC_preconditioning)/="thomas-fermi") SC_preconditioning="none"
 !
 ! Magnetic logical setup
 !========================
 !
#if defined _MAGNETIC
 !
 if (l_sc_magnetic) call MAG_initial_check()
 !
#endif
 !
#if defined _ELECTRIC
 !
 call ELECTR_initial_check(Xk)
 !
 if(.not.associated(Xk%k_table))      call k_build_up_BZ_tables(Xk)
 !
#endif
 !
 ! Switch to the user defined potential (if appropriate)
 !========
 call XC_potentials(H_potential,"SC")
 call XC_additional_SC_potentials()
 !
 SC_potential    =H_potential
 SC_kind         =H_kind
 SC_xc_functional=H_xc_functional
 SC_perturbation =H_perturbation
 !
 ! Dipoles & dimensions
 !======================
 ! 
 call WF_and_dipole_dimensions(X)
 !
 ! Before reading the SC energies save the KS one in E_reference
 !
 call E_reset(E_reference)
 call E_duplicate(E,E_reference)
 !
 ! Sectioning
 !============
 !
 if (trim(SC_potential)/='none'.and.trim(SC_potential)/='default') call section('*','Self-Consistent '//trim(SC_potential))
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
   call msg('nr','[SC] Found   accuracy on magnetization:',magn_convergence)
   call msg('rn','[SC]                  on energies [ev]:',E_convergence*HA2EV)
   l_SC_cycle=.not.(E_convergence<SC_E_threshold.and.rho_convergence<SC_rho_threshold.and.magn_convergence<SC_rho_threshold)
   !
   ! Even if the accuracy on DB is not small enough with 0 iterations
   ! I can skip the SC cycle
   !
   if (l_SC_cycle) l_SC_cycle=SC_iterations>0
   !
 else
   !
   l_NSC_shot=SC_iterations==1.and.(io_V==0.or.io_V==-2).and.l_sc_V_is_local.and..not.l_SC_cycle
   !
 endif
 !
 ! Quasi-Particle setup
 ! ====================
 call H_QP_table_setup(E)
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
 call msg('r' ,'[SC] Use non-local mixing             :',l_SC_nl_mix)
 call msg('r' ,'[SC] Preconditioning                  :',SC_preconditioning) 
 call msg('r' ,'[SC] Diagonal approximation           :',l_SC_diagonal)
 if (l_SC_cycle) then
   call msg('r' ,'[SC] N (N-1) order mixing             :',SC_cycle_mixing)
   call msg('r' ,'[SC] Imposed accuracy on density      :',SC_rho_threshold)
   if(n_spin>1) call msg('r' ,'[SC]                  on magnetization:',SC_rho_threshold)
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
 ! Check FFT size to be consistent with local V in NSC shot
 !       --------
 call fft_setup(WF_G_max,WF_Go_indx,.false.)
 !
 if (l_NSC_shot.and.SC_fft_size/=fft_size) then
   call warning(' FFT size of local SC potential does not fit. Skipping NSC run.')
   l_NSC_shot=.FALSE.
   return
 endif
 !
 SC_fft_size=fft_size
 !
 ! Checking extended COLLISIONS
 !==============================
 !
 call COLLISIONS_naming( SC_kind , "HXC" )
 !
 if (io_COLLs) call COLLISIONS_load(.TRUE.)
 !
 if (l_use_Hxc_collisions) then
   call msg('s' ,'[SC] Extented collisions Used')
   call msg('r' ,'[SC] Extented collisions              :',l_use_Hxc_collisions)
   COHSEX_use_empties=.TRUE.
   if (COLLISIONS_have_HARTREE.and.SC_preconditioning=="thomas-fermi") then
     call warning(" [COLLISIONS] Found Hartree collisions. Switched to simple preconditioning")
     SC_preconditioning="simple"
   endif
 endif
 !
end subroutine SC_start_and_restart

!
!       Copyright (C) 2000-2013 the YAMBO team 
!            http://www.yambo-code.org
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
subroutine RT_initialize(E,k,q)
 ! 
 use drivers,        ONLY:l_use_collisions,l_elph_scatt,l_elel_scatt
 use SC,             ONLY:local_V,SC_bands,RT_nk,RT_ibz_coo,RT_all2ibz,RT_Nk_ibz,RT_nstates
 use pars,           ONLY:SP,pi
 use units,          ONLY:FS2AUT,HA2EV,HBAR_eVfs,AU2VMm1
 use com,            ONLY:msg,warning,error
 use vec_operate,    ONLY:normalize_v
 use plasma,         ONLY:EH_gas
 use electrons,      ONLY:nel,levels,BZ_RIM_nbands,BZ_RIM_tot_nkpts,RIM_is_symmetry_expanded
 use real_time,      ONLY:l_NE_simulate_RPA,Thermal_steps,RT_step,l_NE_with_fields, &
&                         l_NE_simulate_IP,l_RT_diagonalize,NE_steps,NE_tot_time,   &
&                         l_RT_diamagnetic,RT_IO_JPO_steps,RT_IO_time,l_NE_simulate_BSE,&
&                         l_NE_simulate_Irredux,l_RT_needs_XC0,Hfrozen,XCfrozen,    &
&                         l_RT_induced_field,l_NE_simulate_TDDFT,                   &
&                         RT_diagonalization_steps,RT_is_dephased,RAD_LifeTime,       &
&                         Phase_LifeTime,NE_MEM_treshold,RT_IO_steps_DB,            &
&                         G_MEM_steps,Gr_kind,RIM_ibz_coo,RT_IO_JPO_steps,RT_IO_G_steps, &
&                         l_RT_CCA_Kernel,Life_MEM_steps,RT_IO_output_steps,    &
&                         l_RT_uses_E_RIM,RIM_bz_coo,RT_dynamics_steps,RT_dynamics_decay_time,&
&                         RT_dynamics_KIND,l_NE_dynamics
 use IO_m,           ONLY:io_COLLs
 use R_lattice,      ONLY:bz_samp
 use fields,         ONLY:global_gauge,Efield,n_ext_fields,field_is_ok,l_manual_excitation, &
&                         theta_function_iTo_ref,i_Pump,i_Probe,EtoT,Efield_strength
 use parser_m,       ONLY:parser
 implicit none
 !
 type(levels), intent(in) :: E
 type(bz_samp),intent(in) :: q,k
 !
 ! Work space
 !
 integer                  :: i1,rest,ik,IO_steps(3),N_T_samplings
 character(5)             :: MSG_ch
 real(SP)                 :: T_memory
 logical                  :: l_Full_RT_T_evolution,l_compute_fluence,l_field_is_a_pulse 
 integer,         external:: RT_k_grid,RT_SET_lifetimes_evaluation_plan
 !
 ! Logicals
 !==========
 !
 call parser('Diamagnetic',          l_RT_diamagnetic)
 call parser('InducedField' ,        l_RT_induced_field)
 call parser('RTfreezeH',            Hfrozen)
 call parser('RTfreezeXC',           XCfrozen)
 call parser('RT_T_evol',            l_Full_RT_T_evolution)
 call parser('RTpumpEhEn',           l_manual_excitation)
 !
 l_RT_CCA_Kernel       =.not.l_Full_RT_T_evolution.and.l_elph_scatt
 !
 l_NE_simulate_BSE     =.FALSE.
 l_NE_simulate_TDDFT   =.FALSE.
 !
 l_RT_diagonalize      =RT_diagonalization_steps>1
 !
 call UPDATE_flags(.FALSE.)
 !
 ! If I am doing a NE loop in all cases I need to evaluate XC or Hartree correspoding to the
 ! starting basis. During the thermalization, instead, the potentials are updated
 ! and only T+V_ion must be stored in Ho.
 !
 l_RT_needs_XC0=Thermal_steps==0
 !
 ! Use NE_tot_time if provided
 !
 if (NE_tot_time>0.) NE_steps=max(NE_steps,int(NE_tot_time/RT_step)+1)
 !
 ! Set a number of steps. i1=1(J,P,OCCs)  i1=2(GF) i1=3(output).
 ! Negative value forces the step to be set automatically
 !==============================================================
 !
 do i1=1,3
   !
   if (RT_IO_time(i1)>0) IO_steps(i1) =nint(RT_IO_time(i1)/RT_step)
   !
   if(IO_steps(i1)>0.and.mod(Thermal_steps+NE_steps-1,IO_steps(i1))/=0) then
     !
     if (i1>1) cycle
     !
     rest=mod(Thermal_steps+NE_steps-1,IO_steps(i1))
     if(NE_steps/=0) then
       NE_steps = NE_steps+IO_steps(i1)-rest
     elseif (NE_steps==0.and.Thermal_steps/=0) then
       Thermal_steps = Thermal_steps+IO_steps(i1)-rest
     endif
     !
     ! Set automatically RT_IO_time and RT_IO_steps 
     !
   else if (RT_IO_time(i1)<0) then
     !
     RT_IO_time(i1) =max(NE_tot_time/1000._SP,RT_step)
     IO_steps(i1)=nint(RT_IO_time(i1)/RT_step)
     !
     if (i1>1) cycle
     !
     rest=mod(Thermal_steps+NE_steps-1,IO_steps(i1))
     if(rest/=0) NE_steps = NE_steps+IO_steps(i1)-rest
     !
     if (RT_IO_steps_DB>0.and.IO_steps(i1)/=RT_IO_steps_DB) then
       call warning('IO steps read from the existing databases')
       IO_steps(i1)   =RT_IO_steps_DB
     endif
     !
   endif
   !
   RT_IO_time(i1) =IO_steps(i1)*RT_step
   !
 enddo
 !
 NE_tot_time=RT_step*(NE_steps-1)
 !
 RT_IO_JPO_steps    = IO_steps(1)
 RT_IO_G_steps      = IO_steps(2)
 RT_IO_output_steps = IO_steps(3)
 !
 ! Dephasing Matrix. Including user-defined matrix-like dephasing factors, constant
 !==================
 ! radiative/phase, polaronic based
 !
 call section('=','Dephasing matrix setup')
 !
 call RT_Dephasing_Matrix(E) 
 !
 if (.not.RT_is_dephased) call warning('No Dephasing provided. Is the simulation meaningfull ?')
 !
 ! Memory Depth (G_lesser)
 !=========================
 !
 T_memory=0._SP
 if (l_elph_scatt.and.NE_MEM_treshold>0.) then
   T_memory=-1./minval( abs(E%W(SC_bands(1):SC_bands(2),:,:)))*log(NE_MEM_treshold)
   G_MEM_steps=T_memory/RT_step
 endif
 !
 ! External fields checks
 !========================
 !
 l_NE_with_fields=.false.
 !
 do i1=1,n_ext_fields
   !      
   Efield(i1)%ef_name=trim(Efield(i1)%ef_name)      
   !
   if((Efield(i1)%ef_name/='none')) then
     !
     if (.not.field_is_ok( Efield(i1)) ) call error('Field '//trim(Efield(i1)%ef_name)//' not supported')
     !      
     l_NE_with_fields=.true.
     !
     if(Efield(i1)%ef_name/='DELTA'.and.Efield(i1)%frequency(1)==0._SP) &
&      call error(" External "//trim(Efield(i1)%ef_name)//" field has zero frequency.")
     !
     if(Efield(i1)%ef_name/='DELTA'.and.Efield(i1)%damping==0) &
&      call error(" External "//trim(Efield(i1)%ef_name)//" field has zero damping.")
     ! 
     if(Efield(i1)%intensity==0._SP) &
&      call error(" External "//trim(Efield(i1)%ef_name)//" field has zero intensity.")
     !
     if( all( abs(Efield(i1)%versor(:))<epsilon(1.) ) ) &
&      call error(" External "//trim(Efield(i1)%ef_name)//" field has undefined direction.")
     !
     Efield(i1)%versor   =normalize_v(Efield(i1)%versor)
     Efield(i1)%amplitude=Efield_strength(Efield(i1)%intensity,'AU')
     !
     Efield(i1)%t_initial_indx=int(Efield(i1)%t_initial/RT_step)+1
     Efield(i1)%t_initial=real(Efield(i1)%t_initial_indx-1)*RT_step
     !
     call SC_check_symmetries(Efield(i1)%versor)
     !
     l_field_is_a_pulse= (Efield(i1)%ef_name=='DELTA') .or. (Efield(i1)%ef_name=='QSSIN') &
&                   .or. (Efield(i1)%ef_name=='GAUSS')
     !
     if ( l_field_is_a_pulse ) then
       call RT_Fluence(i1)
     else
       Efield(i1)%t_final_indx=NE_steps
       Efield(i1)%t_final=RT_step
     endif
     !
   endif
   !
 enddo
 !
 l_NE_dynamics=l_NE_with_fields.or.l_manual_excitation
 !
 ! Check on the PROBE/PUMP frequencies
 !====================================
 call RT_Field_Commensurable_Frequencies()
 !
 if ( .not. l_NE_dynamics ) then
   NE_steps=0
   if (Thermal_steps==0) call warning('Skipping simulation. No Ext fields or thermalization provided')
 endif
 !
#if defined _ELPH
 !
 ! Electron-phonon
 ! ===============
 !
 if (l_elph_scatt) then
   !
   call section('=','El-Ph matrix elements loading and setup')
   !                 =======================================
   call RT_ELPH_initialize(k,q)
   !
 endif
 !
#endif
 !
 ! Memory Depth & Simulation points (Lifetimes)
 !=============================================
 !
 Life_MEM_steps=1
 if (RT_dynamics_steps(1)>0.) Life_MEM_steps=3
 N_T_samplings=RT_SET_lifetimes_evaluation_plan( )
 !
 ! Gauge
 !=======
 !
 ! If the gauge is length I need E(t) whose definition includes a delta(t). 
 ! The numerical definition of delta(t) in mod_fields depends on the 
 ! definition of a refence time theta_function_iTo_ref. Using a small time step
 ! or an A(t) that is zero at the start-up of the field 
 ! this parameter does not make any difference. However in the case of a DELTA field
 ! a correct definition is needed to obtain the same result in both gauges.
 ! 
 if (trim(global_gauge) == 'length' ) theta_function_iTo_ref=0
 !
 ! Checking extended COLLISIONS
 !==============================
 !
 call msg('r',' ')
 !
 if (l_elel_scatt) then
   call PLASMA_build_up(E,k,q)
   call PLASMA_parallel_setup(EH_gas)
 endif
 !
 call COLLISIONS_load(.TRUE.)
 !
 call UPDATE_flags(.TRUE.)
 !
 ! RIM support
 !=============
 l_RT_uses_E_RIM=BZ_RIM_tot_nkpts>0.and.BZ_RIM_nbands>=SC_bands(2).and..not.RIM_is_symmetry_expanded
 !
 if (l_RT_diagonalize.and.l_RT_uses_E_RIM) l_RT_diagonalize=.FALSE.
 !
 ! Number of K-points
 !====================
 allocate(RIM_ibz_coo(k%nibz,2))
 allocate(RIM_bz_coo(k%nbz,2))
 allocate(RT_ibz_coo(k%nibz,2))
 allocate(RT_Nk_ibz(k%nibz))
 if (l_RT_uses_E_RIM) then
   RT_nk=RT_k_grid(E,k)
 else
   RT_nk=k%nibz
   RT_Nk_ibz=1
   allocate(RT_all2ibz(k%nibz))
   do ik=1,k%nibz
     RIM_ibz_coo(ik,:)=ik
     RT_ibz_coo(ik,:)=ik
     RT_all2ibz(ik)=ik
   enddo
 endif
 RT_nstates=RT_nk*(SC_bands(2)-SC_bands(1)+1)
 !
 ! LOG
 ! ===
 !
 call section('=','Messaging')
 !
 call msg( 'r','---General---')
 call msg( 'r', 'Simulation time        [fs]:',(/-real(Thermal_steps),real(NE_steps-1)/)*RT_step/FS2AUT)
 call msg( 'r', 'Step length            [fs]:',RT_step/FS2AUT)
 call msg( 'r', 'NE Steps                   :',NE_steps)
 call msg( 'r', 'NE time                [fs]:',(NE_steps-1)*RT_step/FS2AUT)
 call msg( 'r', 'Memory depth(G)        [fs]:',T_memory/FS2AUT)
 call msg( 'r', 'Memory steps(G)            :',G_MEM_steps)
 call msg( 'r', 'Thermalization steps       :',Thermal_steps)
 !
 if (RT_dynamics_steps(1)>0.) then
   call msg( 'nr','---Dynamics---')
   call msg( 'r', 'Sampling                       :',RT_dynamics_KIND)
   call msg( 'r', 'Steps length (inter/intra) [fs]:',RT_dynamics_steps/FS2AUT)
   if (RT_dynamics_decay_time>0.) call msg( 'r', 'Distribution decay         [fs]:',&
&                                                RT_dynamics_decay_time/FS2AUT)
   call msg( 'r', 'Time points                    :',N_T_samplings*Life_MEM_steps)
 endif
 !
 call msg( 'nr','---Flags (if any)---')
 if (l_NE_simulate_RPA)   call msg( 'r','Simulate RPA               :',l_NE_simulate_RPA)
 if (l_NE_simulate_BSE)   call msg( 'r','Simulate BSE               :',l_NE_simulate_BSE)
 if (l_NE_simulate_TDDFT) call msg( 'r','Simulate TD-DFT            :',l_NE_simulate_TDDFT)
 if (l_elph_scatt)        call msg( 'r','El-Ph Carrier scattering   :',l_elph_scatt)
 if (l_elel_scatt)        call msg( 'r','El-El Carrier scattering   :',l_elel_scatt)
 !
 call msg( 'nr','---I/O---')
 call                          msg('r', 'T between I/O of J,P,f [fs]:',RT_IO_time(1)/FS2AUT)
 call                          msg('r', '          I/O of   GFs [fs]:',RT_IO_time(2)/FS2AUT)
 call                          msg('r', '               outputs [fs]:',RT_IO_time(3)/FS2AUT)
 if (l_use_collisions)    call msg( 'r','Extended collisions   [I/O]:',io_COLLs)
 !
 call msg( 'nr','---Damping and retarded GF---')
 call msg( 'r','Retarded Green`s function   :',Gr_kind)
 call msg( 'r', 'Damping (Phase)    [fs/meV]:',(/Phase_LifeTime/FS2AUT,HBAR_eVfs/Phase_LifeTime*FS2AUT*1000._SP/))
 call msg( 'r', 'Damping (Radiative)[fs/meV]:',(/RAD_LifeTime/FS2AUT,   HBAR_eVfs/RAD_LifeTime*FS2AUT*1000._SP/))
 !
 call msg( 'nr','---RIM---')
 call msg( 'r', 'Energy RIM support           :',l_RT_uses_E_RIM)
 if (l_RT_uses_E_RIM) then
   call msg( 'r', 'K-points            (TOTAL):',BZ_RIM_tot_nkpts)
   call msg( 'r', '                     (USED):',RT_nk)
 endif
 !
 if (l_NE_with_fields) then
   call msg( 'nr','---Fields---')
   call msg('r','Gauge                       :',trim(global_gauge))
   call msg('r','Pump/Probe periodicity  [fs]:',EtoT(E=maxval(Efield(:)%W_step))/FS2AUT)
   call msg('r','Pump/Probe energy step  [eV]:',maxval(Efield(:)%W_step)*HA2EV)
   if ( l_field_is_a_pulse ) then
     call msg('r','Max Fluence        [kJ/cm^2]:',Efield(i_Pump)%Fluence( Efield(i_Pump)%t_final_indx ))
     call msg('r','           reached time [fs]:',Efield(i_Pump)%t_final/FS2AUT)
   endif
   do i1=1,n_ext_fields
     if (i_Pump==i_Probe.and.i1/=i_Probe.and..not.any(trim(Efield(i1)%ef_name)==(/'SIN    ','ANTIRES','RES    '/)) ) cycle
     MSG_ch="Pump"
     if (i1==i_Probe) MSG_ch="Probe"
     call msg('r', MSG_ch//' field                       :'//trim(Efield(i1)%ef_name))
     call msg('r', MSG_ch//' energy window/period [eV/fs]:',(/Efield(i1)%frequency*HA2EV,&
&                  2._SP*pi*HBAR_evfs/(Efield(i1)%frequency*HA2EV)/))
     call msg('r', MSG_ch//' elemental oscillation   [fs]:',EtoT(E=Efield(i1)%frequency(2)-Efield(i1)%frequency(2))/FS2AUT)
     call msg('r', MSG_ch//' damping                 [fs]:',Efield(i1)%damping/FS2AUT)
     call msg('r', MSG_ch//' energy steps                :',Efield(i1)%n_frequencies)
     call msg('r', MSG_ch//' electric field         [V/m]:',Efield(i1)%amplitude*AU2VMm1)
   enddo
 endif
 !
 contains
   !
   subroutine UPDATE_flags(COLLISIONS_checked)
     !
     logical :: COLLISIONS_checked
     !
     if (COLLISIONS_checked) then
       if (l_use_collisions.and..not.l_elel_scatt) then
         XCfrozen=.FALSE.
         if (Hfrozen) call warning('COLLISIONS used but Hartree frozen')
       endif
     endif
     !
     if (l_elel_scatt) XCfrozen=.TRUE.
     !
     l_NE_simulate_RPA     =.not.Hfrozen.and.     XCfrozen
     l_NE_simulate_IP      =     Hfrozen.and.     XCfrozen
     l_NE_simulate_Irredux =     Hfrozen.and..not.XCfrozen
     if (                   .not.Hfrozen.and..not.XCfrozen) then
       l_NE_simulate_BSE     =.not.local_V
       l_NE_simulate_TDDFT   =local_V
     endif
   end subroutine
   !
end subroutine RT_initialize

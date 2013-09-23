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
 use drivers,        ONLY:l_eval_collisions,l_ph_corr,l_carrier_dynamics
 use SC,             ONLY:local_V,SC_bands,RT_nk
 use pars,           ONLY:SP,pi
 use units,          ONLY:FS2AUT,HA2EV,HBAR_eVfs,AU2VMm1
 use com,            ONLY:msg,warning,error
 use vec_operate,    ONLY:normalize_v
 use electrons,      ONLY:nel,levels,BZ_RIM_nbands,BZ_RIM_tot_nkpts
 use real_time,      ONLY:l_NE_simulate_RPA,Thermal_steps,RT_step,l_NE_with_fields, &
&                         l_NE_simulate_IP,l_RT_diagonalize,NE_steps,NE_tot_time,   &
&                         l_RT_diamagnetic,RT_IO_steps,RT_IO_time,l_NE_simulate_BSE,&
&                         l_NE_simulate_Irredux,l_RT_needs_XC0,Hfrozen,XCfrozen,    &
&                         l_RT_induced_field,RT_dynamics_jumps,l_NE_simulate_TDDFT, &
&                         RT_diagonalization_steps,RT_is_dumped,RAD_LifeTime,       &
&                         Phase_LifeTime,NE_MEM_Treshold,RT_IO_steps_DB,            &
&                         G_MEM_steps,l_RT_skip_GF_IO,l_RT_skip_OCC_IO,             &
&                         l_RT_skip_JandP_IO,l_G_retarded_DB,Gr_kind,         &
&                         Gr_bands,l_RT_CCA_Kernel,Life_MEM_steps,Fluence,l_RT_uses_E_RIM
 use collisions_IO,  ONLY:l_STORE_COLLISIONS
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp,nkibz
 use fields,         ONLY:global_gauge,Efield,n_ext_fields,field_is_ok,ef_i_time,&
&                         theta_function_iTo_ref,i_Pump,i_Probe,EtoT,Efield_strength
 use parser_m,       ONLY:parser
 implicit none
 !
 type(levels), intent(in) :: E
 type(bz_samp),intent(in) :: q,k
 !
 ! Work space
 !
 integer                  :: i1,rest,nk_todo
 character(5)             :: MSG_ch
 real(SP)                 :: T_memory
 logical                  :: l_Full_RT_T_evolution,l_compute_fluence
 !
 ! Logicals
 !==========
 !
 call parser('Diamagnetic',          l_RT_diamagnetic)
 call parser('InducedField' ,        l_RT_induced_field)
 call parser('RTfreezeH',            Hfrozen)
 call parser('RTfreezeXC',           XCfrozen)
 call parser('SkipGFIO',             l_RT_skip_GF_IO)
 call parser('SkipOCCIO',            l_RT_skip_OCC_IO)
 call parser('SkipJPIO',             l_RT_skip_JandP_IO)
 call parser('RT_T_evol',            l_Full_RT_T_evolution)
 !
 l_RT_CCA_Kernel=.not.l_Full_RT_T_evolution
 l_NE_simulate_BSE     =.FALSE.
 l_NE_simulate_TDDFT   =.FALSE.
 !
 l_G_retarded_DB=Gr_bands(1)>0
 !
 if (l_G_retarded_DB) then
   l_G_retarded_DB=l_carrier_dynamics.or.l_ph_corr
   if (Gr_bands(1)>SC_bands(1).or.Gr_bands(2)<SC_bands(2)) l_G_retarded_DB=.FALSE.
 endif
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
 if (NE_tot_time>0.) NE_steps=max(NE_steps,int(NE_tot_time/RT_step))
 !
 ! Set a number of steps compatible with the cache.
 ! Negative value forces the step to be set automatically
 !========================================================
 !
 if (RT_IO_time>0) RT_IO_steps=nint(RT_IO_time/RT_step)
 !
 if(RT_IO_steps>0.and.mod(Thermal_steps+NE_steps,RT_IO_steps)/=0) then
   !
   rest=mod(Thermal_steps+NE_steps,RT_IO_steps)
   if(NE_steps/=0) then
     NE_steps = NE_steps+RT_IO_steps-rest
   elseif (NE_steps==0.and.Thermal_steps/=0) then
     Thermal_steps = Thermal_steps+RT_IO_steps-rest
   endif
   !
   ! Set automatically RT_IO_time and RT_IO_steps 
   !
 else if (RT_IO_time<0) then
   RT_IO_time =max(NE_tot_time/1000._SP,RT_step)
   RT_IO_steps=nint(RT_IO_time/RT_step)
   !
   rest=mod(Thermal_steps+NE_steps,RT_IO_steps)
   if(rest/=0) NE_steps = NE_steps+RT_IO_steps-rest
   !
   if (RT_IO_steps_DB>0.and.RT_IO_steps/=RT_IO_steps_DB) then
     call warning('IO steps read from the existing databases')
     RT_IO_steps=RT_IO_steps_DB
     RT_IO_time =RT_IO_steps*RT_step
   endif
   !
 endif
 !
 NE_tot_time=RT_step*NE_steps
 !
 ! Dephasing Matrix. Including user-defined matrix-likfe dephasing factors, constant
 !==================
 ! radiative/phase, polaronic based
 !
 call section('=','Dephasing matrix setup')
 !
 call RT_Dephasing_Matrix(E) 
 !
 if (.not.RT_is_dumped) call warning('No Dephasing provided. Is the simulation meaningfull ?')
 !
 ! Memory Depth
 !============== 
 !
 T_memory=0._SP
 if (l_ph_corr.and.NE_MEM_Treshold>0.) then
   T_memory=-1./minval( abs(E%W(SC_bands(1):SC_bands(2),:,:)))*log(NE_MEM_Treshold)
   G_MEM_steps=T_memory/RT_step
 endif
 !
 ! Memory Depth
 !============== 
 !
 if (RT_dynamics_jumps<0) RT_dynamics_jumps=0
 Life_MEM_steps=1
 if (RT_dynamics_jumps>0) then
   Life_MEM_steps=3
 endif
 !
 ! External fields checks
 !========================
 !
 l_NE_with_fields=.false.
 l_compute_fluence=.false.
 ef_i_time=0
 !
 do i1=1,n_ext_fields
   !      
   Efield(i1)%ef_name=trim(Efield(i1)%ef_name)      
   !
   if((Efield(i1)%ef_name/='none')) then
     !
     if (.not.field_is_ok( Efield(i1)) ) call error('Field '//trim(Efield(i1)%ef_name)//' not supported')
     !      
     ef_i_time(i1)=Efield(i1)%t_initial/RT_step
     !      
     l_NE_with_fields=.true.
     !
     if(i1==i_Pump) l_compute_Fluence= (Efield(i1)%ef_name=='DELTA') .or. (Efield(i1)%ef_name=='QSSIN') &
&                                 .or. (Efield(i1)%ef_name=='GAUSS')
     !
     Efield(i1)%versor         =normalize_v(Efield(i1)%versor)
     !
     Efield(i1)%int_t_initial  =int(Efield(i1)%t_initial/RT_step)+1
     !
     Efield(i1)%amplitude=Efield_strength(Efield(i1)%intensity,'AU')
     !
     call SC_check_symmetries(Efield(i1)%versor)
     !
     if(Efield(i1)%ef_name/='DELTA'.and.Efield(i1)%frequency(1)==0._SP) &
&      call warning(" External "//trim(Efield(i1)%ef_name)//" field has zero frequency.")
     !
     if(Efield(i1)%ef_name/='DELTA'.and.Efield(i1)%damping==0) &
&      call warning(" External "//trim(Efield(i1)%ef_name)//" field has zero damping.")
     ! 
     if(Efield(i1)%intensity==0._SP) &
&      call warning(" External "//trim(Efield(i1)%ef_name)//" field has zero intensity.")
     !
     if(i1==i_Pump) l_compute_fluence=l_compute_fluence.and..not.(Efield(i1)%intensity==0._SP)
     !
   endif
   !
 enddo
 !
 ! Check on the PROBE/PUMP frequencies
 !==============================================
 call RT_Field_Commensurable_Frequencies()
 !
 if (.not.l_NE_with_fields) then
   NE_steps=0
   if (Thermal_steps==0) call warning('Skipping simulation. No Ext fields or thermalization provided')
 endif
 !
#if defined _ELPH
 !
 ! Electron-phonon
 ! ===============
 !
 if (l_ph_corr) then
   !
   call section('=','El-Ph matrix elements loading and setup')
   !                 =======================================
   call RT_ELPH_initialize(k,q)
   !
 endif
 !
#endif
 !
 ! LOG
 ! ===
 !
 call section('=','Messaging')
 !
 if(l_NE_with_fields) call msg('rn','[RT] Gauge                      :',trim(global_gauge))
 !
 call msg( 'r', '[RT] NE Steps                   :',NE_steps)
 call msg( 'r', '[RT] NE time                [fs]:',NE_steps*RT_step/FS2AUT)
 call msg( 'r', '[RT] Memory depth           [fs]:',T_memory/FS2AUT)
 call msg( 'r', '[RT] Memory steps               :',G_MEM_steps)
 call msg( 'r', '[RT] Thermalization steps       :',Thermal_steps)
 call msg( 'r', '[RT] K-points evolved      [o/o]:',real(nk_todo,SP)/real(nkibz)*100._SP)
 call msg( 'r', '[RT] Step length            [fs]:',RT_step/FS2AUT)
 call msg( 'r', '[RT] Damping (Phase)    [fs/meV]:',(/Phase_LifeTime/FS2AUT,HBAR_eVfs/Phase_LifeTime*FS2AUT*1000._SP/))
 call msg( 'r', '[RT] Damping (Radiative)[fs/meV]:',(/RAD_LifeTime/FS2AUT,   HBAR_eVfs/RAD_LifeTime*FS2AUT*1000._SP/))
 if(NE_steps>0) then
   call msg('rn','[RT] Simulation time        [fs]:',(/-real(Thermal_steps)*RT_step,RT_step*real(NE_steps)/)/FS2AUT)
   call msg('r', '[RT] Time between I/O`s     [fs]:',RT_IO_time/FS2AUT)
   call msg('rn','[RT] Electron-gas plasma frequency [eV]:',sqrt(4._SP*pi*nel/DL_vol)*HA2EV)
 endif
 call msg('r', '[RT] Pump/Probe periodicity     [fs]:',EtoT(E=maxval(Efield(:)%W_step))/FS2AUT)
 call msg('r', '[RT]            energy step     [eV]:',maxval(Efield(:)%W_step)*HA2EV)
 call msg('r', '[RT] IO cache steps                 :',RT_IO_steps)
 call msg('r', '[RT] IO cache time              [fs]:',RT_IO_time/FS2AUT)
 !
 do i1=1,n_ext_fields
   !
   if (i_Pump==i_Probe.and.i1/=i_Probe.and..not.any(trim(Efield(i1)%ef_name)==(/'SIN    ','ANTIRES','RES    '/)) ) cycle
   !
   MSG_ch="Pump"
   if (i1==i_Probe) MSG_ch="Probe"
   call msg('r', '[RT] '//MSG_ch//' energy window/period [eV/fs]:' &
&     ,(/Efield(i1)%frequency*HA2EV,2._SP*pi*HBAR_evfs/(Efield(i1)%frequency*HA2EV)/))
   call msg('r', '[RT] '//MSG_ch//' elemental oscillation[fs]:',EtoT(E=Efield(i1)%frequency(2)-Efield(i1)%frequency(2))/FS2AUT)
   call msg('r', '[RT] '//MSG_ch//' energy steps             :',Efield(i1)%n_frequencies)
   call msg('r', '[RT] '//MSG_ch//' electric field      [V/m]:',Efield(i1)%amplitude*AU2VMm1)
 enddo
 !
 ! Fluence
 !=========
 if (l_NE_with_fields) then
   allocate(Fluence(NE_steps))
   Fluence=0._SP
   if (l_compute_fluence) call RT_Fluence()
 endif
 !
 ! Generalized Rabi Function
 !============================
 if (l_NE_with_fields) call RT_Rabi_Function(E,Efield(i_Pump))
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
 call COLLISIONS_load(.TRUE.)
 !
 call UPDATE_flags(.TRUE.)
 !
 if(NE_steps>0) then
   call msg('nr','[RT] Simulate IP                :',l_NE_simulate_IP)
   call msg( 'r','[RT] Simulate RPA               :',l_NE_simulate_RPA)
   call msg( 'r','[RT] Simulate BSE               :',l_NE_simulate_BSE)
   call msg( 'r','[RT] Simulate TD-DFT            :',l_NE_simulate_TDDFT)
   call msg( 'r','[RT] Carriers El-ph scattering  :',l_ph_corr)
   call msg( 'r','[RT] Carriers El-El scattering  :',l_carrier_dynamics)
   if (l_G_retarded_DB) call msg( 'r','[RT] Retareded Green`s function :',Gr_kind)
   call msg( 'r','[RT] Extended collisions        :',l_eval_collisions)
   if (l_eval_collisions) call msg( 'r','[RT]                       [I/O]:',l_STORE_COLLISIONS)
 endif
 !
 ! RIM support
 !=============
 l_RT_uses_E_RIM=BZ_RIM_tot_nkpts>0.and.BZ_RIM_nbands>=SC_bands(2)
 call msg( 'r','[RT] Energy RIM support         :',l_RT_uses_E_RIM)
 !
 ! Number of K-points
 !====================
 RT_nk=k%nibz
 if (l_RT_uses_E_RIM) RT_nk=BZ_RIM_tot_nkpts
 !
 contains
   !
   subroutine UPDATE_flags(COLLISIONS_checked)
     !
     logical :: COLLISIONS_checked
     !
     if (COLLISIONS_checked) then
       if (l_eval_collisions.and..not.l_carrier_dynamics) then
         XCfrozen=.FALSE.
         if (Hfrozen) call warning('COLLISIONS used but Hartree frozen')
       endif
     endif
     !
     if (l_carrier_dynamics) XCfrozen=.TRUE.
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

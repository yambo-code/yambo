!
!         Copyright (C) 2000-2010 the YAMBO team 
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
subroutine RT_initialize(Xen)
 ! 
 use drivers,        ONLY:l_collisions_IO,l_sc_contains_lda
 use SC,             ONLY:local_V
 use pars,           ONLY:SP,pi
 use units,          ONLY:SPEED_OF_LIGHT,FS2AUT,HARTREE,HBAR_eVfs
 use com,            ONLY:msg,warning,error
 use vec_operate,    ONLY:normalize_v
 use electrons,      ONLY:nel,levels
 use real_time,      ONLY:l_NE_simulate_RPA,Thermal_steps,RT_step,l_NE_with_fields, &
&                         l_NE_simulate_IP,l_RT_diagonalize,NE_steps,NE_tot_time,   &
&                         l_RT_diamagnetic,max_rt_steps_disk,l_NE_simulate_BSE,     &
&                         l_NE_simulate_Irredux,l_RT_needs_XC0,Hfrozen,XCfrozen,    &
&                         l_RT_induced_field,NE_Hxc_jumps,l_NE_simulate_TDDFT,      &
&                         RT_diagonalization_steps,RT_is_dumped,QP_LifeTime,Phase_LifeTime, &
&                         RT_TwoL_Polarization
#if defined _TWO_LEVELS
 use real_time,      ONLY:tau_offdiag,tau_diag,l_3levels
#endif
 use D_lattice,      ONLY:DL_vol
 use fields,         ONLY:global_gauge,Efield,n_ext_fields,field_is_ok,ef_i_time,&
&                         theta_function_iTo_ref,i_Pump,i_Probe,EtoT
 use parser_m,       ONLY:parser
 implicit none
 !
 type(levels), intent(in) :: Xen
 !
 ! Work space
 !
 integer                  :: i1,rest
 character(5)             :: MSG_ch
 !
 ! Logicals
 !==========
 !
 call parser('Diamagnetic',  l_RT_diamagnetic)
 call parser('InducedField' ,l_RT_induced_field)
 call parser('RTBSEOnly',    l_NE_simulate_BSE)
 call parser('RTfreezeH',    Hfrozen)
 call parser('RTfreezeXC',   XCfrozen)
#if defined _TWO_LEVELS
 call parser('RT3Levels',    l_3levels)
#endif
 !
 l_RT_diagonalize      =RT_diagonalization_steps>1
 l_NE_simulate_RPA     =.not.Hfrozen.and.XCfrozen
 l_NE_simulate_IP      =Hfrozen.and.XCfrozen
 l_NE_simulate_Irredux =Hfrozen.and..not.XCfrozen
 if(l_NE_simulate_BSE.and.XCfrozen) call warning(" BSE turned off because XC is frozen! ")
 l_NE_simulate_BSE     =.not.XCfrozen.and.l_NE_simulate_BSE
 l_NE_simulate_TDDFT   =.not.XCfrozen.and..not.Hfrozen.and.l_sc_contains_lda
 !
 !
 ! In some cases I need to freeze XC or Hartree. In these cases I initialize here
 ! the scattering term. No updates will follow in RT_Collision
 !
 l_RT_needs_XC0=any((/l_NE_simulate_IP,l_NE_simulate_RPA,l_NE_simulate_BSE,&
&                     l_NE_simulate_Irredux,l_NE_simulate_TDDFT,Thermal_steps>0/))
 !
 ! Check if Collisions are required
 !==================================
 !
 if(.not.local_V.and..not.l_NE_simulate_IP.and..not.l_collisions_IO) &
&  call error(" Real-time works only with collisions, turn them on 'yambo -e' !")
 !
 ! Use NE_tot_time if provided
 !
 if (NE_tot_time>0.) NE_steps=max(NE_steps,int(NE_tot_time/RT_step))
 !
 ! Set a number of steps compatible with the cache 
 !=================================================
 !
 if(mod(Thermal_steps+NE_steps,max_rt_steps_disk)/=0) then
   rest=mod(Thermal_steps+NE_steps,max_rt_steps_disk)
   if(NE_steps/=0) then
     NE_steps = NE_steps+max_rt_steps_disk-rest
   elseif (NE_steps==0.and.Thermal_steps/=0) then
     Thermal_steps = Thermal_steps+max_rt_steps_disk-rest
   endif
 endif
 !
 NE_tot_time=RT_step*NE_steps
 !
 if (NE_Hxc_jumps<=0) NE_Hxc_jumps=1
 !
 ! QP and Phase Lifetimes
 !========================
 !
 RT_is_dumped=any((/QP_LifeTime,Phase_LifeTime/)>1.E-8_SP)
#if defined _TWO_LEVELS
 if((any(tau_offdiag>1.E-8_SP).or.any(tau_diag>1.E-8_SP)).and.l_3levels) &
&  RT_is_dumped=.true.
#endif
 !
 ! External fields checks
 !========================
 !
 l_NE_with_fields=.false.
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
     Efield(i1)%versor         =normalize_v(Efield(i1)%versor)
     !
     Efield(i1)%int_t_initial  =int(Efield(i1)%t_initial/RT_step)+1
     !
     Efield(i1)%amplitude=sqrt(Efield(i1)%intensity*(8._SP*pi)/SPEED_OF_LIGHT) 
     !
     call SC_check_symmetries(Efield(i1)%versor)
     !
     if(Efield(i1)%ef_name/='DELTA'.and.(Efield(i1)%frequency(1)==0._SP.and.Efield(i1)%damping==0)) &
&      call warning(" External "//trim(Efield(i1)%ef_name)//" field has zero frequency and damping.")
     ! 
     if(Efield(i1)%intensity==0._SP) &
&      call warning(" External "//trim(Efield(i1)%ef_name)//" field has zero intensity.")
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
 !call RT_step_test()
 !
 ! LOG
 !=====
 !
 call msg( 'r', '[RT] NE Steps                   :',NE_steps)
 call msg( 'r', '[RT] NE time                [fs]:',NE_steps*RT_step/FS2AUT)
 call msg( 'r', '[RT] Thermalization steps       :',Thermal_steps)
 call msg( 'r', '[RT] Step length            [fs]:',RT_step/FS2AUT)
 call msg( 'r', '[RT] Damping (Phase)    [fs/meV]:',(/Phase_LifeTime/FS2AUT,HBAR_eVfs/Phase_LifeTime*FS2AUT*1000./))
 call msg( 'r', '[RT] Damping (QP)       [fs/meV]:',(/QP_LifeTime/FS2AUT,   HBAR_eVfs/QP_LifeTime*FS2AUT*1000./))
 if(NE_steps>0) then
   call msg('rn','[RT] Simulation time        [fs]:',(/-real(Thermal_steps)*RT_step,RT_step*real(NE_steps)/)/FS2AUT)
   call msg('rn','[RT] Electron-gas plasma frequency [eV]:',sqrt(4._SP*pi*nel/DL_vol)*HARTREE)
   !
   call msg( 'r','[RT] Simulate IP                :',l_NE_simulate_IP)
   call msg( 'r','[RT] Simulate RPA               :',l_NE_simulate_RPA)
   call msg( 'r','[RT] Simulate BSE               :',l_NE_simulate_BSE)
   call msg( 'r','[RT] Simulate TD-DFT            :',l_NE_simulate_TDDFT)
   !
 endif
 call msg('sr', '[RT] Pump/Probe periodicity     [fs]:',EtoT(E=maxval(Efield(:)%W_step))/FS2AUT)
 call msg('sr', '[RT]            energy step     [eV]:',maxval(Efield(:)%W_step)*HARTREE)
 !
 do i1=1,n_ext_fields
   !
   MSG_ch="Pump"
   if (i1==i_Probe) MSG_ch="Probe"
   call msg('sr', '[RT] '//MSG_ch//' energy window        [eV]:',Efield(i1)%frequency*HARTREE)
   call msg('sr', '[RT] '//MSG_ch//' energy steps             :',Efield(i1)%n_frequencies)
 enddo
 !
 if(l_NE_with_fields) call msg('rn','[RT] Gauge                      :',trim(global_gauge))
 !
 ! Generalized Rabi Frequency
 !============================
 if(l_NE_with_fields) call RT_TwoL_Polarization(Xen,.FALSE.)
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
end subroutine RT_initialize

!
!        Copyright (C) 2000-2016 the YAMBO team
!            http://www.yambo-code.org
!
! Authors (see AUTHORS file for details):
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
 use drivers,        ONLY:l_use_collisions,l_elph_scatt,l_elel_scatt,l_sc_ip,l_sc_hartree,&
&                         l_sc_is_libDFT,l_sc_cohsex,l_sc_hf,l_elphoton_scatt
 use pars,           ONLY:SP,pi
 use units,          ONLY:FS2AUT,HA2EV,HBAR_eVfs,AU2VMm1,AU2KWCMm2,SPEED_OF_LIGHT
 use stderr,         ONLY:intc
 use com,            ONLY:msg,warning,error
 use vec_operate,    ONLY:normalize_v
 use plasma,         ONLY:EH_gas
 use X_m,            ONLY:global_gauge
 use electrons,      ONLY:nel,levels,BZ_DbGd_nbands,BZ_DbGd_tot_nkpts,DbGd_is_symmetry_expanded
 use rt_ctl,         ONLY:RT_IO_JPO_steps,RT_IO_time,RT_IO_steps_DB, &
&                         RT_IO_G_steps,RT_IO_output_steps,RT_carriers_object
 use real_time,      ONLY:RT_step,l_NE_with_fields,RT_bands, &
&                         NE_steps,NE_tot_time,RT_carriers,   &
&                         l_NE_simulate_Irredux,l_RT_induced_field,                   &
&                         RT_is_dephased,RAD_LifeTime,       &
&                         Phase_LifeTime,NE_MEM_treshold,G_MEM_steps,Gr_kind,DbGd_ibz_coo, &
&                         l_RT_CCA_Kernel,Life_MEM_steps,    &
&                         l_RT_uses_E_DbGd,DbGd_bz_coo,RT_life_interp_steps,&
&                         RT_life_interp_KIND,l_NE_dynamics,l_RT_impose_N_dN,I_MEM_steps,Integrator_name,&
&                         l_RT_EULER,l_RT_RK2,l_RT_RWA,l_RT_INTERPOLATE,l_RT_EXP,DbGd_EE_scatt_table,&
&                         DbGd_EE_Nk_table,l_RT_impose_E,DbGd_EE_percent,DbGd_EE_Nk_table,&
&                         l_BGR,l_RT_HEUN,eval_DENSITY,eval_HARTREE,eval_DFT,&
&                         RT_nk,RT_ibz_coo,RT_all2ibz,RT_Nk_ibz
 use IO_m,           ONLY:io_COLLs
 use R_lattice,      ONLY:bz_samp
 use fields,         ONLY:Efield,n_ext_fields,field_is_ok,&
&                         l_manual_excitation,EtoT,Efield_strength
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(levels), intent(in) :: E
 type(bz_samp),intent(in) :: q,k
 !
 ! Work space
 !
 integer                  :: i1,rest,ik,IO_steps(3),N_T_samplings
 character(6)             :: MSG_ch
 real(SP)                 :: T_memory
 logical                  :: l_Full_RT_T_evolution,l_skip_it
 integer,         external:: RT_k_grid,RT_Lifetimes_evaluation_plan
 !
 ! Logicals
 !==========
 !
 call parser('BGR',                  l_BGR)
 call parser('InducedField' ,        l_RT_induced_field)
 call parser('RT_T_evol',            l_Full_RT_T_evolution)
 call parser('RTskipImposeN',        l_skip_it)
 if (l_skip_it) l_RT_impose_N_dN=.FALSE.
 !
 call parser('RTskipImposeE',        l_skip_it)
 if (l_skip_it) l_RT_impose_E=.FALSE.
 !
 call parser('RTpumpEhEn',           l_manual_excitation)
 !
 l_RT_CCA_Kernel       =.not.l_Full_RT_T_evolution.and.l_elph_scatt
 !
 eval_DFT     =  l_sc_is_libDFT            .and..not.l_sc_ip
 eval_HARTREE = (.not.l_NE_simulate_Irredux.and..not.l_sc_ip).or.l_sc_hartree
 eval_DENSITY =  eval_HARTREE .or. eval_DFT
 !
 ! Integrator FLAGS
 !==================
 !
 l_RT_EULER=index(Integrator_name,'EULER')/=0.or.index(Integrator_name,'euler')/=0
 !
 l_RT_RK2  =index(Integrator_name,'RK2'  )/=0.or.index(Integrator_name,'rk2'  )/=0
 !
 l_RT_HEUN =index(Integrator_name,'HEUN' )/=0.or.index(Integrator_name,'heun' )/=0
 !
 l_RT_RWA  =index(Integrator_name,'RWA'  )/=0.or.index(Integrator_name,'rwa'  )/=0
 !
 l_RT_EXP  =index(Integrator_name,'EXP'  )/=0.or.index(Integrator_name,'exp'  )/=0
 !
 l_RT_INTERPOLATE=index(Integrator_name,'INTERPOLATE')/=0.or.index(Integrator_name,'interpolate')/=0
 !
 if (l_RT_EXP.and.l_RT_INTERPOLATE) l_RT_INTERPOLATE=.false.
 !
 if (l_RT_INTERPOLATE) call error(' INTERPOLATION of coherent part: status unknonw')
 !
 Integrator_name=""
 if ((     l_RT_EULER).and.(.not.l_RT_RK2).and.(.not.l_RT_HEUN)) Integrator_name = "EULER"
 if ((.not.l_RT_EULER).and.(     l_RT_RK2).and.(.not.l_RT_HEUN)) Integrator_name = "RK2"
 if ((.not.l_RT_EULER).and.(.not.l_RT_RK2).and.(     l_RT_HEUN)) Integrator_name = "HEUN"
 if(trim(Integrator_name)=="") then
   call warning("Bad integrator. Using default RK2")
   l_RT_RK2=.true.
   Integrator_name = "RK2"
 endif
 if (l_RT_RWA)         Integrator_name = trim(Integrator_name)//" + RWA"
 if (l_RT_EXP)         Integrator_name = trim(Integrator_name)//" + EXP"
 if (l_RT_INTERPOLATE) Integrator_name = trim(Integrator_name)//" + INTERPOLATE"
 !
 if(.not.any((/trim(global_gauge)=="velocity".or.trim(global_gauge)=="length"/)) ) then
   call warning(" Not a valid gauge in input. Switching to length gauge")
   global_gauge="length"
 endif
 !
 ! Use NE_tot_time if provided
 !
 if (NE_tot_time>0._SP) NE_steps=max(NE_steps,nint(NE_tot_time/RT_step)+1)
 !
 ! Set a number of steps. i1=1(J,P,CARRIERs)  i1=2(GF) i1=3(output).
 ! Negative value forces the step to be set automatically
 !==================================================================
 !
 do i1=1,3
   !
   if (RT_IO_time(i1)>0) IO_steps(i1) =nint(RT_IO_time(i1)/RT_step)
   if (IO_steps(i1)== 0) IO_steps(i1) =1
   !
   if(IO_steps(i1)>0.and.mod(NE_steps-1,IO_steps(i1))/=0) then
     !
     if (i1>1) cycle
     !
     rest=mod(NE_steps-1,IO_steps(i1))
     NE_steps = NE_steps+IO_steps(i1)-rest
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
     rest=mod(NE_steps-1,IO_steps(i1))
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
 T_memory=0._SP
 if (l_elph_scatt.and.NE_MEM_treshold>0.) then
   T_memory=-1./minval( abs(E%W(RT_bands(1):RT_bands(2),:,:)))*log(NE_MEM_treshold)
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
     if (.not.field_is_ok( Efield(i1)) ) &
&      call error('Field '//trim(Efield(i1)%ef_name)//' not supported or polarization is wrong')
     !      
     l_NE_with_fields=.true.
     !
     if(Efield(i1)%ef_name/='DELTA'.and.Efield(i1)%frequency(1)==0._SP) &
&      call error(" External "//trim(Efield(i1)%ef_name)//" field has zero frequency.")
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
     Efield(i1)%t_initial_indx=nint(Efield(i1)%t_initial/RT_step)+1
     if ( mod(Efield(i1)%t_initial_indx-1,RT_IO_JPO_steps)/=0 ) then
       call warning(' Field must start at a T-step where the current is written. Correcting T0')
       Efield(i1)%t_initial_indx=Efield(i1)%t_initial_indx-mod(Efield(i1)%t_initial_indx-1,RT_IO_JPO_steps)
     endif
     if (Efield(i1)%t_initial_indx< RT_IO_JPO_steps+1 ) then
       call warning(' Field cannot start before dT(J) . Setting T0=dT(J)')
       Efield(i1)%t_initial_indx=RT_IO_JPO_steps+1
     endif
     Efield(i1)%t_initial=real(Efield(i1)%t_initial_indx-1)*RT_step
     !
     call SC_check_symmetries(Efield(i1)%versor)
     !
     if ( Efield(i1)%ef_name=='DELTA' ) then
       Efield(i1)%t_final_indx=Efield(i1)%t_initial_indx+2
       Efield(i1)%t_final=RT_step*(Efield(i1)%t_final_indx-1)       
     else if ( (Efield(i1)%ef_name=='QSSIN') .or. (Efield(i1)%ef_name=='GAUSS') ) then
       call RT_Fluence(i1)
     else
       Efield(i1)%t_final_indx=    NE_steps
       Efield(i1)%t_final=RT_step*(NE_steps-1)
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
 if ( .not. l_NE_dynamics ) call error('No external fields nor manual pumping provided')
 !
 ! RIM support
 !=============
 l_RT_uses_E_DbGd=BZ_DbGd_tot_nkpts>0.and.BZ_DbGd_nbands>=RT_bands(2).and..not.DbGd_is_symmetry_expanded
 !
 ! Number of K-points
 !====================
 allocate(DbGd_ibz_coo(k%nibz,2))
 allocate(DbGd_bz_coo(k%nbz,2))
 allocate(RT_ibz_coo(k%nibz,2))
 allocate(RT_Nk_ibz(k%nibz))
 if (l_RT_uses_E_DbGd) then
   RT_nk=RT_k_grid(E,k,q)
 else
   RT_nk=k%nibz
   RT_Nk_ibz=1
   DbGd_EE_percent=0.
   allocate(RT_all2ibz(k%nibz))
   if(l_elel_scatt) then
     allocate(DbGd_EE_scatt_table(k%nibz,q%nbz))
     allocate(DbGd_EE_Nk_table(k%nibz,k%nibz))
     DbGd_EE_scatt_table=.true.
     DbGd_EE_Nk_table=1
   endif
   do ik=1,k%nibz
     DbGd_ibz_coo(ik,:)=ik
     RT_ibz_coo(ik,:)=ik
     RT_all2ibz(ik)=ik
   enddo
 endif
 !
 ! Electron-phonon
 ! ===============
 !
 if (l_elph_scatt) then
   !
   call section('=','El-Ph matrix elements loading and setup')
   !                 =======================================
   call RT_ELPH_initialize(E,k,q)
   !
 endif
 !
 ! Memory Depth & Simulation points (Lifetimes)
 !=============================================
 if (l_elph_scatt.or.l_elel_scatt) then
   if (.not.associated(E%W)) then
     call warning("NO QP damping provided. E-E and/or E-P switched off")
     l_elph_scatt=.FALSE.
     l_elel_scatt=.FALSE.
   endif
   N_T_samplings =RT_Lifetimes_evaluation_plan( )
 endif
 !
 ! Checking extended COLLISIONS
 !==============================
 !
 if (l_elel_scatt.or.l_sc_cohsex.or.l_sc_hf) then
   !
   call section('=','Electronic Collisions loading and setup')
   !
   if (l_elel_scatt) then
     call PLASMA_build_up(E,k,q)
     call PLASMA_parallel_setup(EH_gas)
   endif
   !
   call COLLISIONS_load(.TRUE.)
   !
 endif
 !
 !
 ! Initial allocation and basic definitions of the RT_carriers object
 !====================================================================
 !
 call RT_carriers_object(RT_carriers,E,k,WHAT='allocate')
 !
 ! LOG
 ! ===
 !
 call section('=','Messaging')
 !
 call msg( 'r','---General---')
 call msg( 'r', 'Integrator                 :',trim(Integrator_name))
 call msg( 'r', 'Simulation time        [fs]:',real(NE_steps-1)*RT_step/FS2AUT)
 call msg( 'r', 'Step length            [fs]:',RT_step/FS2AUT)
 call msg( 'r', 'NE Steps                   :',NE_steps)
 call msg( 'r', 'NE time                [fs]:',(NE_steps-1)*RT_step/FS2AUT)
 call msg( 'r', 'Memory depth(G)        [fs]:',T_memory/FS2AUT)
 call msg( 'r', 'Memory steps(G)            :',G_MEM_steps)
 if (l_RT_INTERPOLATE) call msg( 'r', 'Memory steps(COHERENT part):',I_MEM_steps)
 !
 if (.not.trim(RT_life_interp_KIND)=="NONE") then
   call msg( 'nr','---Dynamics---')
   call msg( 'r', 'Sampling                       :',RT_life_interp_KIND)
   call msg( 'r', 'Memory steps (Lifetimes)       :',Life_MEM_steps)
   call msg( 'r', 'Steps length (inter/intra) [fs]:',RT_life_interp_steps/FS2AUT)
   call msg( 'r', 'Time points                    :',N_T_samplings*Life_MEM_steps)
 endif
 !
 call msg( 'nr','---Flags (if any)---')
 if (l_sc_ip)             call msg( 'r','Simulate IP                :',l_sc_ip)
 if (l_sc_hartree)        call msg( 'r','Simulate RPA               :',l_sc_hartree)
 if (l_sc_is_libDFT)      call msg( 'r','Simulate TD-DFT            :',l_sc_is_libDFT)
 if (l_sc_cohsex)         call msg( 'r','Simulate BSE               :',l_sc_cohsex)
 !
 if (l_NE_simulate_Irredux) call msg( 'r','Simulate Irredux           :',l_NE_simulate_Irredux)
 !
 if (l_elph_scatt)        call msg( 'r','El-Ph         scattering   :',l_elph_scatt)
 if (l_elel_scatt)        call msg( 'r','El-El         scattering   :',l_elel_scatt)
 if (l_elphoton_scatt)    call msg( 'r','El-Photon     scattering   :',l_elphoton_scatt)
 if (l_BGR)               call msg( 'r','On-the-fly BGR added       :',l_BGR)
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
 if ((l_elel_scatt.or.l_elph_scatt).and.(.not.l_RT_impose_N_dN.or.l_RT_impose_E)) then
   call msg( 'nr','---Conservation Rules---')
   call msg( 'r',                  'dN (e-p) and N (e-p + e-e)  :',l_RT_impose_N_dN)
   if (l_elel_scatt) call msg( 'r','Energy (e-e)                :',l_RT_impose_E)
 endif
 !
 call msg( 'nr','---RIM---')
 call msg( 'r', 'Energy RIM support           :',l_RT_uses_E_DbGd)
 if (l_RT_uses_E_DbGd) then
   call msg( 'r', 'K-points            (TOTAL):',BZ_DbGd_tot_nkpts)
   call msg( 'r', '                     (USED):',RT_nk)
   if (l_elel_scatt) then
     call msg( 'r', 'Percentual used in e-e[o/o]:',DbGd_EE_percent)
     call msg( 'r', 'Max e-e scattering states  :',maxval(DbGd_EE_Nk_table))
   endif
 endif
 !
 if (l_NE_with_fields) then
   call msg( 'nr','---Fields---')
   call msg('r','Gauge                       :',trim(global_gauge))
   !
   !call msg('r','Pump/Probe periodicity  [fs]:',EtoT(E=maxval(Efield(:)%W_step))/FS2AUT)
   !call msg('r','Pump/Probe energy step  [eV]:',maxval(Efield(:)%W_step)*HA2EV)
   !
   do i1=1,n_ext_fields
     MSG_ch="Field"//trim(intc(i1))
     call msg('r', MSG_ch//' field                       :'//trim(Efield(i1)%ef_name))
     call msg('r', MSG_ch//' polarization                :'//trim(Efield(i1)%ef_pol))
     call msg('r', MSG_ch//' energy window/period [eV/fs]:',(/Efield(i1)%frequency*HA2EV,&
&                  2._SP*pi*HBAR_evfs/(Efield(i1)%frequency*HA2EV)/))
     call msg('r', MSG_ch//' elemental oscillation   [fs]:',EtoT(E=Efield(i1)%frequency(2)-Efield(i1)%frequency(2))/FS2AUT)
     call msg('r', MSG_ch//' damping                 [fs]:',Efield(i1)%damping/FS2AUT)
     call msg('r', MSG_ch//' energy steps                :',Efield(i1)%n_frequencies)
     call msg('r', MSG_ch//' electric field         [V/m]:',Efield(i1)%amplitude*AU2VMm1)
     call msg('r', MSG_ch//' max intensity      [kW/cm^2]:',Efield(i1)%intensity*AU2KWCMm2)
     !
     if( (Efield(i1)%ef_name=='QSSIN') .or. (Efield(i1)%ef_name=='GAUSS') .or. (Efield(i1)%ef_name=='DELTA') ) then
       call msg('r', MSG_ch//' max fluence        [nJ/cm^2]:',Efield(i1)%fluence )
       call msg('r', MSG_ch//' final time              [fs]:',Efield(i1)%t_final/FS2AUT)
     endif
     !
   enddo
   !
 endif
 !
end subroutine RT_initialize

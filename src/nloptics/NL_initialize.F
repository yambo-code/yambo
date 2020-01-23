!
!        Copyright (C) 2000-2019 the YAMBO team
!            http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA
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
subroutine NL_initialize(E,k,q)
 ! 
 use pars,           ONLY:SP,DP,pi
 use drivers,        ONLY:l_sc_fock,l_sc_coh,l_sc_sex,l_sc_hartree
 use vec_operate,    ONLY:normalize_v
 use hamiltonian,    ONLY:H_potential
 use units,          ONLY:AU2VMm1,AUT2FS,HA2EV
 use electrons,      ONLY:levels,n_sp_pol
 use com,            ONLY:msg
 use zeros,          ONLY:zero_dfl
 use R_lattice,      ONLY:bz_samp,k_map
 use fields,         ONLY:Efield,Efield_strength,n_ext_fields,EtoT
 use real_time,      ONLY:RT_step,NE_steps,NE_tot_time,eval_HARTREE,eval_DFT,l_RT_induced_field, &
&                         Phase_LifeTime,Integrator_name,RT_dyn_step,NE_i_time,RT_potential
 use RT_control,     ONLY:SETUP_RT_IO_type,OBS_RT_IO_t
 use nl_optics,      ONLY:l_use_DIPOLES,EULER,EULEREXP,RK2,RK4,RK2EXP,RK4EXP,HEUN,INVINT,Integrator,CRANK_NICOLSON, &
&                         NL_damping,NL_correlation,Correlation,NL_er,NL_estep,n_frequencies,IPA,TDDFT,HARTREE,LRC,JGM, &
&                         LRCW,SEX,HF,eval_dG,eval_COLLISIONS,NL_bands,NL_verb_name,VERB_HIGH,VERB_LOW,NL_verbosity, &
&                         n_order,WF_sample,NL_set_WF_sample,NL_INIT_WF_sample,l_eval_CURRENT
 use electric,       ONLY:l_force_SndOrd
 use parser_m,       ONLY:parser
 !
#include<memory.h>
 !
 type(levels), intent(in) :: E
 type(bz_samp),intent(in) :: q,k
 !
 ! Working space
 !
 real(SP) :: Time, WF_sample_range(2),W_0,T_0,sigma 
 integer  :: i1
 !
 ! Parser
 ! ============
 !
 call parser('FrSndOrd',             l_force_SndOrd)
 call parser('InducedField' ,        l_RT_induced_field)
 call parser('EvalCurrent' ,         l_eval_CURRENT)
 !
 if(l_use_DIPOLES) call warning(' Fixed dipoles: only linear response properties are correct!')
 !
 ! Check double-precision and other possible errors
 ! ================================================
 !
 if(DP/=SP)      call error(" Non-linear optics works only in double precision, configure with --enable-dp flag")
 if(n_sp_pol/=1) call error(" Spin-polarized systems not yet implemented")
 if(any(NL_bands(:)==0)) call error("Any of the NLBands limits is 0, change to nv|nc with 0<nv<fermi<nc")
 !
 ! K-sampling
 ! ===========
 !
 if(.not.allocated(k_map%k_map_dir)) call k_map_nearest_by_dir(k,k_map)
 if(.not.allocated(k%k_table))       call k_build_up_BZ_tables(k)
 !
 ! Integrator ! ============
 !
 select case (trim(Integrator_name))
    case('EULER')
        Integrator=EULER
    case('EULEREXP')
        Integrator=EULEREXP
    case('RK2')  
        Integrator=RK2
    case('RK4')
        Integrator=RK4
    case('RK2EXP')
        Integrator=RK2EXP
    case('RK4EXP')
        Integrator=RK4EXP
    case('HEUN')
        Integrator=HEUN
        call error("HEUN integrator not yet available")
    case('INVINT')
        Integrator=INVINT
    case('CRANKNIC')
        Integrator=CRANK_NICOLSON
    case default
        call error("Unknow integrator")
 end select
 !
 ! Verbosity Level
 ! 
 select case (trim(NL_verb_name))
   case('low')
      NL_verbosity=VERB_LOW
   case('high')
      NL_verbosity=VERB_HIGH
   case default
      call error("Unknow verbosity level")
 end select
 !
 ! Check Field
 ! =========================
 !
 ! This part of the code works with only one field
 !
 n_ext_fields=1
 !
 do i1=1,n_ext_fields
   !
   Efield(i1)%ef_name=trim(Efield(i1)%ef_name)      
   !
   if((Efield(i1)%ef_name=='none')) cycle
   !
   if(Efield(i1)%intensity==0._SP) &
&    call error(" External "//trim(Efield(i1)%ef_name)//" field has zero intensity.")
     !
   if(all(abs(Efield(i1)%versor(:))<epsilon(1.))) &
&    call error(" External "//trim(Efield(i1)%ef_name)//" field has undefined direction.")
     !
     Efield(i1)%versor        =normalize_v(Efield(i1)%versor)
     Efield(i1)%amplitude     =Efield_strength(Efield(i1)%intensity,'AU')
     Efield(i1)%t_initial_indx=nint(Efield(i1)%t_initial/RT_step)+1
     Efield(i1)%t_initial     =real(Efield(i1)%t_initial_indx-1)*RT_step
     Efield(i1)%ef_pol        ="linear"
     !
     call Check_symmetries(Efield(i1)%versor)
     !
 enddo
 !
 if(Efield(1)%ef_name/='SOFTSIN'.and.Efield(1)%ef_name/='SIN') &
    call warning(' Non-linear optics properties can be obtained with SOFTSIN/SIN fields only!')
 !
 ! For short pulses field (PULSE, QSSIN, QSFIELD, SPULSE)
 ! calcualte the T_0
 !
 W_0=Efield(1)%frequency(1)
 sigma=Efield(1)%width
 T_0=pi/W_0*(real(nint(W_0/pi*3._SP*sigma),SP))
 if(Efield(1)%ef_name=='SPULSE') T_0=sigma
 !
 ! Important in the Time-dependent Schrodinger Eq.
 ! the Phase_LifeTime is 1/2 of the one used in the 
 ! Non-Equilibrium Green's Function
 !
 Phase_LifeTime=2._SP/NL_damping
 !
 ! Determine the total time from the damping or from T_0
 !
 if(NE_tot_time<=0._SP) then
   !
   if(Efield(1)%ef_name=='SOFTSIN'.or.Efield(1)%ef_name=='SIN') then
     !  
     ! Run lenght is 6*Phase_lifeTime + 4 fs for response analysis
     !
     if(NL_damping<zero_dfl) call error(" Error damping=0 and NLtime<0!! ") 
     !
     ! Dephasing Time
     !
     NE_tot_time=Phase_LifeTime*5._SP
     !
     ! Add time to sample the response
     !
     NE_tot_time=NE_tot_time+EtoT(E=NL_er(1))+5._SP*RT_step
     !
     call msg('sr','Dephasing Time          [fs]:',5._SP*Phase_LifeTime*AUT2FS)
     call msg('sr','Sampling  Time          [fs]:',(EtoT(E=NL_er(1))+5._SP*RT_step)*AUT2FS)
     !
   elseif(Efield(1)%ef_name=='PULSE'.or.Efield(1)%ef_name=='QSSIN' &
&     .or.Efield(1)%ef_name=='QSFIELD'.or.Efield(1)%ef_name=='SPULSE') then
     !
     NE_tot_time=3*T_0
     call msg('sr','Time T_0                [fs]:',T_0*AUT2FS)
     call msg('sr','Dephasing               [fs]:',Phase_LifeTime*AUT2FS)
     !
   else
     call error("Negative total simulation time! ")
   endif
   ! 
 else
   call msg('s',' Total simulation time read from input') 
 endif
 !
 call msg('sr','Total simulation time   [fs]:',NE_tot_time*AUT2FS)
 !
 NE_steps=int(NE_tot_time/RT_step)+1
 !
 ! Correlation
 ! ========================
 !
 eval_HARTREE   =.false.
 eval_DFT       =.false.
 eval_dG        =.false.
 eval_COLLISIONS=.false.
 !
 ! NB.: This part duplicates what is in
 !      INIT.F 
 ! 
 select case (trim(NL_Correlation))
    case('IPA')
        Correlation=IPA
        RT_potential='IP'
    case('RPA')
        Correlation=HARTREE
        RT_potential='HARTREE'
        eval_HARTREE =.true.
    case('HARTREE')
        Correlation=HARTREE
        RT_potential='HARTREE'
        eval_HARTREE =.true.
    case('TDDFT')  
        Correlation=TDDFT
        RT_potential='default'
        eval_HARTREE =.true.
        eval_DFT     =.true.
    case('LRC')
        Correlation=LRC
        RT_potential='HARTREE'
        eval_HARTREE =.true.
    case('LRW')
        Correlation=LRCW
        RT_potential='HARTREE'
        eval_HARTREE =.true.
    case('JGM')
        Correlation=JGM
        RT_potential='HARTREE'
        eval_HARTREE =.true.
    case('SEX')
        Correlation    =SEX
        RT_potential   ='HARTREE+SEX'
        eval_HARTREE   =.true.
        eval_dG        =.true.
        eval_COLLISIONS=.true.
        l_sc_coh       =.true.
        l_sc_sex       =.true.
    case('HF')
        Correlation=HF
        RT_potential   ='HARTREE+FOCK'
        eval_HARTREE   =.true.
        eval_dG        =.true.
        eval_COLLISIONS=.true.
        l_sc_hartree   =.true.
        l_sc_fock      =.true.
    case default
        call error("Unknow correlation in the response function")
 end select
 !
 H_potential = RT_potential
 !
 ! Deallocate bare_qpg if not required
 !=============================
 ! DS 2019/06/18:
 ! bare_qpg is used by DIPOLE_dimensions
 ! The dependency maybe likey removed in the future
 ! For now I'm commenting the following lines
 !  
 !if(.not.eval_HARTREE.and.CORRELATION/=JGM) then
 !  YAMBOX_FREE(bare_qpg)
 !endif
 !
 ! Frequencies range
 ! =========================
 NL_estep=(NL_er(2)-NL_er(1))/real(n_frequencies,SP)
 !
 ! Initialize Time variables
 ! =========================
 ! At present only a constant time-step for all non-linear
 ! frequencies is supported
 !
 RT_dyn_step=RT_step
 OBS_RT_IO_t%INTERVAL_time     =RT_step
 OBS_RT_IO_t%INTERVAL_time_SAVE=RT_step
 call SETUP_RT_IO_type(OBS_RT_IO_t,.TRUE.)
 YAMBO_ALLOC(OBS_RT_IO_t%Time,(NE_steps))
 do NE_i_time=1,NE_steps
   Time=real(NE_i_time-1,SP)*RT_step
   OBS_RT_IO_t%Time(NE_i_time)=Time
 enddo
 !
 ! WFs sampling
 ! =========================
 !
 call NL_INIT_WF_sample(WF_sample)
 WF_sample_range  = (/OBS_RT_IO_t%Time(NE_steps) - EtoT(E=NL_er(1))+5._SP*RT_step,OBS_RT_IO_t%Time(NE_steps)/)
 call NL_set_WF_sample(WF_sample,n_order,RT_step,WF_sample_range,v=E%nbf,k=k%nibz)
 if (Efield(1)%ef_name/='SOFTSIN'.and.Efield(1)%ef_name/='SIN') WF_sample%ison = .false.
 if (n_order > 0) then
   if (WF_sample%ison.eqv..false.) call warning('WF sampling switched off (only available with SOFTSIN/SIN field')
   if (WF_sample%n_order < n_order) call warning('WF sampling analysis order set to the maximum of 7')
 end if
 !
 ! LOG
 ! ===
 !
 call section('=','Messaging')
 !
 call msg( 'r','---General---')
 call msg( 'r', 'Integrator                 :',trim(Integrator_name))
 call msg( 'r', 'Simulation time        [fs]:',real(NE_steps-1)*RT_step*AUT2FS)
 call msg( 'r', 'Step length            [fs]:',RT_step*AUT2FS)
 call msg( 'r', 'NE Steps                   :',NE_steps)
 call msg( 'r', 'Correlation                :'//trim(NL_correlation))
 call msg( 'r', 'Damping                [eV]:',NL_damping*HA2EV)
 call msg( 'r', 'Dephasing              [fs]:',Phase_LifeTime*AUT2FS)
 call msg( 'r', 'Efield                     :'//trim(Efield(1)%ef_name))
 call msg( 'r', 'Efield width           [fs]:',Efield(1)%width*AUT2FS)
 call msg( 'r', 'Efield T_0             [fs]:',T_0)
 call msg( 'r', 'Efield Amplitude      [V/m]:',Efield(1)%amplitude*AU2VMm1)
 call msg( 'r', 'Frequencies range          :',NL_er*HA2EV)
 call msg( 'sr','Frequencies steps          :',n_frequencies)
 call msg( 'r', 'Frequency   step           :',NL_estep*HA2EV)
 call msg( 'r', 'Use Dipoles                :',l_use_DIPOLES)
 call msg( 'r', 'Induced Field              :',l_RT_induced_field)
 call msg( 'r', 'Force Snd Order Dipoles    :',l_force_SndOrd)
 call msg( 'r', 'Evaluate Current           :',l_eval_CURRENT)
 call msg( 'r', 'WF sampling                :',WF_sample%ison) 
 !
end subroutine NL_initialize

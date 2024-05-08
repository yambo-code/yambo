!
!        Copyright (C) 2000-2021 the YAMBO team
!            http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA IA
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
subroutine FL_initialize(E,k,q)
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
&                         Phase_LifeTime,RT_dyn_step,NE_i_time,RT_potential
 use real_time,      ONLY:RT_nbands,RT_bands
 use nl_optics,      ONLY:l_use_DIPOLES,NL_damping,NL_correlation,Correlation,NL_er,NL_estep,n_frequencies,&
&                         IPA,TDDFT,HARTREE,LRC,JGM,LRCW,SEX,HF,eval_dG,eval_COLLISIONS,NL_bands,NL_nbands,&
&                         NL_verb_name,VERB_HIGH,VERB_LOW,NL_verbosity,l_eval_CURRENT,n_runs,n_angles,n_frequencies
 use electric,       ONLY:l_force_SndOrd,l_P_periodic
 use fl_optics,      ONLY:l_FL_dephase,FL_Chi_order,FL_add_mode,max_time_step,max_fl_mode,max_extra_mode,max_duffin_mode, &
&                         FL_states,FL_space_dim,n_tot_time_steps,n_tot_fl_modes,n_tot_extra_modes,n_tot_duffin_modes,&
&                         l_FL_precond
 use parser_m,       ONLY:parser
 !
#include<memory.h>
 !
 type(levels), intent(in) :: E
 type(bz_samp),intent(in) :: q,k
 !
 ! Working space
 !
 integer  :: i1
 !
 ! Input variables: FL_Chi_order, FL_add_mode, NL_bands
 !
 max_time_step   = FL_Chi_order
 max_fl_mode     = FL_Chi_order+FL_add_mode
 max_extra_mode  = 2*max_fl_mode
 max_duffin_mode = 4*max_fl_mode
 !
 n_tot_time_steps   = 2*max_time_step+1
 n_tot_fl_modes     = 2*max_fl_mode+1
 n_tot_extra_modes  = 2*max_extra_mode+1
 n_tot_duffin_modes = 2*max_duffin_mode+1
 !
 n_runs=n_frequencies
 n_angles=1
 !
 NL_nbands=NL_bands(2)-NL_bands(1)+1
 !
 RT_bands =NL_bands
 RT_nbands=NL_nbands
 !
 FL_space_dim = NL_bands(2) * n_tot_fl_modes
 FL_states(1) = (NL_bands(1)-1)*n_tot_fl_modes+1
 FL_states(2) = FL_space_dim
 !
 call section('*','FL Initialization')
 !
 ! Parser
 ! ============
 !
 call parser('DensityPrecondition',  l_FL_precond)
 call parser('Dephase',              l_FL_dephase)
 call parser('FrSndOrd',             l_force_SndOrd)
 call parser('InducedField' ,        l_RT_induced_field)
 call parser('EvalCurrent' ,         l_eval_CURRENT)
 l_P_periodic = .true.
 !
 if(l_use_DIPOLES) call warning(' Fixed dipoles: only linear response properties are correct!')
 if(l_FL_dephase)  call warning(' Dephase in Floquet is only for testing purposes.')
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
     call Check_symmetries(Efield(i1)%versor,"electric")
     !
 enddo
 !
 if(Efield(1)%ef_name/='SOFTSIN'.and.Efield(1)%ef_name/='SIN') &
    call warning(' Non-linear optics properties can be obtained with SOFTSIN/SIN fields only!')
 !
 if(Efield(1)%ef_name/='SIN') then
    Efield(1)%ef_name = 'SIN'
    call warning(' Chainging to SIN field as Floquet assumes a SIN field by definition')
 endif
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
        call warning ('  [FL] HARTREE implementation in Floquet is still under development')
    case('HARTREE')
        Correlation=HARTREE
        RT_potential='HARTREE'
        eval_HARTREE =.true.
        call warning ('  [FL] HARTREE implementation in Floquet is still under development')
    case('TDDFT')  
        Correlation=TDDFT
        RT_potential='default'
        eval_HARTREE =.true.
        eval_DFT     =.true.
        call error ('  [FL] TDDFT not yet implemented in Floquet')
    case('LRC')
        Correlation=LRC
        RT_potential='HARTREE'
        eval_HARTREE =.true.
        call error ('  [FL] LRC not yet implemented in Floquet')
    case('LRW')
        Correlation=LRCW
        RT_potential='HARTREE'
        eval_HARTREE =.true.
        call error ('  [FL] LRW not yet implemented in Floquet')
    case('JGM')
        Correlation=JGM
        RT_potential='HARTREE'
        eval_HARTREE =.true.
        call error ('  [FL] JGM not yet implemented in Floquet')
    case('SEX')
        Correlation    =SEX
        RT_potential   ='HARTREE+SEX'
        eval_HARTREE   =.true.
        eval_dG        =.true.
        eval_COLLISIONS=.true.
        l_sc_coh       =.true.
        l_sc_sex       =.true.
        call error ('  [FL] SEX not yet implemented in Floquet')
    case('HF')
        Correlation=HF
        RT_potential   ='HARTREE+FOCK'
        eval_HARTREE   =.true.
        eval_dG        =.true.
        eval_COLLISIONS=.true.
        l_sc_hartree   =.true.
        l_sc_fock      =.true.
        call error ('  [FL] HF not yet implemented in Floquet')
    case default
        call error("Unknow correlation in the response function")
 end select
 !
 H_potential = RT_potential
 !
 ! Frequencies range
 ! =========================
 NL_estep=(NL_er(2)-NL_er(1))/real(n_frequencies,SP)
 !
 ! LOG
 ! ===
 !
 call section('+','Messaging')
 !
 call msg( 'r','---General---')
 call msg( 'r', 'Correlation                '//trim(NL_correlation))
 call msg( 'r', 'Damping                    ',NL_damping*HA2EV,"[eV]")
 call msg( 'r', 'Efield                     '//trim(Efield(1)%ef_name))
 call msg( 'r', 'Efield Amplitude      [V/m]',Efield(1)%amplitude*AU2VMm1)
 call msg( 'r', 'Frequencies range          ',NL_er*HA2EV)
 call msg( 'sr','Frequencies steps          ',n_frequencies)
 call msg( 'r', 'Frequency   step           ',NL_estep*HA2EV)
 call msg( 'r', 'Use Dipoles                ',l_use_DIPOLES)
 call msg( 'r', 'Induced Field              ',l_RT_induced_field)
 call msg( 'r', 'Force Snd Order Dipoles    ',l_force_SndOrd)
 call msg( 'r', 'Evaluate Current           ',l_eval_CURRENT)
 !
end subroutine FL_initialize

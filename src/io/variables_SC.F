!
!        Copyright (C) 2000-2014 the YAMBO team
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
integer function variables_SC(ID,local_description,what)
 !
 use pars,           ONLY:lchlen
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elel_corr
 use units,          ONLY:AU2Tesla,DEG2RAD,FS2AUT,AU2KWCMm2,HA2EV
 use D_lattice,      ONLY:atoms_string
 use R_lattice,      ONLY:nkibz
 use stderr,         ONLY:intc
 use electrons,      ONLY:BZ_RIM_tot_nkpts
 use QP_m,           ONLY:SC_band_mixing,COHSEX_use_empties
 use SC,             ONLY:SC_bands,it_now,SC_potential,SC_kind,SC_perturbation,l_sc_diagonal,&
&                         SC_xc_functional,V_hartree_sc,l_NSC_shot,SC_fft_size,SC_up_W_iters,&
&                         SC_ng_Sx,SC_X_bands,SC_X_size,SC_description,n_SC_descriptions,local_V,RT_nk
 use IO_m,           ONLY:io_elemental,io_status,ver_is_gt_or_eq,IO_INCOMPATIBLE_VAR,&
&                         variable_is_found
 use real_time,      ONLY:l_NE_with_fields,RT_step,NE_steps,NE_time,              &
&                         Thermal_steps,Integrator_name,l_RT_diagonalize,RT_diagonalization_steps, &
&                         RAD_LifeTime,Phase_LifeTime,l_RT_diamagnetic,RT_IO_G_steps,              &
&                         RT_IO_JPO_steps,l_RT_induced_field,G_MEM_steps,&
&                         l_RT_CCA_Kernel,RT_IO_steps_DB,Gr_description,&
&                         l_RT_uses_E_RIM,N_Gr_descs,RT_dynamics_steps,RT_dynamics_decay_time,&
&                         RT_dynamics_KIND,l_NE_simulate_Irredux
#if defined _MAGNETIC
 use magnetic,       ONLY:MAG_hamiltonian_type,MAG_B,MAG_radius,MAG_gauge,MAG_theta,MAG_psi
 use global_XC,      ONLY:EXT_B_LANDAU,EXT_B_PAULI,EXT_B_MAGNETISM
#endif
#if defined _ELPH
 use ELPH,           ONLY:ph_modes,elph_branches
#endif
 use global_XC,      ONLY:SC_EXX,SC_HF,SC_COHSEX,SC_EXXC
 use fields,         ONLY:global_gauge,n_ext_fields,Efield
 implicit none
 !
 integer            ::ID
 character(lchlen)  ::local_description(100)
 character(*)       ::what
 !
 ! Work Space
 !
 integer               :: SC_kind_disk,SC_perturbation_disk,&
&                         io_status_save,i1,i_desc,local_size,VAR_SIZE,i_dummy
 logical               :: CHECK_,l_J,l_G,l_occupations,l_NE_with_fields_disk,l_MP
 character(lchlen)     :: ch
 !
 l_J =.FALSE.
 l_G =.FALSE.
 l_occupations =what=="occupations"
 l_MP =what=="MP"
#if defined _RT || defined _YPP_RT 
 l_J =what=="J_and_P"
 l_G =what=="G"
#endif
 !
 i_desc=0
 local_description=' '
 !
 CHECK_=.TRUE.
 if (l_NSC_shot) CHECK_=.FALSE.
 !
 call io_elemental(ID,VAR='ATOMS_STRING',CH0="",VAR_SZ=1,MENU=0)
 i_desc=i_desc+1
 call io_elemental(ID,CH0=atoms_string,&
&     VAR=' Unit cell atoms                 :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 call io_elemental(ID,VAR='XC_FUNCTIONAL',CH0="",VAR_SZ=1,MENU=0)
 i_desc=i_desc+1
 call io_elemental(ID,CH0=SC_potential,&
&     VAR=' xc Functional                   :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 call io_elemental(ID,VAR="XC_GLOBALS",VAR_SZ=3,MENU=0)
 call io_elemental(ID,I0=SC_perturbation,DB_I0=SC_perturbation_disk)
 call io_elemental(ID,I0=SC_kind,DB_I0=SC_kind_disk)
 call io_elemental(ID,I0=SC_xc_functional)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 io_status_save=io_status(ID)
 !
 call io_elemental(ID,VAR="LOCAL_ITER_BANDS_MIX",VAR_SZ=6,MENU=0) 
 !
 i_desc=i_desc+1
 call io_elemental(ID,L0=local_V,&
&     VAR=' The potential is local          :',DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,I0=it_now,DB_I0=it_now,&
&     VAR=' Iteration                       :',DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,I1=SC_bands,&
&     VAR=' Bands                           :',CHECK=CHECK_,OP=(/"==","=="/),&
&     DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
#if defined _RT
 call io_elemental(ID,I0=RT_nk,&
&     VAR=' Number of K-points              :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
#endif
#if defined _SC && !defined _RT
 call io_elemental(ID,I0=nkibz,&
&     VAR=' Number of K-points              :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
#endif
 i_desc=i_desc+1
 call io_elemental(ID,R0=SC_band_mixing,&
&     VAR=' Bands mixing                    :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 if (any((/SC_kind_disk==SC_EXX,SC_kind_disk==SC_EXXC,SC_kind_disk==SC_HF,SC_kind_disk==SC_COHSEX/))) then
   call io_elemental(ID,VAR="SIGMAx_G",VAR_SZ=1,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,I0=SC_ng_Sx,&
&       VAR=' Sx RL components                :',CHECK=CHECK_,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 !
 if ( SC_kind_disk==SC_COHSEX   ) then
   call io_elemental(ID,VAR="X_bands_size",VAR_SZ=6,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,I1=SC_X_bands,&
&       VAR=' Response function bands         :',CHECK=CHECK_,OP=(/"==","=="/),&
&       DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=SC_X_size,&
&       VAR=' Response function size          :',CHECK=CHECK_,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=COHSEX_use_empties,&
&     VAR=" COHSEX use empty bands          :", CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=SC_up_W_iters,&
&     VAR=' Updating W every                :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_sc_diagonal,&
&     VAR=' Diagonal approximation          :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 !
 if (ver_is_gt_or_eq(ID,revision=2877)) then
   if (all((/SC_kind_disk/=SC_HF,SC_kind_disk/=SC_COHSEX/)).or.l_MP) then
     call io_elemental(ID,VAR="FFT_SIZE",VAR_SZ=1,MENU=0,DESCRIPTION=local_description(i_desc))         
     i_desc=i_desc+1
     call io_elemental(ID,I0=SC_fft_size,&
&         VAR=' FFT mesh points                 :',CHECK=.true.,OP=(/"=="/),&
&         DESCRIPTION=local_description(i_desc))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
 endif
 !
#if defined _ELPH
 !
 VAR_SIZE=4
 if (ver_is_gt_or_eq(ID,revision=3353)) VAR_SIZE=5
 call io_elemental(ID,VAR="ELPH_vars",VAR_SZ=VAR_SIZE,MENU=0) 
 !
 i_desc=i_desc+1
 call io_elemental(ID,L0=l_elph_scatt,&
&   VAR=" [E-P] Electron-phonon           :", CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,L0=l_RT_CCA_Kernel,&
&   VAR=" [E-P] Completed Collision(CCA)  :", CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 if (ver_is_gt_or_eq(ID,revision=3353)) then
   call io_elemental(ID, &
&     VAR=" [E-P] Branches                  :",I1=elph_branches,CHECK=.true.,OP=(/"==","=="/),&
&     DESCRIPTION=local_description(i_desc))
 else
   call io_elemental(ID,&
&     VAR=" [E-P] Phonon modes              :",I0=ph_modes,CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
 endif
 i_desc=i_desc+1
 call io_elemental(ID, &
&   VAR=" [E-P] Memory steps              :",I0=G_MEM_steps,CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTION=local_description(i_desc))
 !
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
#endif
 !
#if defined _RT
 if (ver_is_gt_or_eq(ID,revision=2877)) then
   call io_elemental(ID,VAR="E-E_vars",VAR_SZ=2,MENU=0) 
 else
   call io_elemental(ID,VAR="CARRIER_scattering_vars",VAR_SZ=2,MENU=0) 
 endif
 i_desc=i_desc+1
 call io_elemental(ID,L0=l_elel_scatt,&
&   VAR=" [E-E] Coulomb scattering        :", CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTION=local_description(i_desc))
 if (ver_is_gt_or_eq(ID,revision=2877)) then
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_elel_corr,&
&    VAR=" [E-E] Coulomb correlation       :", CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
 endif
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
#endif
 !
 if (io_status_save==0.and.io_status(ID)<0) then
   io_status(ID)=IO_INCOMPATIBLE_VAR
   if (l_NSC_shot) io_status(ID)=0
 endif
 !
 if (.not.ver_is_gt_or_eq(ID,revision=2877)) then
   if (all((/SC_kind_disk/=SC_HF,SC_kind_disk/=SC_COHSEX/)).or.l_MP) then
     call io_elemental(ID,VAR="FFT_SIZE",VAR_SZ=1,MENU=0,DESCRIPTION=local_description(i_desc))         
     i_desc=i_desc+1
     call io_elemental(ID,I0=SC_fft_size,&
&         VAR=' FFT mesh points                 :',CHECK=.true.,OP=(/"=="/),&
&         DESCRIPTION=local_description(i_desc))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
 endif
 !
#if defined _MAGNETIC
 if (any((/SC_perturbation_disk==EXT_B_LANDAU,SC_perturbation_disk==EXT_B_PAULI,&
&          SC_perturbation_disk==EXT_B_MAGNETISM/))) then
   !
   call io_elemental(ID,VAR="MAG_hamiltonian",CH0="",VAR_SZ=1,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,CH0=MAG_hamiltonian_type, &
&       VAR=' [MAG] Hamiltonian kind          :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR='B_FIELD_parameters',VAR_SZ=4,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,UNIT=AU2Tesla,R0=MAG_B,&
&       VAR=' [MAG] B modulus              [T]:',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   !
   i_desc=i_desc+1
   call io_elemental(ID,UNIT=(1./DEG2RAD),R0=MAG_theta,& 
&       VAR=' [MAG] B theta angle    [degrees]:',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,UNIT=(1./DEG2RAD),R0=MAG_psi,& 
&       VAR=' [MAG] B psi angle      [degrees]:',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   !
   if (SC_perturbation_disk/=EXT_B_PAULI) then
     i_desc=i_desc+1
     call io_elemental(ID,R0=MAG_radius,&
&       VAR=' [MAG] B flux radius             :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   endif
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR='MAG_GAUGE',CH0="",VAR_SZ=1,MENU=0)
   i_desc=i_desc+1
   call io_elemental(ID,CH0=MAG_gauge,&
&       VAR=' [MAG] Gauge                     :',CHECK=CHECK_,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
 endif
 !
#endif
 !
 if(.not.any((/l_J,l_G,l_occupations/))) call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 if(any((/l_J,l_G,l_occupations/))) then
   !
   VAR_SIZE=9
   call io_elemental(ID,VAR="RT_DYNAMICS",VAR_SZ=VAR_SIZE,MENU=0) 
   !
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_NE_simulate_Irredux,&
&    VAR=" [RT] Simulate irredux           :", CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   !
   i_desc=i_desc+1
   call io_elemental(ID,&
&    VAR=  " [RT] Time step              [fs]:",R0=RT_step,CHECK=.true.,OP=(/"=="/),UNIT=1./FS2AUT,&
&    DESCRIPTION=local_description(i_desc))
   !
   if(variable_is_found(ID,"TEMPERATURES")/=0) then
     if (ver_is_gt_or_eq(ID,revision=3163)) then
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [RT-DYN-dT] Inter/intra grps[fs]:",R1=RT_dynamics_steps,WARN=.true.,OP=(/"==","=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [RT-DYN-dT] Time decay      [fs]:",R0=RT_dynamics_decay_time,WARN=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
       i_desc=i_desc+1
       call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
       call io_elemental(ID,VAR='RT_DYNAMICS_kind',CH0="",VAR_SZ=1,MENU=0)
       call io_elemental(ID,CH0=RT_dynamics_KIND, &
&        VAR=" [RT-DYN-dT] Sampling            :",WARN=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc))
       call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
       call io_elemental(ID,VAR="RT_DYNAMICS_more",VAR_SZ=3,MENU=0) 
     else
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [RT] Time jumps                 :",I0=i_dummy,WARN=.true.,OP=(/"=="/),DESCRIPTION=local_description(i_desc))
     endif
     !
   endif
   !
   i_desc=i_desc+1
   call io_elemental(ID,&
&    VAR=" [RT] Time steps    (planned)    :",I0=NE_steps,WARN=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,&
&    VAR=" [RT] NE time       (reached)[fs]:",R0=NE_time,WARN=.true.,OP=(/"=="/),UNIT=1./FS2AUT,&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID, &
&    VAR=" [RT] Thermalization steps       :",I0=Thermal_steps,CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="INTEGRATOR",CH0="",VAR_SZ=1,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,CH0=Integrator_name, &
&    VAR=' [RT] Integrator                 :',CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="RT_OTHERS",VAR_SZ=8,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_diagonalize,&
&    VAR=' [RT] QP analysis                :',CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=RT_diagonalization_steps,&
&    VAR=' [RT] Diagonalization steps      :',CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   if (l_J.or.l_occupations) then
     call io_elemental(ID,DB_I0=RT_IO_steps_DB,I0=RT_IO_JPO_steps,&
&      VAR=' [RT] IO steps                   :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
     i_desc=i_desc+1
   else if (l_G) then
     call io_elemental(ID,I0=RT_IO_G_steps,&
&      VAR=' [RT] IO steps                   :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
     i_desc=i_desc+1
   endif
   call io_elemental(ID,R0=RAD_LifeTime,&
&    VAR=' [RT] Radiative life time    [fs]:', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
   i_desc=i_desc+1
   call io_elemental(ID,R0=Phase_LifeTime,&
&    VAR=' [RT] Dephasing life time    [fs]:', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_NE_with_fields,DB_L0=l_NE_with_fields_disk,&
&    VAR=' [RT] External Field             :', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_diamagnetic,&
&    VAR=' [RT] Diamagnetic term           :',  CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_induced_field,&
&    VAR=' [RT] Induced field              :',  CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! G retarded variables
   !======================
   if (ver_is_gt_or_eq(ID,revision=2877)) then
     call io_elemental(ID,VAR="G_retarded_BASE",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,I0=N_Gr_descs)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     do i1=1,N_Gr_descs
       write (ch,'(a,i5.5)') 'G_ret_DESC_strings_',i1
       call io_elemental(ID,VAR=trim(ch),CH0="",VAR_SZ=1,MENU=0)
       i_desc=i_desc+1
       call io_elemental(ID,CH0=Gr_description(i1),VAR='',DESCRIPTION=local_description(i_desc))
       call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     enddo
   endif
   !
   ! RIM
   !======================
   VAR_SIZE=2
   if (ver_is_gt_or_eq(ID,revision=3387)) VAR_SIZE=3
   call io_elemental(ID,VAR="RIM_support",VAR_SZ=VAR_SIZE,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_uses_E_RIM,&
&    VAR=' [RIM] Random Energies support   :',  CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=BZ_RIM_tot_nkpts,&
&    VAR=' [RIM] Random k-point            :',  CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   if (ver_is_gt_or_eq(ID,revision=3387)) then
     i_desc=i_desc+1
     call io_elemental(ID,R0=RIM_EE_percent,&
&      VAR=' [RIM] Used for e-e         [o/o]:',  CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   endif
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! CONSTRAINS
   !======================
   if (ver_is_gt_or_eq(ID,revision=3387)) then
     call io_elemental(ID,VAR="CONSTRAINS",VAR_SZ=VAR_SIZE,MENU=0) 
     i_desc=i_desc+1
     call io_elemental(ID,L0=.not.l_RT_skip_impose,&
&      VAR=' [CONSTRAINS] N (ee+ep) & dN (ep):',  CHECK=.true.,OP=(/"=="/),&
&      DESCRIPTION=local_description(i_desc))
     i_desc=i_desc+1
     call io_elemental(ID,L0=l_RT_EE_impose_E,&
&      VAR=' [CONSTRAINS] IP Energy      (ee):',  CHECK=.true.,OP=(/"=="/),&
&      DESCRIPTION=local_description(i_desc))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   !
   ! FIELDS
   !======================
   if (l_NE_with_fields_disk) then
     !
     call io_elemental(ID,VAR="GAUGE_TYPE",CH0="",VAR_SZ=1,MENU=0)
     i_desc=i_desc+1
     call io_elemental(ID,CH0=global_gauge, &
&       VAR=' [FIELDs] Gauge Type             :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     do i1=1,n_ext_fields
       call io_elemental(ID,VAR="FIELD_NAME"//trim(intc(i1)),CH0="",VAR_SZ=1,MENU=0)
       i_desc=i_desc+1
       call io_elemental(ID,CH0=Efield(i1)%ef_name, &
&        VAR=' [FIELDs] Type                   :',CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc))
       call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
       !
       local_size=10
       call io_elemental(ID,VAR="EXTERNAL_FIELD"//trim(intc(i1)),VAR_SZ=local_size,MENU=0)
       !
       i_desc=i_desc+1
       call io_elemental(ID,R1=Efield(i1)%versor,WARN=.true., &
&        VAR=" [FIELDs] Versor                 :",OP=(/"==","==","=="/),&
&        DESCRIPTION=local_description(i_desc))
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [FIELDs] Intensity      [kWCMm2]:",R0=Efield(i1)%intensity,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=AU2KWCMm2)
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [FIELDs] Damping            [fs]:",R0=Efield(i1)%damping,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [FIELDs] Frequency range    [eV]:",R1=Efield(i1)%frequency,CHECK=.true.,OP=(/"==","=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=HA2EV)
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [FIELDs] Frequency steps        :",I0=Efield(i1)%n_frequencies,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc))
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [FIELDs] Frequency step     [eV]:",R0=Efield(i1)%W_step,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=HA2EV)
       i_desc=i_desc+1
       call io_elemental(ID,R0=Efield(i1)%t_initial,&
&        VAR=' [FIELDs] Initial time       [fs]:',CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
       !                       
       if (i1/=n_ext_fields) call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
       if (i1==n_ext_fields) call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
     enddo
     !
   else
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   endif
 endif
 !
 variables_SC=io_status(ID)
 n_SC_descriptions=i_desc
 SC_description(:i_desc)=local_description(:i_desc)
 !
end function

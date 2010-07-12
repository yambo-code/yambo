!
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
integer function variables_SC(ID,local_description,what)
 !
 use pars,           ONLY:lchlen
 use units,          ONLY:AU2Tesla,radian,FS2AUT,W2Au,HARTREE
 use D_lattice,      ONLY:atoms_string
 use stderr,         ONLY:intc
 use QP_m,           ONLY:SC_band_mixing
 use SC,             ONLY:SC_bands,it_now,SC_potential,SC_kind,SC_perturbation,&
&                         SC_xc_functional,V_hartree_sc,l_NSC_shot,SC_fft_size,&
&                         SC_ng_Sx,SC_X_bands,SC_X_size,SC_description,n_SC_descriptions
 use IO_m,           ONLY:io_elemental,io_status,ver_is_gt_or_eq,IO_INCOMPATIBLE_VAR
#if defined _RT
 use real_time,      ONLY:l_NE_with_fields,RT_step,NE_steps,Hfrozen,XCfrozen,                      &
&                         Thermal_steps,Integrator_name,l_RT_diagonalize,RT_diagonalization_steps, &
&                         QP_LifeTime,Phase_LifeTime,l_RT_no_diamagnetic,&
&                         max_rt_steps_disk,l_RT_induced_field
 use fields,         ONLY:n_ext_fields,Efield
#endif                                
#if defined _MAGNETIC
 use magnetic,       ONLY:MAG_hamiltonian_type,MAG_B,MAG_radius,MAG_gauge,MAG_theta,MAG_psi
 use global_XC,      ONLY:EXT_B_LANDAU,EXT_B_PAULI,EXT_B_MAGNETISM
#endif
 use global_XC,      ONLY:SC_EXX,SC_HF,SC_COHSEX,SC_EXXC
 use fields,         ONLY:global_gauge
 implicit none
 !
 integer            ::ID
 character(lchlen)  ::local_description(100)
 character(*)       ::what
 !
 ! Work Space
 !
 integer               :: SC_kind_disk,SC_perturbation_disk,&
&                         io_status_save,i1,i_desc
 logical               :: CHECK_,Local_V
#if defined _RT		
 logical               :: l_J,l_G,l_NE_with_fields_disk
#endif
 !
#if defined _RT		
 l_J =what=="J"
 l_G =what=="G"
#endif
 !
 i_desc=0
 CHECK_=.TRUE.
 if (l_NSC_shot) CHECK_=.FALSE.
 !
 call io_elemental(ID,VAR='ATOMS_STRING',CH0="",VAR_SZ=1)
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
 if (ver_is_gt_or_eq(ID,revision=455)) then
   call io_elemental(ID,VAR="XC_GLOBALS",VAR_SZ=3,MENU=0)
   call io_elemental(ID,I0=SC_perturbation,DB_I0=SC_perturbation_disk)
 else
   call io_elemental(ID,VAR="XC_KIND",VAR_SZ=2,MENU=0)
 endif
 call io_elemental(ID,I0=SC_kind,DB_I0=SC_kind_disk)
 call io_elemental(ID,I0=SC_xc_functional)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 io_status_save=io_status(ID)
 call io_elemental(ID,VAR="LOCAL_ITER_BANDS_MIX",VAR_SZ=4,MENU=0) 
 i_desc=i_desc+1
 call io_elemental(ID,L0=Local_V,&
&     VAR=' The potential is local          :',DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,I0=it_now,DB_I0=it_now,&
&     VAR=' Iteration                       :',DESCRIPTION=local_description(i_desc))
 !
 i_desc=i_desc+1
 call io_elemental(ID,I0=SC_bands,&
&     VAR=' Total number of bands           :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
 !
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
   call io_elemental(ID,VAR="X_bands_size",VAR_SZ=3,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,I1=SC_X_bands,&
&       VAR=' Response function bands         :',CHECK=CHECK_,OP=(/"==","=="/),&
&       DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=SC_X_size,&
&       VAR='                   size          :',CHECK=CHECK_,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 !
 if (io_status_save==0.and.io_status(ID)<0) then
   io_status(ID)=IO_INCOMPATIBLE_VAR
   if (l_NSC_shot) io_status(ID)=0
 endif
 !
 if (all((/SC_kind_disk/=SC_HF,SC_kind_disk/=SC_COHSEX/))) then
   call io_elemental(ID,VAR="FFT_SIZE",VAR_SZ=1,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,I0=SC_fft_size,&
&       VAR=' FFT mesh points                 :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
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
   if (ver_is_gt_or_eq(ID,revision=492)) then
     i_desc=i_desc+1
     call io_elemental(ID,UNIT=(1./radian),R0=MAG_theta,& 
&         VAR=' [MAG] B theta angle    [degrees]:',CHECK=.true.,OP=(/"=="/),&
&         DESCRIPTION=local_description(i_desc))
     i_desc=i_desc+1
     call io_elemental(ID,UNIT=(1./radian),R0=MAG_psi,& 
&         VAR=' [MAG] B psi angle      [degrees]:',CHECK=.true.,OP=(/"=="/),&
&         DESCRIPTION=local_description(i_desc))
   endif
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
#if defined _RT
 !
 if(any((/l_J,l_G/))) then
   call io_elemental(ID,VAR="RT_DYNAMICS",VAR_SZ=5,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,L0=Hfrozen,&
&    VAR=" [RT] Hartree not updated        :", CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=XCfrozen,&
&    VAR=" [RT] XC term frozen             :", CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,&
&    VAR=" [RT] Time step              [fs]:",R0=RT_step,CHECK=.true.,OP=(/"=="/),UNIT=1./FS2AUT,&
&    DESCRIPTION=local_description(i_desc))
   !
   if(l_G) then
     i_desc=i_desc+1
     call io_elemental(ID, &
&      VAR=" [RT] Time steps                 :",I0=NE_steps,CHECK=.true.,OP=(/">="/),&
&      DESCRIPTION=local_description(i_desc))
   endif
   !
   i_desc=i_desc+1
   call io_elemental(ID, &
&    VAR=" [RT] Thermalization steps       :",I0=Thermal_steps,CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="INTEGRATOR",CH0="",VAR_SZ=1,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,CH0=Integrator_name, &
&    VAR=' [RT] Integrator                 :',CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="RT_OTHERS",VAR_SZ=9,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_diagonalize,&
&    VAR=' [RT] QP analysis                :',CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=RT_diagonalization_steps,&
&    VAR=' [RT] Diagonalization steps      :',CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=max_rt_steps_disk,&
&    VAR=' [RT] Cache size                 :',CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,R0=QP_LifeTime,&
&    VAR=' [RT] Quasi-particle life time   :', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   if (ver_is_gt_or_eq(ID,revision=605)) then
     call io_elemental(ID,R0=Phase_LifeTime,&
&      VAR=' [RT] Dephasing life time        :', CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
     i_desc=i_desc+1
   endif
   call io_elemental(ID,L0=l_NE_with_fields,DB_L0=l_NE_with_fields_disk,&
&    VAR=' [RT] External Field             :', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_no_diamagnetic,&
&    VAR=' [RT] No diamagnetic term        :',  CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_induced_field,&
&    VAR=' [RT] Include induced field      :',  CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   !
   if (l_NE_with_fields_disk) then
     !
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     call io_elemental(ID,VAR="GAUGE_TYPE",CH0="",VAR_SZ=1,MENU=0)
     i_desc=i_desc+1
     call io_elemental(ID,CH0=global_gauge, &
&       VAR=' [RT] Gauge Type                 :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     do i1=1,n_ext_fields
       call io_elemental(ID,VAR="FIELD_NAME"//trim(intc(i1)),CH0="",VAR_SZ=1,MENU=0)
       i_desc=i_desc+1
       call io_elemental(ID,CH0=Efield(i1)%ef_name, &
&       VAR=' [FIELDs] Field                 :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
       call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
       !
       call io_elemental(ID,VAR="EXTERNAL_FIELD"//trim(intc(i1)),VAR_SZ=7,MENU=0)
       i_desc=i_desc+1
       call io_elemental(ID,R1=Efield(i1)%versor,CHECK=.true., &
&       VAR=" [FIELDs] Field versor           :",OP=(/"==","==","=="/),&
&       DESCRIPTION=local_description(i_desc))
       i_desc=i_desc+1
       call io_elemental(ID,&
&       VAR=" [FIELDs] Field intensity [kWLm2]:",R0=Efield(i1)%intensity,CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc),UNIT=1./W2Au)
       i_desc=i_desc+1
       call io_elemental(ID,&
&       VAR=" [FIELDs] Field damping     [eV]:",R0=Efield(i1)%damping,CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc),UNIT=HARTREE)
       i_desc=i_desc+1
       call io_elemental(ID,&
&       VAR=" [FIELDs] Field frequency    [eV]:",R0=Efield(i1)%frequency,CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc),UNIT=HARTREE)
       i_desc=i_desc+1
       call io_elemental(ID,R0=Efield(i1)%t_initial,&
&       VAR=' [FIELDs] Initial time       [fs]:',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
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
#endif
 !
 variables_SC=io_status(ID)
 n_SC_descriptions=i_desc
 SC_description(:i_desc)=local_description(:i_desc)
 !
end function

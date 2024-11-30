!
! License-Identifier: GPL
!
! Copyright (C) 2010 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
integer function variables_SC(ID,what)
 !
 use units,          ONLY:AU2Tesla,DEG2RAD,AU2VMm1
 use D_lattice,      ONLY:atoms_string
 use R_lattice,      ONLY:nkibz
 use QP_m,           ONLY:SC_band_mixing,COHSEX_use_empties
 use SC,             ONLY:SC_bands,it_now,SC_potential,SC_kind,SC_perturbation,l_sc_diagonal,&
&                         SC_xc_functional,l_NSC_shot,SC_fft_size,SC_up_W_iters,&
&                         SC_ng_Sx,SC_X_bands,SC_X_size,SC_desc
 use hamiltonian,    ONLY:l_sc_V_is_local,H_ref_bands
 use IO_int,         ONLY:io_elemental
 use IO_m,           ONLY:io_status,IO_INCOMPATIBLE_VAR,read_is_on
 use magnetic,       ONLY:MAG_hamiltonian_type,MAG_B,MAG_radius,MAG_gauge,MAG_theta,MAG_psi
 use global_XC,      ONLY:EXT_B_LANDAU,EXT_B_PAULI,EXT_B_MAGNETISM
 use drivers,        ONLY:l_sc_electric
 use fields,         ONLY:Efield
 use descriptors,    ONLY:IO_desc_reset
 use global_XC,      ONLY:H_SE_EXX,H_SE_FOCK,H_SE_COHSEX,H_SE_COH,H_SE_SEX,H_SE_EXXC,H_SE_HARTREE
 !
 implicit none
 !
 integer            ::ID
 character(*)       ::what
 !
 ! Work Space
 !
 integer               :: SC_kind_disk,SC_perturbation_disk,io_status_save,MENU_
 logical               :: CHECK_,l_MP
 !
 l_MP =what=="MP"
 !
 CHECK_=.TRUE.
 MENU_ =0
 if (l_NSC_shot) CHECK_=.FALSE.
 !
 call IO_desc_reset(SC_desc)
 !
 ! ATOMS_STRING
 !
 call io_elemental(ID,VAR='ATOMS_STRING',CH0="",VAR_SZ=1,MENU=0)
 call io_elemental(ID,CH0=atoms_string,&
&     VAR='[SC] Unit cell atoms',CHECK=CHECK_,OP=(/"=="/),DESCRIPTOR=SC_desc)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 !
 ! XC_FUNCTIONAL
 !
 call io_elemental(ID,VAR='XC_FUNCTIONAL',CH0="",VAR_SZ=1,MENU=0)
 call io_elemental(ID,CH0=SC_potential,&
&     VAR='[SC] xc Functional',CHECK=CHECK_,OP=(/"=="/),DESCRIPTOR=SC_desc)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 !
 ! XC_GLOBALS
 !
 call io_elemental(ID,VAR="XC_GLOBALS",VAR_SZ=3,MENU=0)
 call io_elemental(ID,I0=SC_perturbation,DB_I0=SC_perturbation_disk)
 call io_elemental(ID,I0=SC_kind,DB_I0=SC_kind_disk)
 call io_elemental(ID,I0=SC_xc_functional)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 io_status_save=io_status(ID)
 !
 ! LOCAL_ITER_BANDS_MIX
 !
 call io_elemental(ID,VAR="LOCAL_ITER_BANDS_MIX",VAR_SZ=6,MENU=0) 
 !
 call io_elemental(ID,L0=l_sc_V_is_local,VAR='[SC] Local Potential',DESCRIPTOR=SC_desc)
 call io_elemental(ID,I0=it_now,DB_I0=it_now,VAR='[SC] Iteration',DESCRIPTOR=SC_desc)
 call io_elemental(ID,I1=SC_bands,VAR='[SC] Bands',CHECK=CHECK_,OP=(/"==","=="/),DESCRIPTOR=SC_desc)
 if (read_is_on(ID)) H_ref_bands=SC_bands
 call io_elemental(ID,I0=nkibz,VAR='[SC] K-points',CHECK=CHECK_,OP=(/"=="/),DESCRIPTOR=SC_desc)
 call io_elemental(ID,R0=SC_band_mixing,VAR='[SC] Bands mixing',CHECK=CHECK_,OP=(/"=="/),DESCRIPTOR=SC_desc)
 !
 if (any(SC_kind_disk==(/H_SE_EXX,H_SE_EXXC,H_SE_FOCK,H_SE_SEX,H_SE_COHSEX/)).or.&
&    any(SC_kind_disk-H_SE_Hartree==(/H_SE_EXX,H_SE_EXXC,H_SE_FOCK,H_SE_SEX,H_SE_COHSEX/))) then
   call io_elemental(ID,VAR="SIGMAx_G",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   call io_elemental(ID,I0=SC_ng_Sx,VAR='[SC] Sx RL components',CHECK=CHECK_,OP=(/"=="/),DESCRIPTOR=SC_desc)
 endif
 !
 if (any(SC_kind_disk==(/H_SE_COH,H_SE_SEX,H_SE_COHSEX/)).or.&
&    any(SC_kind_disk-H_SE_Hartree==(/H_SE_COH,H_SE_SEX,H_SE_COHSEX/))) then
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   call io_elemental(ID,VAR="X_bands_size",VAR_SZ=6,MENU=0) 
   call io_elemental(ID,I1=SC_X_bands,VAR='[SC] Response function bands',CHECK=CHECK_,OP=(/"==","=="/),&
&       DESCRIPTOR=SC_desc)
   call io_elemental(ID,I0=SC_X_size,VAR='[SC] Response function size',CHECK=CHECK_,OP=(/"=="/),&
&       DESCRIPTOR=SC_desc)
   call io_elemental(ID,L0=COHSEX_use_empties,VAR="[SC] COHSEX use empty bands", CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTOR=SC_desc)
   call io_elemental(ID,I0=SC_up_W_iters,VAR='[SC] W update steps',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTOR=SC_desc)
   call io_elemental(ID,L0=l_sc_diagonal,VAR='[SC] Diagonal approximation',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTOR=SC_desc)
 endif
 !
 if (all(SC_kind_disk/=(/H_SE_FOCK,H_SE_COHSEX,H_SE_COH,H_SE_SEX/)).or.&
&    all(SC_kind_disk-H_SE_HARTREE/=(/H_SE_FOCK,H_SE_COHSEX,H_SE_COH,H_SE_SEX/)).or.l_MP) then
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   call io_elemental(ID,VAR="FFT_SIZE",VAR_SZ=1,MENU=0,DESCRIPTOR=SC_desc)
   call io_elemental(ID,I0=SC_fft_size,VAR='[SC] FFT mesh points',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTOR=SC_desc)
 endif
 !
 if (io_status_save==0.and.io_status(ID)<0) then
   io_status(ID)=IO_INCOMPATIBLE_VAR
   if (l_NSC_shot) io_status(ID)=0
 endif
 !
 if (any((/SC_perturbation_disk==EXT_B_LANDAU,SC_perturbation_disk==EXT_B_PAULI,&
&          SC_perturbation_disk==EXT_B_MAGNETISM/))) then
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   call io_elemental(ID,VAR="MAG_hamiltonian",CH0="",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,CH0=MAG_hamiltonian_type, VAR='[MAG] Hamiltonian kind',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTOR=SC_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR='B_FIELD_parameters',VAR_SZ=4,MENU=0) 
   call io_elemental(ID,UNIT=AU2Tesla,R0=MAG_B,VAR='[MAG] B modulus',TERMINATOR='[T]',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTOR=SC_desc)
   !
   call io_elemental(ID,UNIT=(1./DEG2RAD),R0=MAG_theta,& 
&       VAR='[MAG] B theta angle',TERMINATOR='[degrees]',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTOR=SC_desc)
   call io_elemental(ID,UNIT=(1./DEG2RAD),R0=MAG_psi,& 
&       VAR='[MAG] B psi angle',TERMINATOR='[degrees]',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTOR=SC_desc)
   !
   if (SC_perturbation_disk/=EXT_B_PAULI) then
     call io_elemental(ID,R0=MAG_radius,VAR='[MAG] B flux radius',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=SC_desc)
   endif
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR='MAG_GAUGE',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=MAG_gauge,VAR='[MAG] Gauge',CHECK=CHECK_,OP=(/"=="/),DESCRIPTOR=SC_desc)
   !
 endif
 !
 if(l_sc_electric) then
   call io_elemental(ID,VAR="ELECTRIC_hamiltonian",CH0="",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,VAR='E_FIELD_parameters',VAR_SZ=4,MENU=0) 
   call io_elemental(ID,UNIT=AU2VMm1,R0=Efield(1)%amplitude,&
&     VAR='[ELECTRIC] E intensity',TERMINATOR='[v/m]',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=SC_desc)
   call io_elemental(ID,VAR="[ELECTRIC] E Versor",R1=Efield(1)%versor, &
&     WARN=.true.,OP=(/"==","==","=="/),DESCRIPTOR=SC_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 endif
 !
 if (what/="carriers".and.what/="THETA".and.what/="OBS".and.what/="G".and.what/="REF") then
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 else
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 !
 variables_SC=io_status(ID)
 !
 if (variables_SC/=0) return
 !
end function

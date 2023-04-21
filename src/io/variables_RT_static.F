!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
#if defined _RT
integer function variables_RT_static(ID)
 !
 use pars,           ONLY:SP,LP,IP,IP_YIO,CR_YIO,LP_YIO
 use units,          ONLY:AUT2FS,AU2KWCMm2,HA2EV
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elel_corr,l_elphoton_scatt
 use stderr,         ONLY:intc
 use hamiltonian,    ONLY:H_ref_bands
 use descriptors,    ONLY:IO_desc_reset
 use R_lattice,      ONLY:FineGd_desc
 use RT_control,     ONLY:RT_do_IO
 use RT_output_m,    ONLY:RT_desc,Gr_desc,n_RT_desc_static
 use RT_lifetimes,   ONLY:l_RT_lifetimes_extrapolation,RT_life_extrap_times_INPUT,l_RT_iterative_ELPH_lifetimes 
 use real_time,      ONLY:RT_nk,l_NE_with_fields,RT_step, &
&                         Integrator_name,RT_bands,RAD_LifeTime,Phase_LifeTime,&
&                         l_RT_induced_field,G_MEM_steps,NE_tot_time,NE_steps,&
&                         l_RT_impose_N_dN,l_RT_uses_E_FineGd,DbGd_EE_percent,l_RT_impose_E,RT_potential
 use IO_m,           ONLY:io_sec,io_status,read_is_on,variable_exist
#if defined _ELPH
 use ELPH,           ONLY:elph_branches
#endif
 use X_m,            ONLY:global_gauge
 use fields,         ONLY:n_ext_fields,Efield
 use IO_int,         ONLY:io_variable_elemental,def_variable_elemental
 !
 implicit none
 !
 integer            :: ID
 !
 integer, external  ::io_descriptors
 !
 integer               :: i1,MENU_
 logical               :: l_NE_with_fields_disk,l_RT_diamagnetic
 real(SP)              :: RT_life_extrap_decay_time
 !
 l_RT_diamagnetic          = .false.
 RT_life_extrap_decay_time = 0._SP
 !
 call IO_desc_reset(RT_desc)
 !
 ! RT_vars
 !
 call def_variable_elemental(ID,VAR="RT_bands_kpts",VAR_SZ=3,VAR_KIND=IP_YIO,MENU=0) 
 call io_variable_elemental(ID,I1=RT_bands,VAR=' [RT] Bands',CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=RT_desc)
 if (read_is_on(ID)) H_ref_bands=RT_bands
 call io_variable_elemental(ID,I0=RT_nk,VAR=' [RT] K-points',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 call def_variable_elemental(ID,VAR="POTENTIAL",VAR_SZ=1,VAR_KIND=CR_YIO,MENU=0)
 call io_variable_elemental(ID,CH0=RT_potential,VAR=' [RT] Potential',WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 !
 ! ELPH_vars
 !
#if defined _ELPH
 call def_variable_elemental(ID,VAR="ELPH_vars",VAR_SZ=5,VAR_KIND=IP_YIO,MENU=0) 
 call io_variable_elemental(ID,L0=l_elph_scatt,&
&   VAR=" [E-P] Electron-phonon", CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 call io_variable_elemental(ID,L0=l_RT_iterative_ELPH_lifetimes,&
&   VAR=" [E-P] Iterative time evolution   ", CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 call io_variable_elemental(ID, &
&   VAR=" [E-P] Branches",I1=elph_branches,CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=RT_desc)
 call io_variable_elemental(ID, &
&   VAR=" [E-P] GF`s Memory steps",I0=G_MEM_steps,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
#endif
 !
 ! E-E_vars
 !
 call def_variable_elemental(ID,VAR="E-E_vars",VAR_SZ=2,VAR_KIND=LP_YIO,MENU=0) 
 call io_variable_elemental(ID,L0=l_elel_scatt,VAR=" [E-E] Coulomb scattering", CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 call io_variable_elemental(ID,L0=l_elel_corr,VAR=" [E-E] Coulomb correlation", CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 !
 ! E-PHOTON_vars
 !
 call def_variable_elemental(ID,VAR="E-PHOTON_vars",VAR_SZ=1,VAR_KIND=LP_YIO,MENU=0) 
 call io_variable_elemental(ID,L0=l_elphoton_scatt,VAR=" [E-PHOTON] Scattering", CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 !
 ! FineGrid descriptors
 ! 
 variables_RT_static=io_descriptors(FineGd_desc,"FineGd",ID,.FALSE.)
 !
 if(any((/RT_do_IO("OBSERVABLES"),RT_do_IO("ANY_G"),RT_do_IO("carriers")/))) then
   !
   ! RT_DYNAMICS
   !
   call def_variable_elemental(ID,VAR="RT_TIMEs_INP",VAR_SZ=5,VAR_KIND=SP,MENU=0) 
   !
   call io_variable_elemental(ID,VAR=" [RT] Time step     (initial)",R0=RT_step,CHECK=.true.,OP=(/"=="/),UNIT=AUT2FS,&
&    DESCRIPTOR=RT_desc,TERMINATOR="[fs]")
   call io_variable_elemental(ID,VAR=" [RT] NE time       (planned)",R0=NE_tot_time,WARN=.true.,OP=(/"=="/),UNIT=AUT2FS,&
&    DESCRIPTOR=RT_desc,TERMINATOR="[fs]")
   call io_variable_elemental(ID,VAR=" [RT] NE steps      (planned)",I0=NE_steps,WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   !
   call io_variable_elemental(ID,VAR=" [RT] Lif Extr Inte/Intr(sav)",R1=RT_life_extrap_times_INPUT,CHECK=.true.,OP=(/"==","=="/),&
&    DESCRIPTOR=RT_desc,UNIT=AUT2FS,TERMINATOR="[fs]")
   !
   ! Lifetimes Extrapolation
   !
   call def_variable_elemental(ID,VAR='RT_life_extrapolation',VAR_SZ=1,VAR_KIND=LP_YIO,MENU=0)
   call io_variable_elemental(ID,L0=l_RT_lifetimes_extrapolation,&
&                             VAR=" [RT] Lifetimes extrapolation",WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   !
   ! INTEGRATOR
   !   
   call def_variable_elemental(ID,VAR="INTEGRATOR",VAR_SZ=1,VAR_KIND=CR_YIO,MENU=0)
   call io_variable_elemental(ID,CH0=Integrator_name, VAR=' [RT] Integrator',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   !
   ! RT_OTHERS
   !
   call def_variable_elemental(ID,VAR="RT_OTHERS",VAR_SZ=6,VAR_KIND=SP,MENU=0) 
   !
   call io_variable_elemental(ID,R0=RAD_LifeTime,&
&    VAR=' [RT] Radiative life time', CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS,TERMINATOR="[fs]")
   call io_variable_elemental(ID,R0=Phase_LifeTime,&
&    VAR=' [RT] Dephasing life time', WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS,TERMINATOR="[fs]")
   call io_variable_elemental(ID,L0=l_NE_with_fields,DB_L0=l_NE_with_fields_disk,&
&    VAR=' [RT] External Field(s)', WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,I0=n_ext_fields,VAR=' [RT] Number of Fields', WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,L0=l_RT_diamagnetic,VAR=' [RT] Diamagnetic term', WARN=.false.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,L0=l_RT_induced_field,VAR=' [RT] Induced field', WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   !
   ! G retarded variables
   !======================
   variables_RT_static=io_descriptors(Gr_desc,"Gr",ID,.FALSE.)
   !
   ! Double Grid
   !======================
   call def_variable_elemental(ID,VAR="Double_Grid_support",VAR_SZ=2,VAR_KIND=SP,MENU=0) 
   call io_variable_elemental(ID,L0=l_RT_uses_E_FineGd,VAR=' [DbGd] Double Grid support',&
&                             CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,R0=DbGd_EE_percent,&
&                             VAR=' [DbGd] Used for e-e',  CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,TERMINATOR="[o/o]")
   !
   ! CONSTRAINS
   !======================
   call def_variable_elemental(ID,VAR="CONSTRAINS",VAR_SZ=2,VAR_KIND=LP_YIO,MENU=0) 
   call io_variable_elemental(ID,L0=l_RT_impose_N_dN,&
&      VAR=' [CONSTRAINS] N (ee+ep) & dN (ep)',  CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,L0=l_RT_impose_E,&
&      VAR=' [CONSTRAINS] IP Energy      (ee)',  CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   !
   ! FIELDS
   !======================
   if (l_NE_with_fields_disk) then
     !
     call def_variable_elemental(ID,VAR="GAUGE_TYPE",VAR_SZ=1,VAR_KIND=CR_YIO,MENU=0)
     call io_variable_elemental(ID,CH0=global_gauge, VAR=' [FIELDs] Gauge Type',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
     !
     do i1=1,n_ext_fields
       if( read_is_on(ID) .and. .not.variable_exist(ID,"FIELD_NAME"//trim(intc(i1))) ) cycle
       call def_variable_elemental(ID,VAR="FIELD_NAME"//trim(intc(i1)),VAR_SZ=1,VAR_KIND=0,MENU=0)
       call io_variable_elemental(ID,CH0=Efield(i1)%ef_name,VAR=' [FIELDs] Type',DESCRIPTOR=RT_desc)
       !
       call def_variable_elemental(ID,VAR="FIELD_POL"//trim(intc(i1)),VAR_SZ=1,VAR_KIND=0,MENU=0)
       call io_variable_elemental(ID,CH0=Efield(i1)%ef_pol,VAR=' [FIELDs] Polarization',DESCRIPTOR=RT_desc)
       !
       call def_variable_elemental(ID,VAR="EXTERNAL_FIELD"//trim(intc(i1)),VAR_SZ=13,VAR_KIND=SP,MENU=0)
       call io_variable_elemental(ID,R1=Efield(i1)%versor,VAR=" [FIELDs] Versor",DESCRIPTOR=RT_desc)
       call io_variable_elemental(ID,R1=Efield(i1)%versor_circ,VAR=" [FIELDs] Versor_circ",DESCRIPTOR=RT_desc)
       call io_variable_elemental(ID,VAR=" [FIELDs] Intensity",R0=Efield(i1)%intensity,&
&                                 DESCRIPTOR=RT_desc,UNIT=AU2KWCMm2,TERMINATOR="[kWCMm2]")
       call io_variable_elemental(ID,VAR=" [FIELDs] Damping",R0=Efield(i1)%width,DESCRIPTOR=RT_desc,UNIT=AUT2FS,TERMINATOR="[fs]")
       call io_variable_elemental(ID,VAR=" [FIELDs] Frequency range",R1=Efield(i1)%frequency,&
&                                 DESCRIPTOR=RT_desc,UNIT=HA2EV,TERMINATOR="[eV]")
       call io_variable_elemental(ID,VAR=" [FIELDs] Frequency steps",I0=Efield(i1)%n_frequencies,DESCRIPTOR=RT_desc)
       call io_variable_elemental(ID,VAR=" [FIELDs] Frequency step",R0=Efield(i1)%W_step,&
&                                 CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=HA2EV,TERMINATOR="[eV]")
       call io_variable_elemental(ID,R0=Efield(i1)%t_initial,VAR=' [FIELDs] Initial time',&
&                                 DESCRIPTOR=RT_desc,UNIT=AUT2FS,TERMINATOR="[fs]")
       !                       
     enddo
     !
   endif
   MENU_=1
   if (any((/io_sec(ID,:)==2/))) MENU_=0
   call def_variable_elemental(ID,VAR="",VAR_SZ=0,VAR_KIND=0,MENU=MENU_)
 endif
 !
 n_RT_desc_static=RT_desc%n
 !
 variables_RT_static     =io_status(ID)
 !
end function variables_RT_static
#endif

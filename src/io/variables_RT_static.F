!
!        Copyright (C) 2000-2020 the YAMBO team
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
integer function variables_RT_static(ID,what)
 !
 use pars,           ONLY:SP,LP,IP
 use units,          ONLY:AUT2FS,AU2KWCMm2,HA2EV
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elel_corr,l_elphoton_scatt
 use stderr,         ONLY:intc
 use hamiltonian,    ONLY:H_ref_bands
 use descriptors,    ONLY:IO_desc_reset
 use RT_output_m,    ONLY:RT_desc,Gr_desc,n_RT_desc_static
 use real_time,      ONLY:RT_nk,l_NE_with_fields,RT_step, &
&                         Integrator_name,RT_bands,l_RT_lifetimes_extrapolation, &
&                         RAD_LifeTime,Phase_LifeTime,&
&                         l_RT_induced_field,G_MEM_steps,NE_tot_time,NE_steps,&
&                         l_RT_CCA_Kernel,l_RT_impose_N_dN, &
&                         l_RT_uses_E_FineGd,RT_life_extrap_times_INPUT,&
&                         DbGd_EE_percent,l_RT_impose_E,&
&                         RT_potential
 use IO_m,           ONLY:io_sec
#if defined _ELPH
 use ELPH,           ONLY:elph_branches
#endif
 !
 ! GPL_EXCLUDE_END
 !
 use X_m,            ONLY:global_gauge
 use fields,         ONLY:n_ext_fields,Efield
 use IO_m,           ONLY:io_variable_elemental,def_variable_elemental,io_status,read_is_on
 !
 implicit none
 !
 integer            :: ID
 character(*)       :: what
 !
 integer, external :: io_descriptors
 !
 integer               :: i1,MENU_
 logical               :: l_OBS,l_G,l_carriers,l_NE_with_fields_disk,l_RT_diamagnetic
 real(SP)              :: RT_life_extrap_decay_time
 !
 l_RT_diamagnetic          = .false.
 RT_life_extrap_decay_time = 0._SP
 !
 l_OBS         = what=="OBSERVABLES"
 l_G           = what=="G"
 l_carriers    = what=="carriers"
 !
 call IO_desc_reset(RT_desc)
 !
 ! RT_vars
 !
 call def_variable_elemental(ID,VAR="RT_bands_kpts",VAR_SZ=3,VAR_KIND=IP+100,MENU=0) 
 call io_variable_elemental(ID,I1=RT_bands,&
&     VAR=' [RT] Bands                      ',CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=RT_desc)
 if (read_is_on(ID)) H_ref_bands=RT_bands
 call io_variable_elemental(ID,I0=RT_nk,&
&     VAR=' [RT] K-points                   ',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 call def_variable_elemental(ID,VAR="POTENTIAL",VAR_SZ=1,VAR_KIND=0,MENU=0)
 call io_variable_elemental(ID,CH0=RT_potential,&
&       VAR=' [RT] Potential                  ',WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 !
 ! GPL_EXCLUDE_START
 !
 ! ELPH_vars
 !
#if defined _ELPH
 call def_variable_elemental(ID,VAR="ELPH_vars",VAR_SZ=5,VAR_KIND=IP+100,MENU=0) 
 !
 call io_variable_elemental(ID,L0=l_elph_scatt,&
&   VAR=" [E-P] Electron-phonon            ", CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 call io_variable_elemental(ID,L0=l_RT_CCA_Kernel,&
&   VAR=" [E-P] Completed Collision(CCA)   ", CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 call io_variable_elemental(ID, &
&   VAR=" [E-P] Branches                   ",I1=elph_branches,CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=RT_desc)
 call io_variable_elemental(ID, &
&   VAR=" [E-P] GF`s Memory steps          ",I0=G_MEM_steps,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
#endif
 !
 ! E-E_vars
 !
 call def_variable_elemental(ID,VAR="E-E_vars",VAR_SZ=2,VAR_KIND=LP+100,MENU=0) 
 call io_variable_elemental(ID,L0=l_elel_scatt,&
&   VAR=" [E-E] Coulomb scattering         ", CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 call io_variable_elemental(ID,L0=l_elel_corr,&
&  VAR=" [E-E] Coulomb correlation        ", CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 !
 ! E-PHOTON_vars
 !
 call def_variable_elemental(ID,VAR="E-PHOTON_vars",VAR_SZ=1,VAR_KIND=LP+100,MENU=0) 
 call io_variable_elemental(ID,L0=l_elphoton_scatt,&
&     VAR=" [E-PHOTON] Scattering           ", CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 !
 ! GPL_EXCLUDE_END
 !
 if(any((/l_OBS,l_G,l_carriers/))) then
   !
   ! RT_DYNAMICS
   !
   call def_variable_elemental(ID,VAR="RT_TIMEs_INP",VAR_SZ=5,VAR_KIND=SP,MENU=0) 
   !
   call io_variable_elemental(ID,&
&    VAR=" [RT] Time step     (initial)[fs] ",R0=RT_step,CHECK=.true.,OP=(/"=="/),UNIT=AUT2FS,DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,&
&    VAR=" [RT] NE time       (planned)[fs]",R0=NE_tot_time,WARN=.true.,OP=(/"=="/),UNIT=AUT2FS,DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,&
&    VAR=" [RT] NE steps      (planned)[fs]",I0=NE_steps,WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   !
   ! GPL_EXCLUDE_START
   !
   call io_variable_elemental(ID,&
&    VAR=" [RT] Lif Extr Inte/Intr(sav)[fs]",R1=RT_life_extrap_times_INPUT,CHECK=.true.,OP=(/"==","=="/),&
&    DESCRIPTOR=RT_desc,UNIT=AUT2FS)
   !
   ! Lifetimes Extrapolation
   !
   call def_variable_elemental(ID,VAR='RT_life_extrapolation',VAR_SZ=1,VAR_KIND=LP+100,MENU=0)
   call io_variable_elemental(ID,L0=l_RT_lifetimes_extrapolation, &
&        VAR=" [RT] Lifetimes extrapolation     ",WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   !
   ! GPL_EXCLUDE_END
   !
   ! INTEGRATOR
   !   
   call def_variable_elemental(ID,VAR="INTEGRATOR",VAR_SZ=1,VAR_KIND=0,MENU=0)
   call io_variable_elemental(ID,CH0=Integrator_name, &
&    VAR=' [RT] Integrator                 ',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   !
   ! RT_OTHERS
   !
   call def_variable_elemental(ID,VAR="RT_OTHERS",VAR_SZ=6,VAR_KIND=SP,MENU=0) 
   !
   call io_variable_elemental(ID,R0=RAD_LifeTime,&
&    VAR=' [RT] Radiative life time    [fs]', CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS)
   call io_variable_elemental(ID,R0=Phase_LifeTime,&
&    VAR=' [RT] Dephasing life time    [fs]', CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS)
   call io_variable_elemental(ID,L0=l_NE_with_fields,DB_L0=l_NE_with_fields_disk,&
&    VAR=' [RT] External Field(s)          ', CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,I0=n_ext_fields,&
&    VAR=' [RT] Number of Fields           ', CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,L0=l_RT_diamagnetic,&
&    VAR=' [RT] Diamagnetic term           ',  CHECK=.false.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,L0=l_RT_induced_field,&
&    VAR=' [RT] Induced field              ',  CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   !
   ! GPL_EXCLUDE_START
   !
   ! G retarded variables
   !======================
   variables_RT_static=io_descriptors(Gr_desc,"Gr",ID,.FALSE.)
   !
   ! GPL_EXCLUDE_END
   !
   ! Double Grid
   !======================
   call def_variable_elemental(ID,VAR="Double_Grid_support",VAR_SZ=2,VAR_KIND=SP,MENU=0) 
   call io_variable_elemental(ID,L0=l_RT_uses_E_FineGd,&
&    VAR=' [DbGd] Double Grid support      ',  CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,R0=DbGd_EE_percent,&
&    VAR=' [DbGd] Used for e-e        [o/o]',  CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   !
   ! GPL_EXCLUDE_START
   !
   ! CONSTRAINS
   !======================
   call def_variable_elemental(ID,VAR="CONSTRAINS",VAR_SZ=2,VAR_KIND=LP+100,MENU=0) 
   call io_variable_elemental(ID,L0=l_RT_impose_N_dN,&
&      VAR=' [CONSTRAINS] N (ee+ep) & dN (ep)',  CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,L0=l_RT_impose_E,&
&      VAR=' [CONSTRAINS] IP Energy      (ee)',  CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   !
   ! GPL_EXCLUDE_END
   !
   ! FIELDS
   !======================
   if (l_NE_with_fields_disk) then
     !
     call def_variable_elemental(ID,VAR="GAUGE_TYPE",VAR_SZ=1,VAR_KIND=0,MENU=0)
     call io_variable_elemental(ID,CH0=global_gauge, &
&       VAR=' [FIELDs] Gauge Type             ',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
     !
     do i1=1,n_ext_fields
       call def_variable_elemental(ID,VAR="FIELD_NAME"//trim(intc(i1)),VAR_SZ=1,VAR_KIND=0,MENU=0)
       call io_variable_elemental(ID,CH0=Efield(i1)%ef_name, &
&        VAR=' [FIELDs] Type                   ',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
       !
       call def_variable_elemental(ID,VAR="FIELD_POL"//trim(intc(i1)),VAR_SZ=1,VAR_KIND=0,MENU=0)
       call io_variable_elemental(ID,CH0=Efield(i1)%ef_pol, &
&        VAR=' [FIELDs] Polarization           ',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
       !
       call def_variable_elemental(ID,VAR="EXTERNAL_FIELD"//trim(intc(i1)),VAR_SZ=13,VAR_KIND=SP,MENU=0)
       call io_variable_elemental(ID,R1=Efield(i1)%versor,WARN=.true., &
&        VAR=" [FIELDs] Versor                  ",OP=(/"==","==","=="/),DESCRIPTOR=RT_desc)
       call io_variable_elemental(ID,R1=Efield(i1)%versor_circ,WARN=.true., &
&        VAR=" [FIELDs] Versor_circ             ",OP=(/"==","==","=="/),DESCRIPTOR=RT_desc)
       call io_variable_elemental(ID,&
&        VAR=" [FIELDs] Intensity      [kWCMm2] ",R0=Efield(i1)%intensity,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc,UNIT=AU2KWCMm2)
       call io_variable_elemental(ID,&
&        VAR=" [FIELDs] Damping            [fs] ",R0=Efield(i1)%width,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc,UNIT=AUT2FS)
       call io_variable_elemental(ID,&
&        VAR=" [FIELDs] Frequency range    [eV] ",R1=Efield(i1)%frequency,CHECK=.true.,OP=(/"==","=="/),&
&        DESCRIPTOR=RT_desc,UNIT=HA2EV)
       call io_variable_elemental(ID,&
&        VAR=" [FIELDs] Frequency steps         ",I0=Efield(i1)%n_frequencies,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc)
       call io_variable_elemental(ID,&
&        VAR=" [FIELDs] Frequency step     [eV] ",R0=Efield(i1)%W_step,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc,UNIT=HA2EV)
       call io_variable_elemental(ID,R0=Efield(i1)%t_initial,&
&        VAR=' [FIELDs] Initial time       [fs]',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS)
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

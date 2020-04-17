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
integer function variables_RT(ID,what)
 !
 use pars,           ONLY:lchlen,SP
 use units,          ONLY:AUT2FS
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elel_corr,l_elphoton_scatt
 use stderr,         ONLY:intc
 use hamiltonian,    ONLY:H_ref_bands
 use RT_output_m,    ONLY:RT_desc,Gr_desc,TIME_STEP_desc
 use descriptors,    ONLY:IO_desc_reset
 use RT_control,     ONLY:OBS_RT_IO_t,GF_RT_IO_t,RT_IO_t
 use IO_m,           ONLY:variable_exist,write_is_on
 use real_time,      ONLY:RT_nk,l_NE_with_fields,RT_step,RT_dyn_step, &
&                         Integrator_name,RT_bands,l_RT_lifetimes_extrapolation, &
&                         RAD_LifeTime,Phase_LifeTime,NE_initial_time_step_update,NE_time_step_update_jump,&
&                         l_RT_induced_field,G_MEM_steps,NE_tot_time,NE_time,NE_steps,&
&                         l_RT_CCA_Kernel,l_RT_impose_N_dN,NE_time_step_update_jump_SAVE, &
&                         l_RT_uses_E_FineGd,RT_life_extrap_times_SAVE,&
&                         DbGd_EE_percent,l_RT_impose_E,l_Update_Time_Step,&
&                         NE_step_update_treshold,RT_life_extrap_times,RT_MAX_step,RT_potential
#if defined _ELPH
 use ELPH,           ONLY:elph_branches
#endif
 !
 ! GPL_EXCLUDE_END
 !
 use X_m,            ONLY:global_gauge
 use fields,         ONLY:n_ext_fields,Efield
 use IO_m,           ONLY:io_elemental,io_status,ver_is_gt_or_eq,read_is_on
 !
 implicit none
 !
 integer            ::ID
 character(*)       ::what
 !
 integer               :: i1
 logical               :: l_OBS,l_G,l_carriers,l_NE_with_fields_disk,l_RT_diamagnetic,l_disk_flag
 real(SP)              :: RT_life_extrap_decay_time
 character(lchlen)     :: ch
 integer, external     :: io_descriptors
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
 call io_elemental(ID,VAR="RT_vars",VAR_SZ=3,MENU=0) 
 call io_elemental(ID,I1=RT_bands,&
&     VAR=' [RT] Bands                      ',CHECK=.true.,OP=(/"==","=="/),&
&     DESCRIPTOR=RT_desc)
 if (read_is_on(ID)) H_ref_bands=RT_bands
 call io_elemental(ID,I0=RT_nk,&
&     VAR=' [RT] K-points                   ',CHECK=.true.,OP=(/"==","=="/),&
&     DESCRIPTOR=RT_desc)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 if (variable_exist(ID,"POTENTIAL").or.write_is_on(ID)) then
   call io_elemental(ID,VAR="POTENTIAL",CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=RT_potential,&
&       VAR=' [RT] Potential                  ',WARN=.true.,OP=(/"=="/),&
&       DESCRIPTOR=RT_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 !
 ! GPL_EXCLUDE_START
 !
 ! ELPH_vars
 !
#if defined _ELPH
 call io_elemental(ID,VAR="ELPH_vars",VAR_SZ=5,MENU=0) 
 call io_elemental(ID,L0=l_elph_scatt,&
&   VAR="[E-P] Electron-phonon            ", CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTOR=RT_desc)
 call io_elemental(ID,L0=l_RT_CCA_Kernel,&
&   VAR="[E-P] Completed Collision(CCA)   ", CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTOR=RT_desc)
 call io_elemental(ID, &
&   VAR="[E-P] Branches                   ",I1=elph_branches,CHECK=.true.,OP=(/"==","=="/),&
&   DESCRIPTOR=RT_desc)
 call io_elemental(ID, &
&   VAR="[E-P] GF`s Memory steps          ",I0=G_MEM_steps,CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTOR=RT_desc)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
#endif
 !
 ! E-E_vars
 !
 call io_elemental(ID,VAR="E-E_vars",VAR_SZ=2,MENU=0) 
 call io_elemental(ID,L0=l_elel_scatt,&
&   VAR="[E-E] Coulomb scattering         ", CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTOR=RT_desc)
 call io_elemental(ID,L0=l_elel_corr,&
&  VAR="[E-E] Coulomb correlation        ", CHECK=.true.,OP=(/"=="/),&
&  DESCRIPTOR=RT_desc)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 ! E-PHOTON_vars
 !
 if (ver_is_gt_or_eq(ID,revision=14112)) then
   call io_elemental(ID,VAR="E-PHOTON_vars",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,L0=l_elphoton_scatt,&
&     VAR="[E-PHOTON] Scattering            ", CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTOR=RT_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 !
 ! GPL_EXCLUDE_END
 !
 if(.not.any((/l_OBS,l_G,l_carriers/))) call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 if(any((/l_OBS,l_G,l_carriers/))) then
   !
   ! RT_DYNAMICS
   !
   call io_elemental(ID,VAR="RT_DYNAMICS",VAR_SZ=14,MENU=0) 
   call io_elemental(ID,&
&    VAR="[RT] Time step (initial)",TERMINATOR="fs ",R0=RT_step,CHECK=.true.,OP=(/"=="/),UNIT=AUT2FS,&
&    DESCRIPTOR=RT_desc)
   call io_elemental(ID,&
&    VAR="[RT]           (now)",TERMINATOR="fs",R0=RT_dyn_step,DB_R0=RT_dyn_step,WARN=.true.,OP=(/"=="/),UNIT=AUT2FS,&
&    DESCRIPTOR=RT_desc)
   call io_elemental(ID,&
&    VAR="[RT] NE time (planned)",TERMINATOR="fs",R0=NE_tot_time,WARN=.true.,OP=(/"=="/),UNIT=AUT2FS,&
&    DESCRIPTOR=RT_desc)
   call io_elemental(ID,&
&    VAR="[RT]         (done)",TERMINATOR="fs",R0=NE_time,WARN=.true.,OP=(/"=="/),UNIT=AUT2FS,&
&    DESCRIPTOR=RT_desc)
   !
   ! GPL_EXCLUDE_START
   !
   call io_elemental(ID,&
&    VAR="[RT] Lifetimes Inter/Intra (user)  ",TERMINATOR="fs",R1=RT_life_extrap_times_SAVE,CHECK=.true.,OP=(/"==","==","=="/),&
&    DESCRIPTOR=RT_desc)
   call io_elemental(ID,&
&    VAR="[RT]                       (running)",TERMINATOR="fs ",R1=RT_life_extrap_times,DB_R1=RT_life_extrap_times,&
&    DESCRIPTOR=RT_desc)
   !
   ! GPL_EXCLUDE_END
   !
   if (l_OBS.or.l_carriers) call LOCAL_IO_variables(OBS_RT_IO_t)
   if (l_G)                 call LOCAL_IO_variables(GF_RT_IO_t)
   !
   call io_elemental(ID,&
&    VAR="[RT] NE steps (done)",I0=NE_steps,WARN=.true.,OP=(/"=="/),&
&    DESCRIPTOR=RT_desc)
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! GPL_EXCLUDE_START
   !
   !
   ! Lifetimes Extrapolation
   !
   call io_elemental(ID,VAR='RT_life_extrapolation',VAR_SZ=1,MENU=0)
   call io_elemental(ID,L0=l_RT_lifetimes_extrapolation, &
&        VAR="[RT] Lifetimes extrapolation     ",WARN=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! Delta T Update
   !   
   call io_elemental(ID,VAR="DELTA_T_UPDATE",VAR_SZ=6,MENU=0) 
   call io_elemental(ID,L0=l_Update_Time_Step,DB_L0=l_disk_flag,VAR=' [dT] Time-Dependent Time step   ',CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTOR=RT_desc)
   if (l_disk_flag) then
     call io_elemental(ID,R0=NE_initial_time_step_update,VAR=" [dT] Starting Time",TERMINATOR="fs",CHECK=.true.,OP=(/"=="/),&
&      DESCRIPTOR=RT_desc)
     call io_elemental(ID,R0=NE_time_step_update_jump_SAVE, VAR=" [dT] Update interval",TERMINATOR="fs",CHECK=.true.,OP=(/"=="/),&
&      DESCRIPTOR=RT_desc)
     call io_elemental(ID,R0=NE_step_update_treshold,VAR=' [dT] Update interval tresh',TERMINATOR='o/o',CHECK=.true.,OP=(/"=="/),&
&      DESCRIPTOR=RT_desc)
     if (ver_is_gt_or_eq(ID,revision=14112)) then
       call io_elemental(ID,R0=RT_MAX_step,VAR=' [dT] Max step in dynamics',TERMINATOR='fs',CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc)
     endif
   endif
   ! ... for RESTART purposes...
   call io_elemental(ID,R0=NE_time_step_update_jump, DB_R0=NE_time_step_update_jump,CHECK=.FALSE.)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! GPL_EXCLUDE_END
   !
   ! INTEGRATOR
   !   
   call io_elemental(ID,VAR="INTEGRATOR",CH0="",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,CH0=Integrator_name, &
&    VAR=' [RT] Integrator                 ',CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTOR=RT_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! RT_OTHERS
   !
   call io_elemental(ID,VAR="RT_OTHERS",VAR_SZ=6,MENU=0) 
   call io_elemental(ID,R0=RAD_LifeTime,&
&    VAR=' [RT] Radiative life time ',TERMINATOR='fs', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTOR=RT_desc)
   call io_elemental(ID,R0=Phase_LifeTime,&
&    VAR=' [RT] Dephasing life time',TERMINATOR='fs', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTOR=RT_desc)
   call io_elemental(ID,L0=l_NE_with_fields,DB_L0=l_NE_with_fields_disk,&
&    VAR=' [RT] External Field(s)          ', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTOR=RT_desc)
   call io_elemental(ID,I0=n_ext_fields,&
&    VAR=' [RT] Number of Fields           ', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTOR=RT_desc)
   call io_elemental(ID,L0=l_RT_diamagnetic,&
&    VAR=' [RT] Diamagnetic term           ',  CHECK=.false.,OP=(/"=="/),&
&     DESCRIPTOR=RT_desc)
   call io_elemental(ID,L0=l_RT_induced_field,&
&    VAR=' [RT] Induced field              ',  CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTOR=RT_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! Time-Steps configurations
   !===========================
   variables_RT=io_descriptors(TIME_STEP_desc,"TIME_STEP",ID,.FALSE.)
   !
   ! GPL_EXCLUDE_START
   !
   ! G retarded variables
   !======================
   variables_RT=io_descriptors(Gr_desc,"Gr",ID,.FALSE.)
   !
   ! GPL_EXCLUDE_END
   !
   ! Double Grid
   !======================
   call io_elemental(ID,VAR="Double_Grid_support",VAR_SZ=2,MENU=0) 
   call io_elemental(ID,L0=l_RT_uses_E_FineGd,&
&    VAR=' [DbGd] Double Grid support      ',  CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTOR=RT_desc)
   call io_elemental(ID,R0=DbGd_EE_percent,&
&    VAR=' [DbGd] Used for e-e        ',TERMINATOR='o/o',  CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTOR=RT_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! GPL_EXCLUDE_START
   !
   ! CONSTRAINS
   !======================
   call io_elemental(ID,VAR="CONSTRAINS",VAR_SZ=2,MENU=0) 
   call io_elemental(ID,L0=l_RT_impose_N_dN,&
&      VAR=' [CONSTRAINS] N (ee+ep) & dN (ep)',  CHECK=.true.,OP=(/"=="/),&
&      DESCRIPTOR=RT_desc)
   call io_elemental(ID,L0=l_RT_impose_E,&
&      VAR=' [CONSTRAINS] IP Energy      (ee)',  CHECK=.true.,OP=(/"=="/),&
&      DESCRIPTOR=RT_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! GPL_EXCLUDE_END
   !
   ! FIELDS
   !======================
   if (l_NE_with_fields_disk) then
     !
     call io_elemental(ID,VAR="GAUGE_TYPE",CH0="",VAR_SZ=1,MENU=0)
     call io_elemental(ID,CH0=global_gauge, &
&       VAR=' [FIELDs] Gauge Type             ',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTOR=RT_desc)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     do i1=1,n_ext_fields
       call io_elemental(ID,VAR="FIELD_NAME"//trim(intc(i1)),CH0="",VAR_SZ=1,MENU=0)
       call io_elemental(ID,CH0=Efield(i1)%ef_name, &
&        VAR=' [FIELDs] Type                   ',CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc)
       !
       call io_elemental(ID,VAR="FIELD_POL"//trim(intc(i1)),CH0="",VAR_SZ=1,MENU=0)
       call io_elemental(ID,CH0=Efield(i1)%ef_pol, &
&        VAR=' [FIELDs] Polarization           ',CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc)
       call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
       !
       call io_elemental(ID,VAR="EXTERNAL_FIELD"//trim(intc(i1)),VAR_SZ=13,MENU=0)
       call io_elemental(ID,R1=Efield(i1)%versor,WARN=.true., &
&        VAR="[FIELDs] Versor                  ",OP=(/"==","==","=="/),&
&        DESCRIPTOR=RT_desc)
       call io_elemental(ID,R1=Efield(i1)%versor_circ,WARN=.true., &
&        VAR="[FIELDs] Versor_circ             ",OP=(/"==","==","=="/),&
&        DESCRIPTOR=RT_desc)
       call io_elemental(ID,&
&        VAR="[FIELDs] Intensity      ",TERMINATOR="kWCMm2",R0=Efield(i1)%intensity,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc)
       call io_elemental(ID,&
&        VAR="[FIELDs] Damping            ",TERMINATOR="fs",R0=Efield(i1)%width,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc)
       call io_elemental(ID,&
&        VAR="[FIELDs] Frequency range    ",TERMINATOR="eV",R1=Efield(i1)%frequency,CHECK=.true.,OP=(/"==","=="/),&
&        DESCRIPTOR=RT_desc)
       call io_elemental(ID,&
&        VAR="[FIELDs] Frequency steps         ",I0=Efield(i1)%n_frequencies,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc)
       call io_elemental(ID,&
&        VAR="[FIELDs] Frequency step     ",TERMINATOR="eV",R0=Efield(i1)%W_step,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc)
       call io_elemental(ID,R0=Efield(i1)%t_initial,&
&        VAR=' [FIELDs] Initial time       ',TERMINATOR='fs',CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTOR=RT_desc)
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
 variables_RT=io_status(ID)
 !
 contains
   !
   subroutine LOCAL_IO_variables(TYP)
     !
     type(RT_IO_t) :: TYP
     !
     call io_elemental(ID,&
&      VAR=" [RT] I/O time               ",TERMINATOR="fs",R0=TYP%INTERVAL_time_SAVE,CHECK=.true.,OP=(/"=="/),&
&      DESCRIPTOR=RT_desc)
     ! ... for RESTART purposes...
     call io_elemental(ID,R0=TYP%INTERVAL_time,  DB_r0=TYP%INTERVAL_time,CHECK=.FALSE.)
     call io_elemental(ID,I0=TYP%INTERVAL_steps, DB_I0=TYP%INTERVAL_steps,CHECK=.FALSE.)
     !
     call io_elemental(ID,I0=TYP%last_point,CHECK=.true.,OP=(/"=="/))
     !
   end subroutine
   !
end function

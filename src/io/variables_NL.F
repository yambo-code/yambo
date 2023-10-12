!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG AC
!
integer function variables_NL(ID)
 !
 use pars,           ONLY:SP,IP,LP,IP_YIO,LP_YIO,CR_YIO
 use units,         ONLY:AUT2FS, HA2EV, AU2KWCMm2
 use nl_optics,     ONLY:NL_damping,l_use_DIPOLES,NL_correlation,NL_er,NL_correlation,n_frequencies,      &
&                        WF_sample_ondisk,n_order,NL_desc
 use QP_m,          ONLY:QP_ng_SH,QP_ng_Sx
 use real_time,     ONLY:Integrator_name,NE_steps,RT_step,l_RT_induced_field,RT_bands,RAD_LifeTime
 use electrons,     ONLY:n_sp_pol
 use electric,      ONLY:l_force_SndOrd
 use fields,        ONLY:n_ext_fields
 use fields,        ONLY:Efield
 use IO_m,          ONLY:io_status
 use IO_int,        ONLY:io_elemental,ver_is_gt_or_eq,io_variable_elemental,def_variable_elemental
 use descriptors,   ONLY:IO_desc_reset
 use stderr,        ONLY:intc
 !
 implicit none
 integer,      intent(in) :: ID
 !
 ! Work Space
 !
 integer  :: i_Prb
 !
 call IO_desc_reset(NL_desc)
 !
 ! PARAMETERS
 !
 call def_variable_elemental(ID,"NE_steps",1,IP_YIO,0)
 call io_variable_elemental(ID,VAR="[RT] Time steps",I0=NE_steps,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=NL_desc)

 call def_variable_elemental(ID,"RT_step",1,SP,0)
 call io_variable_elemental(ID,VAR="[RT] Time step",TERMINATOR="[fs]",R0=RT_step,      CHECK=.true.,OP=(/"=="/), &
&         DESCRIPTOR=NL_desc,UNIT=AUT2FS)

 call def_variable_elemental(ID,"NL_damping",1,SP,0)
 call io_variable_elemental(ID,VAR="[NL] Damping",TERMINATOR="[eV]",R0=NL_damping,   CHECK=.true.,OP=(/"=="/), &
&         DESCRIPTOR=NL_desc,UNIT=HA2EV)

 call def_variable_elemental(ID,"RT_bands",2,IP_YIO,0)
 call io_variable_elemental(ID,VAR="[NL] Bands  range",I1=RT_bands,     CHECK=.true.,OP=(/"==","=="/), &
&         DESCRIPTOR=NL_desc)

 call def_variable_elemental(ID,"NL_er",2,SP,0)
 call io_variable_elemental(ID,VAR="[NL] Energy range",R1=NL_er,        CHECK=.true.,OP=(/"==","=="/), &
&         DESCRIPTOR=NL_desc,UNIT=HA2EV)

 call def_variable_elemental(ID,"n_frequencies",1,IP_YIO,0)
 call io_variable_elemental(ID,VAR="[NL] Number of freqs",I0=n_frequencies,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTOR=NL_desc)

 call def_variable_elemental(ID,"l_force_SndOrd",1,LP_YIO,0)
 call io_variable_elemental(ID,VAR="[NL] Force 2nd approx",L0=l_force_SndOrd,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTOR=NL_desc)

 call def_variable_elemental(ID,"l_use_DIPOLES",1,LP_YIO,0)
 call io_variable_elemental(ID,VAR="[NL] Use DIPOLES",L0=l_use_DIPOLES,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTOR=NL_desc)

 call def_variable_elemental(ID,"l_RT_induced_field",1,LP_YIO,0)
 call io_variable_elemental(ID,VAR="[NL] Induced Field",L0=l_RT_induced_field,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTOR=NL_desc)

 call def_variable_elemental(ID,"QP_ng_SH",1,IP_YIO,0)
 call io_variable_elemental(ID,VAR="[NL] Number of G in HARTREE",I0=QP_ng_SH,CHECK=.true.,OP=(/"=="/), &
     &         DESCRIPTOR=NL_desc)

 call def_variable_elemental(ID,"QP_ng_Sx",1,IP_YIO,0)
 call io_variable_elemental(ID,VAR="[NL] Number of G in EXCHANGE(DFT)",I0=QP_ng_Sx,CHECK=.true.,OP=(/"=="/), &
     &         DESCRIPTOR=NL_desc)

 call def_variable_elemental(ID,"RAD_LifeTime",1,SP,0)
 call io_variable_elemental(ID,VAR="[NL] Radiative Life-Time",R0=RAD_LifeTime,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=NL_desc)
 !
 ! WF SAMPLING
 !
! if (ver_is_gt_or_eq(ID,revision=15726)) then !THIS NEEDS TO BE UPDATED IN CASE ENTERS INTO A DISTRO
!   call io_elemental(ID,VAR="WF_SAMPLE",VAR_SZ=2+n_sp_pol,MENU=0)
!   call io_elemental(ID,VAR="[NL] WF sample order   ",I0=n_order,CHECK=.true.,OP=(/"=="/), & 
!        &         DESCRIPTOR=NL_desc)
!   if (n_sp_pol==1) then
!     call io_elemental(ID,VAR="[NL] WF sample valence ",I0=WF_sample_ondisk%n_val(1),CHECK=.true.,OP=(/"=="/), & 
!          &         DESCRIPTOR=NL_desc)
!   else
!     call io_elemental(ID,VAR="[NL] WF sample valence ",I1=WF_sample_ondisk%n_val,CHECK=.true.,OP=(/"==","=="/), & 
!          &         DESCRIPTOR=NL_desc)
!   endif
!   call io_elemental(ID,VAR="[NL] WF sample kpoints ",I0=WF_sample_ondisk%n_k,CHECK=.true.,OP=(/"=="/), & 
!        &         DESCRIPTOR=NL_desc)
!   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
! end if
 !
 ! INTEGRATOR
 !
 call def_variable_elemental(ID,"Integrator",1,CR_YIO,0)
 call io_variable_elemental(ID,CH0=Integrator_name,VAR=' [RT] Integrator           ',CHECK=.true.,OP=(/"=="/), &
&        DESCRIPTOR=NL_desc)
 !
 ! CORRELATION
 !
 call def_variable_elemental(ID,"Correlation",1,CR_YIO,0)
 call io_variable_elemental(ID,CH0=NL_correlation,VAR=' [NL] Correlation           ',CHECK=.true.,OP=(/"=="/), &
&        DESCRIPTOR=NL_desc)
 !
 ! Efield 
 !
 do i_Prb=1,n_ext_fields
   call def_variable_elemental(ID,"Field_Name_"//trim(intc(i_Prb)),1,CR_YIO,0)
   call io_variable_elemental(ID,CH0=Efield(i_Prb)%ef_name,VAR=' [FIELDs] Type           ',CHECK=.true.,OP=(/"=="/), &
&        DESCRIPTOR=NL_desc)

   call def_variable_elemental(ID,"Field_Versor_"//trim(intc(i_Prb)),3,SP,0)
   call io_variable_elemental(ID,VAR="[FIELDs] Versor                 ",R1=Efield(i_Prb)%versor,WARN=.true.,OP=(/"==","==","=="/), &
&         DESCRIPTOR=NL_desc)
   call def_variable_elemental(ID,"Field_Intensity_"//trim(intc(i_Prb)),1,SP,0)
   call io_variable_elemental(ID,VAR="[FIELDs] Intensity     ",TERMINATOR="[kWCMm2]",&
&         R0=Efield(i_Prb)%intensity,CHECK=.true.,OP=(/"=="/),UNIT=AU2KWCMm2, &
&         DESCRIPTOR=NL_desc)

   call def_variable_elemental(ID,"Field_Damping_"//trim(intc(i_Prb)),1,SP,0)
   call io_variable_elemental(ID,VAR="[FIELDs] Damping           ",TERMINATOR="[fs]",&
&         R0=Efield(i_Prb)%FWHM,CHECK=.true.,OP=(/"=="/),& 
&         DESCRIPTOR=NL_desc)

   call def_variable_elemental(ID,"Field_Freq_range_"//trim(intc(i_Prb)),2,SP,0)
   call io_variable_elemental(ID,VAR="[FIELDs] Frequency range   ",TERMINATOR="[eV]",&
&         R1=Efield(i_Prb)%frequency,CHECK=.true.,OP=(/"==","=="/),UNIT=HA2EV,& 
&         DESCRIPTOR=NL_desc)

   call def_variable_elemental(ID,"Field_Freq_steps_"//trim(intc(i_Prb)),1,IP_YIO,0)
   call io_variable_elemental(ID,VAR="[FIELDs] Frequency steps",I0=Efield(i_Prb)%n_frequencies,CHECK=.true.,OP=(/"=="/),& 
&         DESCRIPTOR=NL_desc)

   call def_variable_elemental(ID,"Field_Freq_step_"//trim(intc(i_Prb)),1,SP,0)
   call io_variable_elemental(ID,VAR="[FIELDs] Frequency step    ",TERMINATOR="[eV]",&
&         R0=Efield(i_Prb)%W_step,CHECK=.true.,OP=(/"=="/),UNIT=HA2EV,& 
&         DESCRIPTOR=NL_desc)

   call def_variable_elemental(ID,"Field_Initial_time_"//trim(intc(i_Prb)),1,SP,0)
   call io_variable_elemental(ID,VAR=" [FIELDs] Initial time      ",TERMINATOR="[fs]",&
&         R0=Efield(i_Prb)%t_initial,CHECK=.true.,OP=(/"=="/),UNIT=AUT2FS,&
&         DESCRIPTOR=NL_desc)

 enddo
 !
 variables_NL=io_status(ID)
 !
end function

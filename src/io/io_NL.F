!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!
integer function io_NL(what,ID)
 !
 use pars,          ONLY:schlen,SP,IP,LP,IP_YIO,LP_YIO,CR_YIO
 use units,         ONLY:HA2EV, AU2KWCMm2, AUT2FS
 use nl_optics,     ONLY:E_tot_t,E_ext_t,E_ks_t,NL_P_t, &
&                        n_frequencies,NL_J_t
 use real_time,     ONLY:NE_steps
 use fields,        ONLY:n_ext_fields
 use RT_control,    ONLY:OBS_RT_IO_t
 use fields,        ONLY:Efield
 use IO_m,          ONLY:io_sec,read_is_on,write_is_on,io_extension
 use IO_int,        ONLY:io_connect,io_disconnect,io_bulk,io_header,def_variable_elemental,&
&                        io_fragment,io_elemental,io_variable_elemental
 use stderr,        ONLY:intc
 !
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
#include<memory.h>
 !
 integer,      intent(in) :: ID
 character(*), intent(in) :: what 
 !
 integer, external    :: variables_NL
 !
 ! Work Space
 !
 integer              :: i_fragment,ID_frag
 character(schlen)    :: VAR_name
 integer              :: i_Prb
 !
#if defined _TIMING
 call timing('io_NL',OPR='start')
#endif
 !
 io_extension(ID)=trim(what)
 !
 ID_frag=0
 !
 io_NL=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID,FORCE_READ_MODE=(.not.any((/io_sec(ID,:)==1/))) )
 if (io_NL/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_NL=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,GAUGE=.true.)
   if (io_NL/=0) goto 1
   !
   io_NL=variables_NL(ID)
   !
   if (io_NL/=0) goto 1
   !
   ! Time variable (just for compatibility reasons)
   !
   if(write_is_on(ID)) then
     OBS_RT_IO_t%N         =NE_steps
     OBS_RT_IO_t%last_point=NE_steps
   endif
   !
   call io_elemental(ID,VAR="IO_TIME_N_points",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,I0=OBS_RT_IO_t%N)
   call io_elemental(ID,VAR="IO_TIME_LAST_POINT",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,I0=OBS_RT_IO_t%last_point)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   if(read_is_on(ID).and..not.allocated(OBS_RT_IO_t%Time)) allocate(OBS_RT_IO_t%Time(NE_steps))
   call io_bulk(ID,VAR="IO_TIME_points",VAR_SZ=(/OBS_RT_IO_t%N/))
   call io_bulk(ID,R1=OBS_RT_IO_t%Time)
   !
   if (io_NL/=0) goto 1
   !
 endif
 !
 i_fragment=maxval(io_sec(ID,:))-1
 !
 if (i_fragment==0) goto 1
 !
 if(read_is_on(ID).and..not.allocated(NL_P_t)) then
   !
   YAMBO_ALLOC(NL_P_t,(NE_steps,3))
   !
   YAMBO_ALLOC(E_tot_t,(NE_steps,3))
   YAMBO_ALLOC(E_ext_t,(NE_steps,3))
   YAMBO_ALLOC(E_ks_t,(NE_steps,3))
   !
 endif
 !
 ! Fragmentation
 !
 call io_fragment(ID,ID_frag,i_fragment=i_fragment,ierr=io_NL)
 if(io_NL/=0) goto 1
 !
 if(what=="Nonlinear_REF") goto 2
 !
 ! Efields are stored in each fragment with the different parameters 
 !
 do i_Prb=1,n_ext_fields
   call def_variable_elemental(ID_frag,"Field_Name_"//trim(intc(i_Prb)),1,CR_YIO,0)
   call io_variable_elemental(ID_frag,CH0=Efield(i_Prb)%ef_name,VAR=' [FIELDs] Type           ',CHECK=.true.,OP=(/"=="/))

   call def_variable_elemental(ID_frag,"Field_Versor_"//trim(intc(i_Prb)),3,SP,0)
   call io_variable_elemental(ID_frag,VAR="[FIELDs] Versor                 ",R1=Efield(i_Prb)%versor, &
&           WARN=.true.,OP=(/"==","==","=="/))
   call def_variable_elemental(ID_frag,"Field_Intensity_"//trim(intc(i_Prb)),1,SP,0)
   call io_variable_elemental(ID_frag,VAR="[FIELDs] Intensity     ",TERMINATOR="[kWCMm2]",&
&         R0=Efield(i_Prb)%intensity,CHECK=.true.,OP=(/"=="/),UNIT=AU2KWCMm2)

   call def_variable_elemental(ID_frag,"Field_Damping_"//trim(intc(i_Prb)),1,SP,0)
   call io_variable_elemental(ID_frag,VAR="[FIELDs] Damping           ",TERMINATOR="[fs]",&
&         R0=Efield(i_Prb)%FWHM,CHECK=.true.,OP=(/"=="/))

   call def_variable_elemental(ID_frag,"Field_Freq_range_"//trim(intc(i_Prb)),2,SP,0)
   call io_variable_elemental(ID_frag,VAR="[FIELDs] Frequency range   ",TERMINATOR="[eV]",&
&         R1=Efield(i_Prb)%frequency,CHECK=.true.,OP=(/"==","=="/),UNIT=HA2EV)

   call def_variable_elemental(ID_frag,"Field_Freq_steps_"//trim(intc(i_Prb)),1,IP_YIO,0)
   call io_variable_elemental(ID_frag,VAR="[FIELDs] Frequency steps", &
&   I0=Efield(i_Prb)%n_frequencies,CHECK=.true.,OP=(/"=="/))

   call def_variable_elemental(ID_frag,"Field_Freq_step_"//trim(intc(i_Prb)),1,SP,0)
   call io_variable_elemental(ID_frag,VAR="[FIELDs] Frequency step    ",TERMINATOR="[eV]",&
&         R0=Efield(i_Prb)%W_step,CHECK=.true.,OP=(/"=="/),UNIT=HA2EV)

   call def_variable_elemental(ID_frag,"Field_Initial_time_"//trim(intc(i_Prb)),1,SP,0)
   call io_variable_elemental(ID_frag,VAR=" [FIELDs] Initial time      ",TERMINATOR="[fs]",&
&         R0=Efield(i_Prb)%t_initial,CHECK=.true.,OP=(/"=="/),UNIT=AUT2FS)

 enddo
 !
 ! Polarization
 ! 
2 write (VAR_name,'(a,i4.4)') 'NL_P_freq_',i_fragment
 call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=shape(NL_P_t))
 call io_bulk(ID_frag,R2=NL_P_t)
 !
 if(what=="Nonlinear_REF") goto 1
 !
 ! Current
 !
 write (VAR_name,'(a,i4.4)') 'NL_J_freq_',i_fragment
 call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=shape(NL_J_t))
 call io_bulk(ID_frag,R2=NL_J_t)
 !
 ! E_ext
 ! 
 write (VAR_name,'(a,i4.4)') 'E_ext_freq_',i_fragment
 call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=(/2,NE_steps,3/))
 call io_bulk(ID_frag,C2=E_ext_t)
 !
 ! E_tot
 ! 
 write (VAR_name,'(a,i4.4)') 'E_tot_freq_',i_fragment
 call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=(/2,NE_steps,3/))
 call io_bulk(ID_frag,C2=E_tot_t)
 !
 ! E_ks
 ! 
 write (VAR_name,'(a,i4.4)') 'E_ks_freq_',i_fragment
 call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=(/2,NE_steps,3/))
 call io_bulk(ID_frag,C2=E_ks_t)
 !
1 call io_fragment_disconnect(ID,ID_frag)
 !
 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_NL',OPR='stop')
#endif
 !
end function

!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA
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
integer function io_NL(ID)
 !
 use pars,          ONLY:SP,schlen,lchlen
 use units,         ONLY:HA2EV, AU2KWCMm2, AUT2FS
 use nl_optics,     ONLY:E_tot_t,E_ext_t,E_ks_t,NL_P_t,NL_damping,l_use_DIPOLES, &
&                        NL_correlation,NL_er,NL_correlation,n_frequencies,      & 
&                        WF_sample,WF_sample_ondisk,V_bands_t,NL_INIT_WF_sample, &
&                        NL_COPY_WF_sample,NL_set_WF_sample,NL_ALLOC_WF_sample,n_order
 use real_time,     ONLY:Integrator_name,NE_steps,RT_step,RT_bands
 use RT_control,    ONLY:JPC_RT_IO_t
 use fields,        ONLY:Efield,EtoT
 use electrons,     ONLY:levels,n_sp_pol
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,&
&                        io_bulk,read_is_on,write_is_on,io_header,io_extension,&
&                        RD_CL_IF_END,RD_CL,io_action,io_fragment,io_elemental,io_status
 !
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
#include<memory.h>
 !
 integer,      intent(in) :: ID
 !
 integer, external    :: variables_NL
 !
 ! Work Space
 !
 integer              :: i_fragment
 character(lchlen)    :: local_description(100)
 character(schlen)    :: VAR_name
 integer, parameter   :: i_Prb=1
 real(SP)             :: WF_sample_range(2) 
 !
#if defined _TIMING
 call timing('io_NL',OPR='start')
#endif
 !
 io_extension(ID)='Nonlinear'
 !
 io_NL=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID)
 if (io_NL/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_NL=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,GAUGE=.true.)
   if (io_NL/=0) goto 1
   !
   call NL_INIT_WF_sample(WF_sample_ondisk)
   if(write_is_on(ID)) call NL_COPY_WF_sample(WF_sample,WF_sample_ondisk)
   io_NL=variables_NL(ID,local_description)
   !
   if (io_NL/=0) goto 1
   !
   ! Time variable (just for compatibility reasons)
   !
   if(write_is_on(ID)) then
      JPC_RT_IO_t%N         =NE_steps    
      JPC_RT_IO_t%last_point=NE_steps    ! 
   endif
   !
   call io_elemental(ID,VAR="IO_TIME_N_points",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,I0=JPC_RT_IO_t%N)
   call io_elemental(ID,VAR="IO_TIME_LAST_POINT",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,I0=JPC_RT_IO_t%last_point)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   if(read_is_on(ID).and..not.allocated(JPC_RT_IO_t%Time)) allocate(JPC_RT_IO_t%Time(NE_steps))
   call io_bulk(ID,VAR="IO_TIME_points",VAR_SZ=(/JPC_RT_IO_t%N/))
   call io_bulk(ID,R1=JPC_RT_IO_t%Time)
   !
   if (io_NL/=0) goto 1
   !
 endif
 !
 i_fragment=maxval(io_sec(ID,:))-1
 !
 if (i_fragment==0) goto 1
 !
 if(read_is_on(ID)) then
   !  
   YAMBO_ALLOC(NL_P_t,(NE_steps,3))
   !
   YAMBO_ALLOC(E_tot_t,(NE_steps,3))
   YAMBO_ALLOC(E_ext_t,(NE_steps,3))
   YAMBO_ALLOC(E_ks_t,(NE_steps,3))
   !
   WF_sample_range(2)= JPC_RT_IO_t%Time(NE_steps)
   WF_sample_range(1)= WF_sample_range(2) - EtoT(E=NL_er(1))+5._SP*RT_step
   call NL_set_WF_sample(WF_sample_ondisk,n_order,RT_step,WF_sample_range)
   call NL_ALLOC_WF_sample(WF_sample_ondisk,RT_bands(2),n_sp_pol) 
   !
 endif
 !
 ! Fragmentation
 !
 call io_fragment(ID,i_fragment=i_fragment)
 !
 ! Manage RD_CL_IF_END
 !
 if (io_action(ID)==RD_CL_IF_END.and.i_fragment==n_frequencies) io_action(ID)=RD_CL
 !
 ! Efield 
 !
 call io_elemental(ID,VAR="FIELD_NAME",CH0="",VAR_SZ=1,MENU=0)
 call io_elemental(ID,CH0=Efield(i_Prb)%ef_name,VAR=' [FIELDs] Type           :',CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 call io_elemental(ID,VAR="EXTERNAL_FIELD",VAR_SZ=10,MENU=0)
 call io_elemental(ID,VAR="[FIELDs] Versor                :",R1=Efield(i_Prb)%versor,WARN=.true.,OP=(/"==","==","=="/))
 call io_elemental(ID,VAR="[FIELDs] Intensity     [kWCMm2]:",R0=Efield(i_Prb)%intensity,CHECK=.true.,OP=(/"=="/),UNIT=AU2KWCMm2)
 call io_elemental(ID,VAR="[FIELDs] Damping           [fs]:",R0=Efield(i_Prb)%FWHM,CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,VAR="[FIELDs] Frequency range   [eV]:",R1=Efield(i_Prb)%frequency,CHECK=.true.,OP=(/"==","=="/),UNIT=HA2EV)
 call io_elemental(ID,VAR="[FIELDs] Frequency steps       :",I0=Efield(i_Prb)%n_frequencies,CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,VAR="[FIELDs] Frequency step    [eV]:",R0=Efield(i_Prb)%W_step,CHECK=.true.,OP=(/"=="/),UNIT=HA2EV)
 call io_elemental(ID,VAR='[FIELDs] Initial time      [fs]:',R0=Efield(i_Prb)%t_initial,CHECK=.true.,OP=(/"=="/),UNIT=AUT2FS)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 call io_elemental(ID,VAR="FIELD_POL",CH0="",VAR_SZ=1,MENU=0)
 call io_elemental(ID,CH0=Efield(i_Prb)%ef_pol,VAR="[FIELDs] Polarization :",CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 ! Polarization
 !  
 write (VAR_name,'(a,i4.4)') 'NL_P_freq_',i_fragment
 call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(NL_P_t))
 call io_bulk(ID,R2=NL_P_t)
 !
 ! E_ext
 ! 
 write (VAR_name,'(a,i4.4)') 'E_ext_freq_',i_fragment
 call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=(/2,NE_steps,3/))
 call io_bulk(ID,C2=E_ext_t)
 !
 ! E_tot
 ! 
 write (VAR_name,'(a,i4.4)') 'E_tot_freq_',i_fragment
 call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=(/2,NE_steps,3/))
 call io_bulk(ID,C2=E_tot_t)
 !
 ! E_ks
 ! 
 write (VAR_name,'(a,i4.4)') 'E_ks_freq_',i_fragment
 call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=(/2,NE_steps,3/))
 call io_bulk(ID,C2=E_ks_t)
 !
 ! WF_sample:  if not switched on skip this variable 
 ! (is that legal or do I need a sec as in io_BSS_diago?)
 !
 if (.not.WF_sample_ondisk%ison) then
   io_NL=-1
   goto 1
 endif
 !
 ! Indexes of the time sampled
 !
 call io_bulk(ID,'INDX_T',VAR_SZ=(/WF_sample_ondisk%n_size/))
 call io_bulk(ID,I1=WF_sample_ondisk%indx_T(:WF_sample_ondisk%n_size))
 !
 ! V_bands at samping times
 !
 call io_bulk(ID,'V_BANDS_T',VAR_SZ=(/WF_sample_ondisk%n_size,RT_bands(2),&
      &WF_sample_ondisk%n_val,WF_sample_ondisk%n_k,n_sp_pol/))
 call io_bulk(ID,C5=V_bands_t,IPOS=(/1,1,1,1,1/))
 !
1 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_NL',OPR='stop')
#endif
 !
end function

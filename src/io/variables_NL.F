!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, AC
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
integer function variables_NL(ID,local_description)
 !
 use pars,          ONLY:SP,lchlen
 use units,         ONLY:AUT2FS, HA2EV, AU2KWCMm2
 use nl_optics,     ONLY:NL_damping,l_use_DIPOLES,NL_correlation,NL_er,NL_correlation,n_frequencies,      &
&                        n_NL_descriptions,NL_description,WF_sample_ondisk,n_order
 use QP_m,          ONLY:QP_ng_SH,QP_ng_Sx
 use real_time,     ONLY:Integrator_name,NE_steps,RT_step,l_RT_induced_field,RT_bands
 use electric,      ONLY:l_force_SndOrd
 use fields,        ONLY:Efield
 use IO_m,          ONLY:io_elemental,io_status,ver_is_gt_or_eq
 !
 implicit none
 integer,      intent(in) :: ID
 character(lchlen)        :: local_description(100)
 !
 ! Work Space
 !
 integer, parameter   :: i_Prb=1
 integer              :: i_desc
 !
 local_description=' '
 i_desc=0
 !
 ! PARAMETERS
 !
 call io_elemental(ID,VAR="NLPARS",VAR_SZ=13,MENU=0)
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [RT] Time steps       :",I0=NE_steps,     CHECK=.true.,OP=(/"=="/), &
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [RT] Time step    [fs]:",R0=RT_step,      CHECK=.true.,OP=(/"=="/), &
&         DESCRIPTION=local_description(i_desc),UNIT=AUT2FS)
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [NL] Damping      [fs]:",R0=NL_damping,   CHECK=.true.,OP=(/"=="/), &
&         DESCRIPTION=local_description(i_desc),UNIT=HA2EV)
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [NL] Bands  range     :",I1=RT_bands,     CHECK=.true.,OP=(/"==","=="/), &
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [NL] Energy range     :",R1=NL_er,        CHECK=.true.,OP=(/"==","=="/), &
&         DESCRIPTION=local_description(i_desc),UNIT=HA2EV)
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [NL] Number of freqs  :",I0=n_frequencies,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [NL] Force 2nd approx :",L0=l_force_SndOrd,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [NL] Use DIPOLES      :",L0=l_use_DIPOLES,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [NL] Induced Field    :",L0=l_RT_induced_field,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR="[NL] Number of G in HARTREE :",I0=QP_ng_SH,CHECK=.true.,OP=(/"=="/), &
     &         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR="[NL] Number of G in EXCHANGE(DFT) :",I0=QP_ng_Sx,CHECK=.true.,OP=(/"=="/), &
     &         DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 ! WF SAMPLING
 !
 if (ver_is_gt_or_eq(ID,revision=15726)) then !THIS NEEDS TO BE UPDATED IN CASE ENTERS INTO A DISTRO
   call io_elemental(ID,VAR="WF_SAMPLE",VAR_SZ=3,MENU=0)
   i_desc=i_desc+1
   call io_elemental(ID,VAR=" [NL] WF sample order  :",I0=n_order,CHECK=.true.,OP=(/"=="/), & 
        &         DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,VAR=" [NL] WF sample valence:",I0=WF_sample_ondisk%n_val,CHECK=.true.,OP=(/"=="/), & 
        &         DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,VAR=" [NL] WF sample kpoints:",I0=WF_sample_ondisk%n_k,CHECK=.true.,OP=(/"=="/), & 
        &         DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 end if
 !
 ! INTEGRATOR
 !
 call io_elemental(ID,VAR="INTEGRATOR",CH0="",VAR_SZ=1,MENU=0)
 call io_elemental(ID,CH0=Integrator_name,VAR=' [RT] Integrator           :',CHECK=.true.,OP=(/"=="/), &
&        DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 ! CORRELATION
 !
 call io_elemental(ID,VAR="CORRELATION",CH0="",VAR_SZ=1,MENU=0)
 i_desc=i_desc+1
 call io_elemental(ID,CH0=NL_correlation,VAR=' [NL] Correlation           :',CHECK=.true.,OP=(/"=="/), &
&        DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 !
 ! Efield 
 !
 call io_elemental(ID,VAR="FIELD_NAME",CH0="",VAR_SZ=1,MENU=0)
 i_desc=i_desc+1
 call io_elemental(ID,CH0=Efield(i_Prb)%ef_name,VAR=' [FIELDs] Type           :',CHECK=.true.,OP=(/"=="/), &
&        DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 call io_elemental(ID,VAR="EXTERNAL_FIELD",VAR_SZ=10,MENU=0)
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [FIELDs] Versor                :",R1=Efield(i_Prb)%versor,WARN=.true.,OP=(/"==","==","=="/), &
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [FIELDs] Intensity     [kWCMm2]:",R0=Efield(i_Prb)%intensity,CHECK=.true.,OP=(/"=="/),UNIT=AU2KWCMm2, &
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [FIELDs] Damping           [fs]:",R0=Efield(i_Prb)%FWHM,CHECK=.true.,OP=(/"=="/),& 
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [FIELDs] Frequency range   [eV]:",R1=Efield(i_Prb)%frequency,CHECK=.true.,OP=(/"==","=="/),UNIT=HA2EV,& 
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [FIELDs] Frequency steps       :",I0=Efield(i_Prb)%n_frequencies,CHECK=.true.,OP=(/"=="/),& 
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR=" [FIELDs] Frequency step    [eV]:",R0=Efield(i_Prb)%W_step,CHECK=.true.,OP=(/"=="/),UNIT=HA2EV,& 
&         DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,VAR=' [FIELDs] Initial time      [fs]:',R0=Efield(i_Prb)%t_initial,CHECK=.true.,OP=(/"=="/),UNIT=AUT2FS,&
&         DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 variables_NL=io_status(ID)
 n_NL_descriptions=i_desc
 NL_description(:i_desc)=local_description(:i_desc)
 !
end function

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
integer function variables_NL(ID)
 !
 use units,         ONLY:AUT2FS, HA2EV, AU2KWCMm2
 use nl_optics,     ONLY:NL_damping,l_use_DIPOLES,NL_correlation,NL_er,NL_correlation,n_frequencies,      &
&                        WF_sample_ondisk,n_order,NL_desc
 use QP_m,          ONLY:QP_ng_SH,QP_ng_Sx
 use real_time,     ONLY:Integrator_name,NE_steps,RT_step,l_RT_induced_field,RT_bands
 use electric,      ONLY:l_force_SndOrd
 use fields,        ONLY:Efield
 use IO_m,          ONLY:io_elemental,io_status,ver_is_gt_or_eq
 use descriptors,   ONLY:IO_desc_reset
 !
 implicit none
 integer,      intent(in) :: ID
 !
 ! Work Space
 !
 integer, parameter   :: i_Prb=1
 !
 call IO_desc_reset(NL_desc)
 !
 ! PARAMETERS
 !
 call io_elemental(ID,VAR="NLPARS",VAR_SZ=13,MENU=0)
 call io_elemental(ID,VAR="[RT] Time steps",I0=NE_steps,     CHECK=.true.,OP=(/"=="/), &
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[RT] Time step",TERMINATOR="[fs]",R0=RT_step,      CHECK=.true.,OP=(/"=="/), &
&         DESCRIPTOR=NL_desc,UNIT=AUT2FS)
 call io_elemental(ID,VAR="[NL] Damping",TERMINATOR="[fs]",R0=NL_damping,   CHECK=.true.,OP=(/"=="/), &
&         DESCRIPTOR=NL_desc,UNIT=HA2EV)
 call io_elemental(ID,VAR="[NL] Bands  range",I1=RT_bands,     CHECK=.true.,OP=(/"==","=="/), &
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[NL] Energy range",R1=NL_er,        CHECK=.true.,OP=(/"==","=="/), &
&         DESCRIPTOR=NL_desc,UNIT=HA2EV)
 call io_elemental(ID,VAR="[NL] Number of freqs",I0=n_frequencies,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[NL] Force 2nd approx",L0=l_force_SndOrd,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[NL] Use DIPOLES",L0=l_use_DIPOLES,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[NL] Induced Field",L0=l_RT_induced_field,CHECK=.true.,OP=(/"=="/), & 
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[NL] Number of G in HARTREE",I0=QP_ng_SH,CHECK=.true.,OP=(/"=="/), &
     &         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[NL] Number of G in EXCHANGE(DFT)",I0=QP_ng_Sx,CHECK=.true.,OP=(/"=="/), &
     &         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 ! WF SAMPLING
 !
 if (ver_is_gt_or_eq(ID,revision=15726)) then !THIS NEEDS TO BE UPDATED IN CASE ENTERS INTO A DISTRO
   call io_elemental(ID,VAR="WF_SAMPLE",VAR_SZ=3,MENU=0)
   call io_elemental(ID,VAR="[NL] WF sample order   ",I0=n_order,CHECK=.true.,OP=(/"=="/), & 
        &         DESCRIPTOR=NL_desc)
   call io_elemental(ID,VAR="[NL] WF sample valence ",I0=WF_sample_ondisk%n_val,CHECK=.true.,OP=(/"=="/), & 
        &         DESCRIPTOR=NL_desc)
   call io_elemental(ID,VAR="[NL] WF sample kpoints ",I0=WF_sample_ondisk%n_k,CHECK=.true.,OP=(/"=="/), & 
        &         DESCRIPTOR=NL_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 end if
 !
 ! INTEGRATOR
 !
 call io_elemental(ID,VAR="INTEGRATOR",CH0="",VAR_SZ=1,MENU=0)
 call io_elemental(ID,CH0=Integrator_name,VAR=' [RT] Integrator           ',CHECK=.true.,OP=(/"=="/), &
&        DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 ! CORRELATION
 !
 call io_elemental(ID,VAR="CORRELATION",CH0="",VAR_SZ=1,MENU=0)
 call io_elemental(ID,CH0=NL_correlation,VAR=' [NL] Correlation           ',CHECK=.true.,OP=(/"=="/), &
&        DESCRIPTOR=NL_desc)
 !
 ! Efield 
 !
 call io_elemental(ID,VAR="FIELD_NAME",CH0="",VAR_SZ=1,MENU=0)
 call io_elemental(ID,CH0=Efield(i_Prb)%ef_name,VAR=' [FIELDs] Type           ',CHECK=.true.,OP=(/"=="/), &
&        DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 call io_elemental(ID,VAR="EXTERNAL_FIELD",VAR_SZ=10,MENU=0)
 call io_elemental(ID,VAR="[FIELDs] Versor                 ",R1=Efield(i_Prb)%versor,WARN=.true.,OP=(/"==","==","=="/), &
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[FIELDs] Intensity     ",TERMINATOR="[kWCMm2]",&
&         R0=Efield(i_Prb)%intensity,CHECK=.true.,OP=(/"=="/),UNIT=AU2KWCMm2, &
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[FIELDs] Damping           ",TERMINATOR="[fs]",&
&         R0=Efield(i_Prb)%FWHM,CHECK=.true.,OP=(/"=="/),& 
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[FIELDs] Frequency range   ",TERMINATOR="[eV]",&
&         R1=Efield(i_Prb)%frequency,CHECK=.true.,OP=(/"==","=="/),UNIT=HA2EV,& 
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[FIELDs] Frequency steps",I0=Efield(i_Prb)%n_frequencies,CHECK=.true.,OP=(/"=="/),& 
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="[FIELDs] Frequency step    ",TERMINATOR="[eV]",&
&         R0=Efield(i_Prb)%W_step,CHECK=.true.,OP=(/"=="/),UNIT=HA2EV,& 
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR=" [FIELDs] Initial time      ",TERMINATOR="[fs]",&
&         R0=Efield(i_Prb)%t_initial,CHECK=.true.,OP=(/"=="/),UNIT=AUT2FS,&
&         DESCRIPTOR=NL_desc)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 variables_NL=io_status(ID)
 !
end function

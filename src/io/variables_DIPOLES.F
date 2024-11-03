!
! License-Identifier: GPL
!
! Copyright (C) 2011 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
integer function variables_DIPOLES(Dip,ID)
 !
 use pars,          ONLY:SP,CR_YIO
 use units,         ONLY:HA2EV
 use DIPOLES,       ONLY:DIPOLE_t,DIP_desc
 use IO_m,          ONLY:io_status
 use IO_int,        ONLY:io_variable_elemental,def_variable_elemental,io_elemental
 use global_XC,     ONLY:Dipole_WF_xc_string,loaded_WF_xc_string
 use descriptors,   ONLY:IO_desc_reset
 !
 implicit none
 !
 type(DIPOLE_t) :: Dip
 integer        :: ID
 !
 ! Workspace
 !
 call IO_desc_reset(DIP_desc)
 !
 call def_variable_elemental(ID,"PARS",11,SP,0)
 !
 call io_variable_elemental(ID,DB_I1=Dip%db_ib,VAR="DIP band range",I1=Dip%ib,CHECK=.true.,OP=(/">=","<="/),&
&                           DESCRIPTOR=DIP_desc)
 call io_variable_elemental(ID,DB_I1=Dip%db_ib_lim,VAR="DIP band range limits",I1=Dip%ib_lim,CHECK=.true.,OP=(/"<=",">="/),&
&                           DESCRIPTOR=DIP_desc)
 call io_variable_elemental(ID,UNIT=HA2EV,&
&     VAR="DIP e/h energy range ",TERMINATOR="[eV]",R1=Dip%ehe,CHECK=.true.,OP=(/">=","<="/),DESCRIPTOR=DIP_desc)
 call io_variable_elemental(ID,VAR="RL vectors in the sum",I0=Dip%ng,WARN=.true.,OP=(/"<="/),&
&                           DESCRIPTOR=DIP_desc)
 call io_variable_elemental(ID,VAR="[r,Vnl] included",L0=Dip%Vnl_included,CHECK=.true.,OP=(/"=="/),&
&                           DESCRIPTOR=DIP_desc)
 call io_variable_elemental(ID,VAR="Bands ordered",L0=Dip%bands_ordered,CHECK=.true.,OP=(/">="/),&
&                           DESCRIPTOR=DIP_desc)
 call io_variable_elemental(ID,VAR="Direct v evaluation",L0=Dip%force_v_g_space,CHECK=.true.,OP=(/"=="/),&
&                           DESCRIPTOR=DIP_desc)
 !
 ! Character variables
 !
 call def_variable_elemental(ID,"APPROACH",1,CR_YIO,0)
 call  io_variable_elemental(ID,VAR="Approach used",CH0=Dip%approach,CHECK=.true.,OP=(/"=="/),&
&                           DESCRIPTOR=DIP_desc)
 !
 call def_variable_elemental(ID,"KINDS",1,CR_YIO,0)
 call  io_variable_elemental(ID,VAR="Dipoles computed",CH0=Dip%computed,CHECK=.true.,OP=(/"<="/),&
&                           DESCRIPTOR=DIP_desc)
 !
 ! Wavefunctions xc 
 !
 call def_variable_elemental(ID,'WAVE_FUNC_XC',1,CR_YIO,0)
 call  io_variable_elemental(ID,DB_CH0=Dipole_WF_xc_string,CH0=loaded_WF_xc_string,VAR=' Wavefunctions',CHECK=.true.,OP=(/"=="/),&
&                           DESCRIPTOR=DIP_desc)
 call io_elemental(ID,VAR="",VAR_SZ=0)
 !
 variables_DIPOLES=io_status(ID)
 !
end function variables_DIPOLES
!
!
integer function variables_Overlaps(Dip,ID)
 !
 use pars,          ONLY:IP_YIO,LP_YIO,CR_YIO
 use DIPOLES,       ONLY:DIPOLE_t,DIP_desc
 use IO_m,          ONLY:io_status
 use IO_int,        ONLY:io_variable_elemental,def_variable_elemental,io_elemental
 use global_XC,     ONLY:Dipole_WF_xc_string,loaded_WF_xc_string
 use descriptors,   ONLY:IO_desc_reset
 !
 implicit none
 !
 type(DIPOLE_t) :: Dip
 integer        :: ID
 !
 ! Workspace
 !
 call IO_desc_reset(DIP_desc)
 !
 call def_variable_elemental(ID,"Bands",2,IP_YIO,0)
 call io_variable_elemental(ID,DB_I1=Dip%db_ib,VAR="DIP band range",I1=Dip%ib,CHECK=.true.,OP=(/">=","<="/),&
&                           DESCRIPTOR=DIP_desc)
 !
 call def_variable_elemental(ID,"Bands_limits",2,IP_YIO,0)
 call io_variable_elemental(ID,DB_I1=Dip%db_ib_lim,VAR="DIP band range limits",I1=Dip%ib_lim,CHECK=.true.,OP=(/"<=",">="/),&
&                           DESCRIPTOR=DIP_desc)
 !
 call def_variable_elemental(ID,"Bands_ordered",1,LP_YIO,0)
 call io_variable_elemental(ID,VAR="Bands ordered",L0=Dip%bands_ordered,CHECK=.true.,OP=(/">="/),&
&                           DESCRIPTOR=DIP_desc)
 !
 call def_variable_elemental(ID,"NG",1,IP_YIO,0)
 call io_variable_elemental(ID,VAR="RL vectors in the sum",I0=Dip%ng,WARN=.true.,OP=(/"<="/),&
&                           DESCRIPTOR=DIP_desc)
 !
 ! Wavefunctions xc 
 !
 call def_variable_elemental(ID,'WAVE_FUNC_XC',1,CR_YIO,0)
 call  io_variable_elemental(ID,DB_CH0=Dipole_WF_xc_string,CH0=loaded_WF_xc_string,VAR=' Wavefunctions',CHECK=.true.,OP=(/"=="/),&
&                           DESCRIPTOR=DIP_desc)
 !
 call def_variable_elemental(ID,"",0,0,1)
 !
 variables_Overlaps=io_status(ID)
 !
end function variables_Overlaps

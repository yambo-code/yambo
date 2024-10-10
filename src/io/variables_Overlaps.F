!
! License-Identifier: GPL
!
! Copyright (C) 2011 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
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

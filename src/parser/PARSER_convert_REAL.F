!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARSER_convert_REAL(rfac,v_descr)
 !
 use pars,   ONLY:SP
 use units,  ONLY:HA2EV,HA2GHZ,HA2THZ,FS2AUT,PS2AUT,AS2AUT,AU2Tesla,&
&                 AU2VMm1,DEG2RAD,AU2KWCMm2
 !
 implicit none
 !
 real(SP):: rfac
 character(*)::v_descr
 !
 if(rfac<0) return
 !
 if (index(trim(v_descr),'meV')/=0) then
   rfac=HA2EV*1000.
 else if (index(trim(v_descr),'eV')/=0) then
   rfac=HA2EV
 else if (index(trim(v_descr),'mHa')/=0) then
   rfac=1.*1000.
 else if (index(trim(v_descr),'Ha')/=0) then
   rfac=1.
 else if (index(trim(v_descr),'mRy')/=0) then
   rfac=2.*1000.
 else if (index(trim(v_descr),'Ry')/=0) then
   rfac=2.
 else if (index(trim(v_descr),'K')/=0) then
   rfac=11604.*HA2EV
 else if (index(trim(v_descr),'GHz')/=0) then
   rfac=HA2GHZ
 else if (index(trim(v_descr),'THz')/=0) then
   rfac=HA2THZ
 else if (index(trim(v_descr),'ps')/=0) then
   rfac=1._SP/PS2AUT
 else if (index(trim(v_descr),'fs')/=0) then
   rfac=1./FS2AUT
 else if (index(trim(v_descr),'as')/=0) then
   rfac=1./AS2AUT
 else if (index(trim(v_descr),'T')/=0) then
   rfac=AU2Tesla
 else if (index(trim(v_descr),'V/mm')/=0) then
   rfac=AU2VMm1
 else if (index(trim(v_descr),'deg')/=0) then
   rfac=1./DEG2RAD
 else if (index(trim(v_descr),'kWLm2')/=0) then
   rfac=AU2KWCMm2
 endif
 !
end subroutine

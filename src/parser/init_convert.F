!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine init_convert(rfac,ivar,v_descr,force_RL)
 !
 use pars,   ONLY:SP
 use units
 !
 implicit none
 !
 real(SP):: rfac
 integer :: ivar
 character(*)::v_descr
 logical     ::force_RL
 !
 if (ivar/=0) then
   if (index(v_descr,'RL')/=0) then
     call Gclose(ivar,'tRL')
   else if (index(v_descr,'mHa')/=0) then
     if (force_RL     ) call Gclose(ivar,'tRL')
     if (.not.force_RL) call Gclose(ivar,'tmHa')
   else if (index(v_descr,'Ha')/=0) then
     if (force_RL     ) call Gclose(ivar,'tRL')
     if (.not.force_RL) call Gclose(ivar,'tHa')
   else if (index(v_descr,'mRy')/=0) then
     if (force_RL     ) call Gclose(ivar,'tRL')
     if (.not.force_RL) call Gclose(ivar,'tmRy')
   else if (index(v_descr,'Ry')/=0) then
     if (force_RL     ) call Gclose(ivar,'tRL')
     if (.not.force_RL) call Gclose(ivar,'tRy')
   endif
   return
 endif
 !
 if (rfac>0.) then
   if (index(trim(v_descr),'meV')/=0) then
     rfac=HARTREE*1000.
   else if (index(trim(v_descr),'eV')/=0) then
     rfac=HARTREE
   else if (index(trim(v_descr),'mHa')/=0) then
     rfac=1.*1000.
   else if (index(trim(v_descr),'Ha')/=0) then
     rfac=1.
   else if (index(trim(v_descr),'mRy')/=0) then
     rfac=2.*1000.
   else if (index(trim(v_descr),'Ry')/=0) then
     rfac=2.
   else if (index(trim(v_descr),'K')/=0) then
     rfac=11604.*HARTREE
   else if (index(trim(v_descr),'GHz')/=0) then
     rfac=HA2GHZ
   else if (index(trim(v_descr),'THz')/=0) then
     rfac=HA2THZ
   else if (index(trim(v_descr),'ps')/=0) then
     rfac=1./PS2AUT
   else if (index(trim(v_descr),'fs')/=0) then
     rfac=1./FS2AUT
   else if (index(trim(v_descr),'T')/=0) then
     rfac=AU2Tesla
   else if (index(trim(v_descr),'V/mm')/=0) then
     rfac=AU2Vmm
   else if (index(trim(v_descr),'deg')/=0) then
     rfac=1./radian
   else if (index(trim(v_descr),'kWLm2')/=0) then
     rfac=1./W2Au
   endif
 endif
 !
end subroutine

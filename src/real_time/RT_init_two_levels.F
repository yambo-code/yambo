!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_init_two_levels(Xen)
 !
 use pars,           ONLY:SP,cI
 use fields,         ONLY:Efield,global_gauge
 use pars,           ONLY:pi,cI
 use units,          ONLY:HARTREE,FS2AUT,HBAR_eVfs
 use com,            ONLY:msg
 use X_m,            ONLY:DIP_iR,DIP_P
 use real_time,      ONLY:W_pump,Wo,Rabi_freq_pump,P,W_probe,Rabi_nn,D,Rabi_freq_probe,l_3levels,dip,W1,tau_offdiag
 use SC,             ONLY:SC_bands
 use electrons,      ONLY:levels,nel
 use com,            ONLY:error,warning
 !
 implicit none
 type(levels), intent(in) :: Xen
 ! 
 ! Work Space
 !
 integer     :: i_Pump,i_Probe
 complex(SP) :: mu(2)
 !
 Wo    =Xen%E(2,1,1)-Xen%E(1,1,1)
 if(l_3levels) then
   if(SC_bands(2)/=3) call error(" Error set SCBands=3 !! ")
   W1=Xen%E(3,1,1)-Xen%E(1,1,1)
   if(nel/=2) call error(" Error set Nelectro=2 !! ")
 endif
 !
 i_Probe=2
 i_Pump =1
 if (Efield(2)%ef_name/="none") then
   i_Pump=2
   i_Probe=1
 endif
 !
 if(l_3levels) then
   !
   ! Three level case
   !
   if(any(dip(:)/=0._SP)) then
     !
     DIP_iR=(0._SP,0._SP)
     DIP_P =(0._SP,0._SP)
     !
     DIP_P(1,1,2,1,1)= cmplx(0._SP,dip(1))
     DIP_P(1,1,3,1,1)= cmplx(0._SP,dip(2))
     DIP_P(1,2,3,1,1)= cmplx(0._SP,dip(3))
!     DIP_P(3,2,3,1,1)= cmplx(0._SP,dip(3))
     !
     DIP_P(1,2,1,1,1)=conjg( DIP_P(1,1,2,1,1))
     DIP_P(1,3,1,1,1)=conjg( DIP_P(1,1,3,1,1))
     DIP_P(1,3,2,1,1)=conjg( DIP_P(1,2,3,1,1))
!     DIP_P(3,3,2,1,1)=conjg( DIP_P(3,2,3,1,1))
     !
     DIP_iR(1,1,2,1,1)= DIP_P(1,1,2,1,1) /(Wo)
     DIP_iR(1,1,3,1,1)= DIP_P(1,1,3,1,1) /(W1)
     DIP_iR(1,2,3,1,1)= DIP_P(1,2,3,1,1) /(W1-Wo)
!     DIP_iR(3,2,3,1,1)= DIP_P(3,2,3,1,1) /(W1-Wo)
     !
     DIP_iR(1,2,1,1,1)= -conjg(DIP_iR(1,1,2,1,1))
     DIP_iR(1,3,1,1,1)= -conjg(DIP_iR(1,1,3,1,1))
     DIP_iR(1,3,2,1,1)= -conjg(DIP_iR(1,2,3,1,1))

!     DIP_iR(3,3,2,1,1)= -conjg(DIP_iR(3,2,3,1,1))
     !
   else
     call warning(" Using intrinsic dipoles ")
     call msg('s','[3 LEVELS] DipR (12,13,23) x',(/abs(DIP_iR(1,1,2,1,1)),abs(DIP_iR(1,1,3,1,1)),abs(DIP_iR(1,2,3,1,1))/))
     call msg('s','[3 LEVELS] DipP (12,13,23) x',(/abs(DIP_P(1,1,2,1,1)),abs(DIP_P(1,1,3,1,1)),abs(DIP_P(1,2,3,1,1))/))
     call msg('s','[3 LEVELS] DipR (12,13,23) y',(/abs(DIP_iR(2,1,2,1,1)),abs(DIP_iR(2,1,3,1,1)),abs(DIP_iR(2,2,3,1,1))/))
     call msg('s','[3 LEVELS] DipP (12,13,23) y',(/abs(DIP_P(2,1,2,1,1)),abs(DIP_P(2,1,3,1,1)),abs(DIP_P(2,2,3,1,1))/))
     call msg('s','[3 LEVELS] DipR (12,13,23) z',(/abs(DIP_iR(3,1,2,1,1)),abs(DIP_iR(3,1,3,1,1)),abs(DIP_iR(3,2,3,1,1))/))
     call msg('s','[3 LEVELS] DipP (12,13,23) z',(/abs(DIP_P(3,1,2,1,1)),abs(DIP_P(3,1,3,1,1)),abs(DIP_P(3,2,3,1,1))/))  
   endif
   !
   select case(trim(Efield(i_Pump)%ef_name))
     case('ANTIRES','SIN')
       W_pump=-Efield(i_Pump)%frequency(1)
     case('RES')
       W_pump=Efield(i_Pump)%frequency(1)
   end select
   !
   call msg('s','[3 LEVELS] Energy Levels [eV]',(/Wo*HARTREE,W1*HARTREE/))
   call msg('s','[3 LEVELS] Pump Rabi freq[eV] ', &
&           abs(2.*Efield(i_pump)%amplitude*sum(DIP_P(:,2,3,1,1)*Efield(i_pump)%versor(:))/W_pump/cI*HARTREE)) 
   call msg('s','[3 LEVELS] Damping [eV] ',HBAR_eVfs/tau_offdiag(1:3)*FS2AUT)
   !
 else
   !
   ! Two level case
   !
   P          =DIP_P(1,2,1,1,1)
   D          =cI*DIP_iR(1,2,1,1,1)
   mu(i_Pump) =P
   !
   select case(trim(Efield(i_Pump)%ef_name))
     case('ANTIRES')
       W_pump=Efield(i_Pump)%frequency(1)
       If ( trim(global_gauge)=="velocity") mu(i_Pump) =P/cI/W_pump
     case('RES')
       W_pump=Efield(i_Pump)%frequency(1)
       If ( trim(global_gauge)=="velocity") mu(i_Pump) =-P/cI/W_pump
     case('DELTA')
       mu(i_Pump) =P
       W_pump=0.
   end select
   !
   Rabi_freq_pump=2.*Efield(i_Pump)%amplitude*mu(i_Pump)
   !
   call msg('s','[2 LEVELS] Wo              [eV   ]',abs(Wo)*HARTREE)
   !
   call msg('s','[2 LEVELS] Pump Rabi freq  [eV   ]',abs(Rabi_freq_pump)*HARTREE)
   call msg('s','[2 LEVELS] Pump freq       [eV/fs]',(/abs(W_pump)*HARTREE,HBAR_eVfs/(W_pump*HARTREE) /))
   W_probe=0.
   !
   if (i_Pump/=i_Probe) then
     !
     W_probe=Efield(i_Probe)%frequency(1)
     !
     if ( trim(Efield(i_Probe)%ef_name) == 'RES' ) mu(i_Probe) =-P/cI/Efield(i_Probe)%frequency(1)
     if ( trim(Efield(i_Probe)%ef_name) == 'ANTIRES' ) mu(i_Probe) =P/cI/Efield(i_Probe)%frequency(1)
     if ( trim(Efield(i_Probe)%ef_name) == 'DELTA' ) mu(i_Probe) =P
     !
     Rabi_freq_probe=2.*Efield(i_Probe)%amplitude*mu(i_Probe)
     !
     call msg('s','[2 LEVELS] Probe Rabi freq [eV   ]',abs(Rabi_freq_probe)*HARTREE)
     call msg('s','[2 LEVELS] Probe freq      [eV/fs]',(/abs(W_probe)*HARTREE,HBAR_eVfs/(W_probe*HARTREE) /))
     !
     Rabi_nn(1,1)=4.*Efield(i_Pump)%amplitude*Efield(i_Pump)%amplitude*conjg(mu(i_Pump))*mu(i_Pump)
     Rabi_nn(1,2)=4.*Efield(i_Pump)%amplitude*Efield(i_Probe)%amplitude*conjg(mu(i_Pump))*mu(i_Probe)
     Rabi_nn(2,1)=4.*Efield(i_Probe)%amplitude*Efield(i_Pump)%amplitude*conjg(mu(i_Probe))*mu(i_Pump)
     Rabi_nn(2,2)=4.*Efield(i_Probe)%amplitude*Efield(i_Probe)%amplitude*conjg(mu(i_Probe))*mu(i_Probe)
   endif
   !
 endif
 !
end subroutine RT_init_two_levels

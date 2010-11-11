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
subroutine RT_init_vecpot()
 !
 ! This subroutine initialize the t=0 external vector potential 
 ! used as starting point for the EOM for A(t)
 !
 use pars,           ONLY:SP,cI
 use com,            ONLY:error
 use units,          ONLY:SPEED_OF_LIGHT
 use fields,         ONLY:ext_field,A_vecpot,n_ext_fields,Efield
#if defined _TWO_LEVELS
 use pars,           ONLY:pi,cI
 use units,          ONLY:HARTREE,HA2FSm1,FS2AUT
 use com,            ONLY:msg
 use X_m,            ONLY:DIP_iR,DIP_P
 use real_time,      ONLY:W_pump,Wo,Rabi_freq_pump,P,delta,Rabi_nn,D,Rabi_freq_probe
 use real_time,      ONLY:l_3levels,dip,W1,tau_offdiag
#endif
 !
 implicit none
 ! 
 ! Work Space
 !
 integer     :: i1
 complex(SP) :: E_times_c(3)
#if defined _TWO_LEVELS
 integer     :: i_pump,i_probe
 complex(SP) :: mu(2)
#endif
 !
 A_vecpot%vecpot    =(0._SP,0._SP)
 A_vecpot%vecpot_vel=(0._SP,0._SP)
 !
#if defined _TWO_LEVELS
 !
 i_probe=2
 i_pump =1
 if (Efield(2)%ef_name/="none") then
   i_pump=2
   i_probe=1
 endif
 !
 if(l_3levels) then
   !
   ! Three level case
   !
   DIP_iR=(0.,0.)
   DIP_P =(0.,0.)
   !
   DIP_P(1,1,2,1,1)= cmplx(0.,dip(1))
   DIP_P(1,1,3,1,1)= cmplx(0.,dip(2))
   DIP_P(1,2,3,1,1)= cmplx(0.,dip(3))
   !
   DIP_P(1,2,1,1,1)=-cmplx(0.,dip(1))
   DIP_P(1,3,1,1,1)=-cmplx(0.,dip(2))
   DIP_P(1,3,2,1,1)=-cmplx(0.,dip(3))
   !
   DIP_iR(1,1,2,1,1)= DIP_P(1,1,2,1,1) /(-Wo)
   DIP_iR(1,1,3,1,1)= DIP_P(1,1,3,1,1) /(-W1)
   DIP_iR(1,2,3,1,1)= DIP_P(1,2,3,1,1) /(-W1+Wo)
   !
   DIP_iR(1,2,1,1,1)= DIP_P(1,2,1,1,1) /(+Wo)
   DIP_iR(1,3,1,1,1)= DIP_P(1,3,1,1,1) /(+W1)
   DIP_iR(1,3,2,1,1)= DIP_P(1,3,2,1,1) /(+W1-Wo)
   !
   select case(trim(Efield(i_pump)%ef_name))
     case('ANTIRES')
       W_pump=-Efield(i_pump)%frequency 
     case('RES')
       W_pump=Efield(i_pump)%frequency 
   end select
   !
   call msg('s','[3 LEVELS] Energy Levels [eV]',(/Wo*HARTREE,W1*HARTREE/))
   !
 else
   !
   ! Two level case
   !
   DIP_iR=(0.,0.)
   DIP_P =(0.,0.)
   DIP_P(1,1,2,1,1)= cI
   DIP_P(1,2,1,1,1)=-cI
   DIP_iR(1,2,1,1,1)= DIP_P(1,2,1,1,1) /(+Wo)
   DIP_iR(1,1,2,1,1)= DIP_P(1,1,2,1,1) /(-Wo)
   P        =  DIP_P(1,1,2,1,1)
   D        =  cI*DIP_iR(1,1,2,1,1)
   !
   select case(trim(Efield(i_pump)%ef_name))
     case('ANTIRES')
       W_pump=Efield(i_pump)%frequency 
       mu(i_pump) = P/cI/W_pump
     case('RES')
       W_pump=Efield(i_pump)%frequency 
       mu(i_pump) =-P/cI/W_pump
     case('DELTA')
       mu(i_pump) =P
       W_pump=0.
   end select
   !
   Rabi_freq_pump=2.*Efield(i_pump)%amplitude*mu(i_pump)
   !
   call msg('s','[2 LEVELS] Wo              [eV   ]',abs(Wo)*HARTREE)
   call msg('s','[2 LEVELS] Pump Rabi freq  [eV   ]',abs(Rabi_freq_pump)*HARTREE)
   call msg('s','[2 LEVELS] Pump freq       [eV/fs]',(/abs(W_pump)*HARTREE,2.*pi/W_pump/FS2AUT /))
   delta=0.
   !
   if (i_pump/=1) then
     !
     delta=Efield(i_probe)%frequency-Efield(i_pump)%frequency
     !
     if ( trim(Efield(i_probe)%ef_name) == 'RES' ) mu(i_probe) =-P/cI/Efield(i_probe)%frequency
     if ( trim(Efield(i_probe)%ef_name) == 'ANTIRES' ) mu(i_probe) =P/cI/Efield(i_probe)%frequency
     if ( trim(Efield(i_probe)%ef_name) == 'DELTA' ) mu(i_probe) =P
     !
     Rabi_freq_probe=2.*Efield(i_probe)%amplitude*mu(i_probe)
     !
     call msg('s','[2 LEVELS] Probe Rabi freq [eV   ]',abs(Rabi_freq_probe)*HARTREE)
     call msg('s','[2 LEVELS] Probe freq      [eV/fs]',(/abs(W_pump+delta)*HARTREE,2.*pi/(W_pump+delta)/FS2AUT /))
     call msg('s','[2 LEVELS] Probe detuning  [eV/fs]',(/abs(delta)*HARTREE,2.*pi/abs(delta)/FS2AUT/))
     !
     Rabi_nn(1,1)=4.*Efield(i_pump)%amplitude*Efield(i_pump)%amplitude*conjg(mu(i_pump))*mu(i_pump)
     Rabi_nn(1,2)=4.*Efield(i_pump)%amplitude*Efield(i_probe)%amplitude*conjg(mu(i_pump))*mu(i_probe)
     Rabi_nn(2,1)=4.*Efield(i_probe)%amplitude*Efield(i_pump)%amplitude*conjg(mu(i_probe))*mu(i_pump)
     Rabi_nn(2,2)=4.*Efield(i_probe)%amplitude*Efield(i_probe)%amplitude*conjg(mu(i_probe))*mu(i_probe)
   endif
   !
 endif
 !
#endif
 !
 do i1=1,n_ext_fields
   ! 
   if(Efield(i1)%ef_name=="none") cycle
   !
   E_times_c=Efield(i1)%amplitude*Efield(i1)%versor(:)*SPEED_OF_LIGHT
   !
   ! Formally speaking:
   !
   !   A'(t=0) = -c E(t=0)
   !   A(t=0)  = -c [\int E(t)]|
   !                           |t=0
   !
   select case(trim(Efield(i1)%ef_name))
     case('SIN')
       A_vecpot%vecpot(:)    =A_vecpot%vecpot(:)    +E_times_c(:)/( Efield(i1)%frequency )
     case('ANTIRES')
       A_vecpot%vecpot_vel(:)=A_vecpot%vecpot_vel(:)-E_times_c(:)
       A_vecpot%vecpot(:)    =A_vecpot%vecpot(:)    -E_times_c(:)/( cI* Efield(i1)%frequency )
     case('RES')
       A_vecpot%vecpot_vel(:)=A_vecpot%vecpot_vel(:)-E_times_c(:)
       A_vecpot%vecpot(:)    =A_vecpot%vecpot(:)    +E_times_c(:)/( cI* Efield(i1)%frequency )
     case('DELTA')
     case('GAUSS')
     case('QSSIN')
     case default
       call error(" External Field "//trim(Efield(i1)%ef_name)//" not implemented ") 
   end select
   !
 enddo
 !
end subroutine RT_init_vecpot

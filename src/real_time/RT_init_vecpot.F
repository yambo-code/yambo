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
 use real_time,      ONLY:W_photon,Wo,Rabi_freq,P,W_detuning,delta,Rabi_nn_square,D
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
 DIP_iR=(0.,0.)
 DIP_P=(0.,0.)
 DIP_P(1,1,2,1,1)= cI
 DIP_P(1,2,1,1,1)=-cI
 DIP_iR(1,2,1,1,1)= DIP_P(1,2,1,1,1) /(+Wo)
 DIP_iR(1,1,2,1,1)= DIP_P(1,1,2,1,1) /(-Wo)
 P        =  DIP_P(1,1,2,1,1)
 D        =  cI*DIP_iR(1,1,2,1,1)
 select case(trim(Efield(i_pump)%ef_name))
   case('RES','ANTIRES')
     W_photon=Efield(i_pump)%frequency 
     Rabi_freq=2.*P*Efield(i_pump)%amplitude/W_photon
   case('DELTA')
     Rabi_freq=2.*P*Efield(i_pump)%amplitude
     W_photon=0.
 end select
 W_detuning=Wo-W_photon
 call msg('s','[2 LEVELS] Pump Rabi freq  [eV   ]',abs(Rabi_freq)*HARTREE)
 call msg('s','[2 LEVELS] Pump freq       [eV/fs]',(/abs(W_photon)*HARTREE,2.*pi/W_photon/FS2AUT /))
 delta=Efield(i_probe)%frequency-Efield(i_pump)%frequency
 call msg('s','[2 LEVELS] Probe detuning  [eV/fs]',(/abs(delta)*HARTREE,2.*pi/abs(delta)/FS2AUT/))
 !
 Rabi_nn_square(1,1)=2.*abs(P)**2.*Efield(1)%amplitude*Efield(1)%amplitude/Efield(1)%frequency/Efield(1)%frequency
 Rabi_nn_square(1,2)=2.*abs(P)**2.*Efield(1)%amplitude*Efield(2)%amplitude/Efield(1)%frequency/Efield(2)%frequency
 Rabi_nn_square(2,1)=2.*abs(P)**2.*Efield(2)%amplitude*Efield(1)%amplitude/Efield(2)%frequency/Efield(1)%frequency
 Rabi_nn_square(2,2)=2.*abs(P)**2.*Efield(2)%amplitude*Efield(2)%amplitude/Efield(2)%frequency/Efield(2)%frequency
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

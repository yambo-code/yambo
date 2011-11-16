!
! Copyright (C) 2000-2011 A. Marini, M. Gruening and the YAMBO team 
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
subroutine RT_Rabi_Function(E,Field)
 !
 ! Here I report the current Rabi frequency and
 ! also the values of I which makes the Rabi freq
 ! equal to 1meV, 10meV, 100meV and 1 eV
 !
 ! Rabi=2 * \mu.E
 !
 ! E is the field amplitude = sqrt ( I *8*pi/c  ) 
 !
 ! Thus
 ! 
 ! I(Rabi)=c/(8 pi) * 4 * (Rabi/|\mu.\versor{E}|)^2
 !
 ! I is given in the input file and I[au]=I[kWLm2]*AU2KWCMm2
 !
 use pars,           ONLY:SP,cI,pi
 use units,          ONLY:HA2EV,SPEED_OF_LIGHT,AU2KWCMm2
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:nkibz,bz_samp 
 use X_m,            ONLY:DIP_P,DIP_iR
 use fields,         ONLY:Efield,i_Pump,ext_field,Rabi_Factor
 use SC,             ONLY:SC_bands
 use com,            ONLY:msg
 use stderr,         ONLY:set_real_printed_length
 !
 implicit none
 type(levels),    intent(in) :: E
 type(ext_field), intent(in) :: Field
 !
 ! Work Space
 !
 integer         :: ik,iv,ic,nh
 complex(SP)     :: Rabi
 real(SP)        :: Rabi_max,two_mu,I_ref(4),Rabi_ref(4)
 real(SP)        :: Cutoff, Critical, Cutoff_max,Critical_min,Wo
 !
 ! Max Rabi factor
 !
 Rabi_max=0._SP
 Cutoff_max = 0._SP
 Critical_min = 1000._SP
 !
 do ik=1,nkibz
   do iv=1,E%nbm
     do ic=E%nbf+1,SC_bands(2)
       !
       if (abs(E%E(iv,ik,1)-E%E(ic,ik,1))<=1.E-5_SP) cycle
       !
       Wo = E%E(ic,ik,1)-E%E(iv,ik,1)
       Rabi=Rabi_Factor(Field, DIP_P(:,ic,iv,ik,1),-cI*DIP_iR(:,ic,iv,ik,1))
       Critical = sqrt(Field%frequency(1)*Wo)
       Cutoff = sqrt(Wo**2 + 4.*abs(Rabi)**2)
       !
       if (abs(Rabi) > Rabi_max) Rabi_max=abs(Rabi)
       if (abs(Rabi).ge.Critical) then
         if (Critical < Critical_min) Critical_min = Critical
         if (Cutoff > Cutoff_max) Cutoff_max = Cutoff
       end if
       !
     enddo
   enddo
 enddo
 !
 call msg( 'r', '[RT] Max Rabi frequency            [eV]:',Rabi_max*HA2EV)
 if (Critical_min.lt.1000._SP.and.Field%frequency(1)/=0._SP) then  
   call msg( 'r', '[RT] Max cutoff frequency          [eV]:',Cutoff_max*HA2EV)
   nh = floor(Cutoff_max/Field%frequency(1))
   call msg( 'r', '[RT] Max cutoff harmonic               :',nh)
   call msg( 'r', '[RT] Min critical frequency        [eV]:',Critical_min*HA2EV)
   nh = floor(Critical_min/Field%frequency(1))
   call msg( 'r', '[RT] Min critical harmonic             :',nh)
 endif
 !
 two_mu=Rabi_max/Field%amplitude
 !
 Rabi_ref=(/0.001,0.01,0.1,1./)/HA2EV
 !
 I_ref(:)=SPEED_OF_LIGHT/(8.*pi)*(Rabi_ref(:)/two_mu)**2.
 !
 call set_real_printed_length(g_length=15)
 !
 call msg( 'r', '[RT] I when Rabi=1meV           [kWCMm2]:',I_ref(1)*AU2KWCMm2)
 call msg( 'r', '[RT] I when Rabi=10meV          [kWCMm2]:',I_ref(2)*AU2KWCMm2)
 call msg( 'r', '[RT] I when Rabi=100meV         [kWCMm2]:',I_ref(3)*AU2KWCMm2)
 call msg( 'r', '[RT] I when Rabi=1eV            [kWCMm2]:',I_ref(4)*AU2KWCMm2)
 !
 call set_real_printed_length()
 !
end subroutine RT_Rabi_Function

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
 ! I is given in the input file and I[au]=I[kWLm2]*W2Au
 !
 ! I also
 !
 use pars,           ONLY:SP,cI,pi
 use units,          ONLY:HARTREE,SPEED_OF_LIGHT,W2Au
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
 integer         :: ik,iv,ic
 complex(SP)     :: Rabi
 real(SP)        :: Rabi_max,two_mu,I_ref(4),Rabi_ref(4)
 !
 ! Max Rabi factor
 !
 do ik=1,nkibz
   do iv=1,E%nbm
     do ic=E%nbf+1,SC_bands(2)
       !
       if (abs(E%E(iv,ik,1)-E%E(ic,ik,1))<=1.E-5_SP) cycle
       !
       Rabi=Rabi_Factor(Field, DIP_P(:,ic,iv,ik,1),-cI*DIP_iR(:,ic,iv,ik,1))
       !
       if (abs(Rabi) > Rabi_max) Rabi_max=abs(Rabi)
       !
     enddo
   enddo
 enddo
 !
 call msg( 'sr', '[RT] Max Rabi frequency            [eV]:',Rabi_max*HARTREE)
 !
 two_mu=Rabi_max/Field%amplitude
 !
 Rabi_ref=(/0.001,0.01,0.1,1./)/HARTREE
 !
 I_ref(:)=SPEED_OF_LIGHT/(8.*pi)*(Rabi_ref(:)/two_mu)**2.
 !
 call set_real_printed_length(g_length=15)
 !
 call msg( 'r', '[RT] I when Rabi=1meV           [kWLm2]:',I_ref(1)/W2Au)
 call msg( 'r', '[RT] I when Rabi=10meV          [kWLm2]:',I_ref(2)/W2Au)
 call msg( 'r', '[RT] I when Rabi=100meV         [kWLm2]:',I_ref(3)/W2Au)
 call msg( 'r', '[RT] I when Rabi=1eV            [kWLm2]:',I_ref(4)/W2Au)
 !
 call set_real_printed_length()
 !
end subroutine RT_Rabi_Function

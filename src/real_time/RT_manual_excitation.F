!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
subroutine RT_manual_excitation(E,k)
 !
 use pars,           ONLY:SP,cI
 use units,          ONLY:BO2ANG
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp
 use fields,         ONLY:l_manual_excitation
 use SC,             ONLY:SC_bands,RT_ibz_coo,RT_Nk_ibz
 use real_time,      ONLY:G_lesser,Eh_pumped_pair_energy,Eh_pumped_pair_width,  &
&                         Eh_pumped_Nel
 use stderr,         ONLY:intc
 use electrons,      ONLY:levels,spin_occ
 use com,            ONLY:msg
 !
 implicit none
 !
 type(levels)      :: E
 type(bz_samp)     :: k
 !
 ! Work Space
 !
 integer :: i1,i2,i3,ib,ik,ik_rim,Pump_level(E%nb,E%nk)
 real(SP):: local_E_window,coeff,delta_Nel,occupation,local_energy(2)
 logical :: Pumped_state(E%nb,E%nk)
 !
 ! Possibility to pump by hand one or more electrons up
 !======================================================
 !
 if (.not.l_manual_excitation) return
 !
 ! Shift energy with respect to VBM/CBm
 !
 local_energy(1)=Eh_pumped_pair_energy(1)+E%E_VBM
 local_energy(2)=Eh_pumped_pair_energy(2)+E%E_CBm
 !
 if (abs(Eh_pumped_pair_energy(2)-Eh_pumped_pair_energy(1))<1.E-5.and.Eh_pumped_pair_energy(1)<0.) return
 !
 delta_Nel=0._SP
 coeff=0._SP
 Pumped_state=.FALSE.
 Pump_level=0
 i3=1
 local_E_window=Eh_pumped_pair_width/10.
 !
 do while(local_E_window<Eh_pumped_pair_width)
   !
   do ik=1,E%nk
     !
     i1=0
     !
     do i2=1,SC_bands(2)
       do ib=SC_bands(1),SC_bands(2)
         !
         if (Pumped_state(ib,ik)) cycle
         !
         if (i1==0) then
           if(abs(E%E(ib,ik,1)-local_energy(1))<local_E_window) i1=ib
         else
           if(abs(E%E(ib,ik,1)-local_energy(2))<local_E_window) then
             if(i1==ib) cycle
             Pumped_state(i1,ik)=.TRUE.
             Pumped_state(ib,ik)=.TRUE.
             Pump_level(ib,ik)= i3
             Pump_level(i1,ik)=-i3
             call msg('sr','[RT] Eh pair pumped `by hand` @ k'//trim(intc(ik))//&
&                 ' bands '//trim(intc(i1))//' - '//trim(intc(ib))//' (level '//trim(intc(i3))//')')
             i1=0
           endif
         endif
         !
       enddo
     enddo
     !
     do ib=SC_bands(1),SC_bands(2)
       if(Pumped_state(ib,ik).and.Pump_level(ib,ik)==i3) coeff=coeff+spin_occ*k%weights(ik)/ &
&                                                            sqrt(abs(real(Pump_level(ib,ik),SP)))
     enddo
     !
   enddo
   !
   i3=i3+1
   local_E_window=local_E_window+Eh_pumped_pair_width/10._SP
   if (all(Pumped_state(SC_bands(1):SC_bands(2),:))) local_E_window=Eh_pumped_pair_width*1.1_SP
   !
 enddo
 !
 coeff=coeff/Eh_pumped_Nel
 !
 do ik=1,E%nk
   do ib=SC_bands(1),SC_bands(2)
     !
     if (.not.Pumped_state(ib,ik)) cycle
     !
     occupation=min(1._SP,1._SP/sqrt(abs(real(Pump_level(ib,ik),SP)))/coeff)
     !
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       if (Pump_level(ib,ik)>0) then
         G_lesser(ib,ib,ik_rim,1)=cI*spin_occ*occupation
         delta_Nel=delta_Nel-cI*G_lesser(ib,ib,ik_rim,1)*k%weights(ik)*2._SP/real(RT_Nk_ibz(ik))
       else if (Pump_level(ib,ik)<0) then
         G_lesser(ib,ib,ik_rim,1)=cI*spin_occ*(1._SP-occupation)
       endif
     enddo
     !
   enddo   
 enddo
 !
 call msg('sr','[RT] Carrier number and density [none,cm-3]:',delta_Nel*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24)/))
 !
end subroutine RT_manual_excitation

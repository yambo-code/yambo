!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine RT_manual_excitation(E_RT,k,do_kpt,SUCCESS)
 !
 use pars,           ONLY:SP
 use units,          ONLY:BO2ANG
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp
 use stderr,         ONLY:intc
 use electrons,      ONLY:levels,spin_occ
 use com,            ONLY:msg
 use real_time,      ONLY:RT_carriers,RT_bands,RTibz
 use YPP_real_time,  ONLY:Eh_pumped_pair_energy,Eh_pumped_pair_width,&
&                         Eh_pumped_pair_deltaE,Eh_pumped_Nel
 !
 implicit none
 !
 type(levels),  intent(in) :: E_RT
 type(bz_samp), intent(in) :: k
 logical,       intent(in) :: do_kpt(RT_carriers%nk)
 logical,       intent(out):: SUCCESS
 !
 ! Work Space
 !
 integer      :: i_pump_level,ib,ibp,iv,ic,ik,i_RT,Pump_level(E_RT%nb,RT_carriers%nk)
 real(SP)     :: local_E_window,delta_Nel,occupation,local_energy(2),k_weights(RT_carriers%nk)
 logical      :: Pumped_state(E_RT%nb,RT_carriers%nk),RT_uses_Fine_Grid,STOP_the_LOOP,l_select_energy,l_select_deltaE
 !
 ! Possibility to pump by hand one or more electrons up
 !======================================================
 RT_uses_Fine_Grid=k%FGibz%N>0
 !
 ! Shift energy with respect to VBM/CBm
 !
 l_select_energy=abs(Eh_pumped_pair_energy(2)-Eh_pumped_pair_energy(1))>1.E-5_SP
 l_select_deltaE=    Eh_pumped_pair_deltaE>1.E-5
 !
 if (.not.(l_select_energy.or.l_select_deltaE)) return
 !
 local_energy(1)=Eh_pumped_pair_energy(1)+E_RT%E_VBM(1)
 local_energy(2)=Eh_pumped_pair_energy(2)+E_RT%E_CBm(1)
 !
 if (RT_uses_Fine_Grid) then
   k_weights=RT_carriers%k_weight
 else
   k_weights=k%weights
 endif
 !
 delta_Nel   =0._SP
 Pumped_state=.FALSE.
 Pump_level  =0
 i_pump_level=1
 local_E_window=Eh_pumped_pair_width/10._SP
 !
 do while(local_E_window<Eh_pumped_pair_width)
   !
   K_LOOP: do ik=1,E_RT%nk
     !
     if(.not.do_kpt(ik)) cycle
     !
     iv=0
     !
     do ibp=1,RT_bands(2)
       do ib=RT_bands(1),RT_bands(2)
         !
         if(l_select_energy) then
           !
           if (Pumped_state(ib,ik)) cycle
           !
           if(ib<iv) cycle
           !
           if (iv==0) then
             if(abs(E_RT%E(ib,ik,1)-local_energy(1))<local_E_window) iv=ib
           else
             if(abs(E_RT%E(ib,ik,1)-local_energy(2))<local_E_window) then
               ic=ib
               Pumped_state(ic,ik)=.TRUE.
               Pumped_state(iv,ik)=.TRUE.
               Pump_level(ic,ik)= i_pump_level
               Pump_level(iv,ik)=-i_pump_level
               call msg('sr','[RT] Eh pair pumped `by hand` @ k'//trim(intc(ik))//&
&                   ' bands '//trim(intc(iv))//' -> '//trim(intc(ic))//' (level '//trim(intc(i_pump_level))//')')
               iv=0
             endif
           endif
           !
         else if (l_select_deltaE) then
           !
           iv=ibp
           ic=ib
           !
           if (ic <=E_RT%nbf(1)) cycle
           if (iv > E_RT%nbm(1)) cycle
           !
           if (Pumped_state(iv,ik)) cycle
           if (Pumped_state(ic,ik)) cycle
           !
           if (ic<=iv) cycle
           !
           if(abs((E_RT%E(ic,ik,1)-E_RT%E(iv,ik,1))-Eh_pumped_pair_deltaE)<local_E_window) then
             Pumped_state(ic,ik)=.TRUE.
             Pumped_state(iv,ik)=.TRUE.
             Pump_level(ic,ik)= i_pump_level
             Pump_level(iv,ik)=-i_pump_level
             call msg('sr','[RT] Eh pair pumped `by hand` @ k'//trim(intc(ik))//&
&                 ' bands '//trim(intc(iv))//' -> '//trim(intc(ic))//' (level '//trim(intc(i_pump_level))//')')
           endif
           !
         endif
         !
       enddo
     enddo
     !
   enddo K_LOOP
   !
   i_pump_level=i_pump_level+1
   local_E_window=local_E_window+Eh_pumped_pair_width/10._SP
   if (all(Pumped_state(RT_bands(1):RT_bands(2),:))) local_E_window=Eh_pumped_pair_width*1.1_SP
   !
 enddo
 !
 SUCCESS=any(Pumped_state)
 if (.not.SUCCESS) return
 !
 RT_carriers%OCC_el%dN=0._SP
 ic=0
 STOP_the_LOOP=.FALSE.
 !
 do while(.not.STOP_the_LOOP)
   delta_Nel=0._SP
   ic=ic+1
   do i_RT=1,RT_carriers%nstates
     ib = RT_carriers%table(i_RT,1)
     ik = RT_carriers%table(i_RT,3)
     if (.not.Pumped_state(ib,ik)) cycle
     occupation=1._SP/sqrt(real(iabs(Pump_level(ib,ik)),SP))*real(ic)/50._SP
     if (occupation>1.) then
       STOP_the_LOOP=.TRUE.
       occupation=1.
     endif
     if (Pump_level(ib,ik)>0) then
       RT_carriers%OCC_el%dN(i_RT)= spin_occ*occupation
       delta_Nel=delta_Nel+RT_carriers%OCC_el%dN(i_RT)*2._SP*RTibz%weights(ik)
     else if (Pump_level(ib,ik)<0) then
       RT_carriers%OCC_el%dN(i_RT)=-spin_occ*occupation
     endif
   enddo
   if (.not.STOP_the_LOOP) STOP_the_LOOP=delta_Nel>Eh_pumped_Nel
 enddo 
 !
 call msg('sr','[RT] Carrier number  [USER]',Eh_pumped_Nel)
 call msg('sr','[RT]                 [GRID]',delta_Nel)
 call msg('sr','[RT]         density       ',delta_Nel/(DL_vol*BO2ANG**3*1.E-24_SP),"[cm-3]")
 !
end subroutine RT_manual_excitation

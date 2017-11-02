!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine RT_manual_excitation(E,k)
 !
 use pars,           ONLY:SP
 use units,          ONLY:BO2ANG
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp
 use stderr,         ONLY:intc
 use electrons,      ONLY:levels,spin_occ
 use com,            ONLY:msg
 use vec_operate,    ONLY:v_is_zero,c2a
 use real_time,      ONLY:RT_carriers,RT_bands,RT_Nk_ibz
 use YPP_real_time,  ONLY:Eh_pumped_pair_energy,Eh_pumped_pair_width,        &
&                         Eh_pumped_pair_BZ_reagions,Eh_pumped_pair_deltaE,  &
&                         Eh_pumped_pair_BZ_width,Eh_pumped_Nel,n_regions,   &
&                         l_RTpump_energy,l_RTpump_kspace
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k
 !
 ! Work Space
 !
 integer :: i_pump_level,i_region,ib,ibp,iv,ic,ik,i_RT,Pump_level(E%nb,E%nk)
 real(SP):: local_E_window,coeff,delta_Nel,occupation,local_energy(2),local_zero(3),local_k(3)
 logical :: Pumped_state(E%nb,E%nk),do_kpt(E%nk)
 !
 ! Possibility to pump by hand one or more electrons up
 !======================================================
 !
 ! Shift energy with respect to VBM/CBm
 !
 local_energy(1)=Eh_pumped_pair_energy(1)+E%E_VBM
 local_energy(2)=Eh_pumped_pair_energy(2)+E%E_CBm
 !
 if (l_RTpump_energy.and.abs(Eh_pumped_pair_energy(2)-Eh_pumped_pair_energy(1))<1.E-5_SP &
&                   .and.Eh_pumped_pair_energy(1)<0._SP) return
 !
 delta_Nel=0._SP
 coeff=0._SP
 Pumped_state=.FALSE.
 do_kpt=.FALSE.
 Pump_level=0
 i_pump_level=1
 local_E_window=Eh_pumped_pair_width/10._SP
 local_zero=Eh_pumped_pair_BZ_width
 !
 do while(local_E_window<Eh_pumped_pair_width)
   !
   do ik=1,E%nk
     !
     iv=0
     !
     if(l_RTpump_kspace) then
       call c2a(v_in=k%pt(ik,:),v_out=local_k,mode='ki2c')
       do i_region=1,n_regions
         if(.not. v_is_zero(local_k-Eh_pumped_pair_BZ_reagions(:,i_region),zero_=local_zero) ) cycle
         do_kpt(ik)=.TRUE.
       enddo
       if(.not.do_kpt(ik)) cycle
     endif
     !
     do ibp=1,RT_bands(2)
       do ib=RT_bands(1),RT_bands(2)
         !
         if(l_RTpump_energy) then
           !
           if (Pumped_state(ib,ik)) cycle
           !
           if(ib<iv) cycle
           !
           if (iv==0) then
             if(abs(E%E(ib,ik,1)-local_energy(1))<local_E_window) iv=ib
           else
             if(abs(E%E(ib,ik,1)-local_energy(2))<local_E_window) then
               ic=ib
               Pumped_state(ic,ik)=.TRUE.
               Pumped_state(iv,ik)=.TRUE.
               Pump_level(ic,ik)= i_pump_level
               Pump_level(iv,ik)=-i_pump_level
               call msg('sr','[RT] Eh pair pumped `by hand` @ k'//trim(intc(ik))//&
&                   ' bands '//trim(intc(iv))//' - '//trim(intc(ic))//' (level '//trim(intc(i_pump_level))//')')
               iv=0
             endif
           endif
           !
         else if(l_RTpump_kspace) then
           !
           iv=ibp
           ic=ib
           !
           if (Pumped_state(iv,ik)) cycle
           if (Pumped_state(ic,ik)) cycle
           !
           if(ic<=iv) cycle
           !
           if(abs((E%E(ic,ik,1)-E%E(iv,ik,1))-Eh_pumped_pair_deltaE)<local_E_window) then
             Pumped_state(ic,ik)=.TRUE.
             Pumped_state(iv,ik)=.TRUE.
             Pump_level(ic,ik)= i_pump_level
             Pump_level(iv,ik)=-i_pump_level
             call msg('sr','[RT] Eh pair pumped `by hand` @ k'//trim(intc(ik))//&
&                 ' bands '//trim(intc(iv))//' - '//trim(intc(ic))//' (level '//trim(intc(i_pump_level))//')')
           endif
           !
         endif
         !
       enddo
     enddo
     !
     do ib=RT_bands(1),RT_bands(2)
       if(Pumped_state(ib,ik).and.Pump_level(ib,ik)==i_pump_level) coeff=coeff+spin_occ*k%weights(ik)/ &
&                                                            sqrt(abs(real(Pump_level(ib,ik),SP)))
     enddo
     !
   enddo
   !
   i_pump_level=i_pump_level+1
   local_E_window=local_E_window+Eh_pumped_pair_width/10._SP
   if (all(Pumped_state(RT_bands(1):RT_bands(2),:))) local_E_window=Eh_pumped_pair_width*1.1_SP
   !
 enddo
 !
 coeff=coeff/Eh_pumped_Nel
 !
 RT_carriers%delta_f=0._SP
 !
 do i_RT=1,RT_carriers%nstates
   !
   ib     = RT_carriers%table(i_RT,1)
   ik     = RT_carriers%table(i_RT,2)
   !
   if (.not.Pumped_state(ib,ik)) cycle
   !
   occupation=min(1._SP,1._SP/sqrt(abs(real(Pump_level(ib,ik),SP)))/coeff)
   !
   if (Pump_level(ib,ik)>0) then
     RT_carriers%delta_f(i_RT)= spin_occ*occupation
     delta_Nel=delta_Nel+RT_carriers%delta_f(i_RT)*k%weights(ik)*2._SP/real(RT_Nk_ibz(ik),SP)
   else if (Pump_level(ib,ik)<0) then
     RT_carriers%delta_f(i_RT)=-spin_occ*occupation
   endif
   !
 enddo
 !
 call msg('sr','[RT] Carrier number and density [none,cm-3]:',delta_Nel*(/1._SP,1._SP/(DL_vol*BO2ANG**3*1.E-24_SP)/))
 !
end subroutine RT_manual_excitation

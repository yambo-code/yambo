!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_manual_excitation(E,k,SUCCESS)
 !
 use pars,           ONLY:SP
 use units,          ONLY:BO2ANG,HA2EV
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp,k_the_nearest
 use stderr,         ONLY:intc
 use electrons,      ONLY:levels,spin_occ,E_reset,E_duplicate,n_sp_pol
 use com,            ONLY:msg
 use real_time,      ONLY:RT_carriers,RT_bands,RTibz
 use YPPm,           ONLY:BANDS_path,CIRCUIT_k,CIRCUIT_k_label
 use YPP_interfaces, ONLY:k_special
 use YPP_real_time,  ONLY:Eh_pumped_pair_energy,Eh_pumped_pair_width,        &
&                         Eh_pumped_pair_BZ_regions,Eh_pumped_pair_deltaE,  &
&                         Eh_pumped_pair_BZ_width,Eh_pumped_Nel,Eh_pumped_pair_n_BZ_regions,   &
&                         l_RTpump_energy,l_RTpump_kspace
 !
#include<memory.h>
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k
 logical,       intent(out):: SUCCESS
 !
 ! Work Space
 !
 type(levels) :: E_local
 integer      :: i_pump_level,i_region,ib,ibp,iv,ic,ik,i_RT,Pump_level(E%nb,RT_carriers%nk),i_k_near,i_err
 real(SP)     :: local_E_window,delta_Nel,occupation,local_energy(2),local_k(3),&
&                k_dist(RT_carriers%nk),k_weights(RT_carriers%nk)
 logical      :: Pumped_state(E%nb,RT_carriers%nk),do_kpt(RT_carriers%nk),RT_uses_Fine_Grid,STOP_the_LOOP
 !
 ! Possibility to pump by hand one or more electrons up
 !======================================================
 do_kpt           =.FALSE.
 RT_uses_Fine_Grid=k%FGibz%N>0
 !
 ! Shift energy with respect to VBM/CBm
 !
 local_energy(1)=Eh_pumped_pair_energy(1)+E%E_VBM(1)
 local_energy(2)=Eh_pumped_pair_energy(2)+E%E_CBm(1)
 !
 if (l_RTpump_energy.and.abs(Eh_pumped_pair_energy(2)-Eh_pumped_pair_energy(1))<1.E-5_SP &
&                   .and.Eh_pumped_pair_energy(1)<0._SP) return
 !
 ! If I am using a FineGrid I need to fill a temporary E_local
 !
 call E_reset(E_local)
 !
 if (RT_uses_Fine_Grid) then
   E_local%nbf=E%nbf
   E_local%nbm=E%nbm
   E_local%nb=E%nb
   E_local%nk=RT_carriers%nk
   YAMBO_ALLOC(E_local%E,(E_local%nb,E_local%nk,n_sp_pol))
   do i_RT=1,RT_carriers%nstates
     ib     = RT_carriers%table(i_RT,1)
     ik     = RT_carriers%table(i_RT,3)
     k_weights(ik)     =RT_carriers%k_weight(ik)
     E_local%E(ib,ik,:)=RT_carriers%E_bare(i_RT)
   enddo
 else
   call E_duplicate(E,E_local)
   k_weights=k%weights
 endif
 !
 ! Use special points to select the region
 !
 if(l_RTpump_kspace) then
   !
   call k_special( )
   !
   if (len_trim(BANDS_path)>0) call k_circuit_driver( BANDS_path, 0 )
   !
   do i_region=1,Eh_pumped_pair_n_BZ_regions
     local_k=Eh_pumped_pair_BZ_regions(i_region,:)
     !
     i_k_near=k_the_nearest(local_k,RT_carriers%kpt,RT_carriers%nk,k_dist,ROTATE_k_ref=.TRUE.,i_err=i_err)
     !
     if (k_dist(i_k_near)<Eh_pumped_pair_BZ_width.or.Eh_pumped_pair_BZ_width<0) then
       do_kpt(i_k_near)=.TRUE. 
       call NEAR_k_report(i_k_near,local_k," ")
     endif
   enddo
   !
   do i_region=1,CIRCUIT_k%nbz
     !
     i_k_near=k_the_nearest(CIRCUIT_k%pt(i_region,:),RT_carriers%kpt,RT_carriers%nk,k_dist,ROTATE_k_ref=.TRUE.,i_err=i_err)
     !
     if (k_dist(i_k_near)<Eh_pumped_pair_BZ_width.or.Eh_pumped_pair_BZ_width<0) then
       do_kpt(i_k_near)=.TRUE. 
       call NEAR_k_report(i_k_near,CIRCUIT_k%pt(i_region,:),CIRCUIT_k_label(i_region))
     endif
   enddo
   !
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
   K_LOOP: do ik=1,E_local%nk
     !
     iv=0
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
             if(abs(E_local%E(ib,ik,1)-local_energy(1))<local_E_window) iv=ib
           else
             if(abs(E_local%E(ib,ik,1)-local_energy(2))<local_E_window) then
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
         else if (l_RTpump_kspace.and.do_kpt(ik)) then
           !
           iv=ibp
           ic=ib
           !
           if (ic <=E_local%nbf) cycle
           if (iv > E_local%nbm) cycle
           !
           if (Pumped_state(iv,ik)) cycle
           if (Pumped_state(ic,ik)) cycle
           !
           if (ic<=iv) cycle
           !
           if(abs((E_local%E(ic,ik,1)-E_local%E(iv,ik,1))-Eh_pumped_pair_deltaE)<local_E_window) then
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
 !
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
 call msg('sr','[RT]         density [cm-3]',delta_Nel/(DL_vol*BO2ANG**3*1.E-24_SP))
 !
 call E_reset(E_local)
 !
 contains
   !
   subroutine NEAR_k_report( ik, u_k, label )
     use pars,           ONLY:schlen
     integer           :: ik,i_c,i_v
     real(SP)          :: u_k(3)
     character(*)      :: label
     character(schlen) :: title
     call msg('s','User    defined',u_k,after_msg="[iku]"//" "//trim(label))
     call msg('s','Nearest k-point',RT_carriers%kpt(ik,:),after_msg="[iku][ik="//trim(intc(ik))//"]")
     call msg('s','Distance       ',k_dist(ik),after_msg="[iku]")
     do i_c=E_local%nbf+1,E_local%nbf+2
       do i_v=E_local%nbf-1,E_local%nbf
         title="Gap CBm+"//trim(intc(i_c-E_local%nbf))//" VBM-"//trim(intc(E_local%nbf-i_v))
         call msg('s',trim(title),(E_local%E(i_c,ik,1)-E_local%E(i_v,ik,1))*HA2EV,after_msg="[ev]")
       enddo 
     enddo
   end subroutine NEAR_k_report
   !
end subroutine RT_manual_excitation

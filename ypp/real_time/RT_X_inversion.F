!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG
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
subroutine RT_X_inversion(RT_P_Probe,Susceptibility,Harmonic_Frequency,X_effective)
 !
 use pars,           ONLY:SP,pi
 use YPP_real_time,  ONLY:N_probes,X_order,N_probe_frequencies,RT_conf,i_t_start,&
&                         i_t_end,N_pumps
 use fields,         ONLY:EtoT,Efield,Divide_by_Field
 use real_time,      ONLY:RT_P
 use units,          ONLY:AUT2FS
 use com,            ONLY:msg
 use LIVE_t,         ONLY:live_timing
#include<memory.h>
 !
 real(SP)      :: RT_P_probe(RT_conf%T_n_steps,N_probes,3),Harmonic_Frequency(0:X_order,N_probe_frequencies)
 complex(SP)   :: Susceptibility(0:X_order,N_probe_frequencies),X_effective(0:X_order,N_probe_frequencies,3)
 !
 ! Work Space
 !
 real(SP)          :: T_period,time_range_save(2),T_period_max
 logical           :: use_LRR
 integer           :: i_c,i_p,i_f,i_order,i_Probe
 real(SP),    allocatable :: X_w(:)
 !
 call section("=",'Response function(s) by Inversion')
 !====================================================
 !
 !DEBUG>
 !case with N_probe_frequencies> 1 still experimental
 !DEBUG<
 !use_LRR=N_probe_frequencies>1.and.N_probes==1
 use_LRR=.FALSE.
 !
 YAMBO_ALLOC(X_w,(X_order+1))
 !
 time_range_save=RT_conf%T_range
 !
 if (.not.use_LRR) then
   !=======================================
   ! 1 OR SEVERAL PROBES/ 1 FREQUENCY each
   !=======================================
   ! 
   T_period_max=-1.
   !
   do i_p=1,N_probes
     !
     if (N_pumps==1) T_period=T_range(0,0)
     if (N_pumps==0) T_period=T_range(1,i_p)
     !
     if (T_period>T_period_max) T_period_max=T_period
     !
   enddo
   !
   call msg('s','[RT] Effective max time period [fs]:',T_period_max*AUT2FS)
   !
   call live_timing('X effective :',N_probes)
   !
   do i_p=1,N_probes
     !
     if (N_pumps==1) T_period=T_range(0,0)
     if (N_pumps==0) T_period=T_range(1,i_p)
     !
     do i_c = 1,3
       !
       call RT_coefficients_Inversion(X_order+1,X_order+1,RT_P_probe(:,i_p,i_c),X_effective(0:,i_p,i_c),&
&                                     Harmonic_Frequency(0:,i_p),T_period,use_LRR)
       !
     end do
     !
     call live_timing(steps=1)
     !
   enddo
   !
   call live_timing()
   !
   if (N_probes==1) then
     !
     do i_p=1,N_probes
       !
       X_w(:)=Harmonic_Frequency(0:,1)
       !
       call RT_split_Polarization(X_order+1,RT_conf%T_n_steps,X_w,X_effective(:,i_p,:),RT_P_probe(:,i_p,:),"dump")
       !
     enddo
     !
   endif
   !
 else
   !
   !===============================
   ! 1 PROBE/ SEVERAL FREQUENCIES
   !===============================
   !
   T_period=T_range(1,0)
   !
   call msg('s','[RT] Time range [fs]:',(/real(i_t_start),real(i_t_end)/)*RT_conf%delta_T*AUT2FS)
   !
   do i_c = 1,3
     !
     call RT_coefficients_Inversion(N_probe_frequencies,N_probe_frequencies,&
&                                   RT_P_probe(:,1,i_c),X_effective(1,:,i_c),Harmonic_Frequency(1,:),T_period,use_LRR)
     !
   end do
   !
 endif
 !
 i_Probe=2
 !
 do i_order=0,X_order
   !
   do i_f=1,N_probe_frequencies
     !
     Susceptibility(i_order,i_f)=4.*pi*dot_product( Efield(i_Probe)%versor(:),X_effective(i_order,i_f,:) )*&
&                                                   Divide_by_Field(Efield(i_Probe),i_order)
     !
   enddo
   ! 
 enddo
 !
 YAMBO_FREE(X_w)
 !
 contains 
   !
   real(SP) function T_range(i_n,i_f)
     !
     integer :: i_n,i_f
     !
     RT_conf%T_range=time_range_save
     !
     if (i_f>  0) T_range = EtoT(E=Harmonic_Frequency(i_n,i_f))
     if (i_f== 0) T_range = EtoT(E=maxval(Harmonic_Frequency(:,:)))
     !
     if (i_f==-1) then
       T_range = RT_conf%T_range(2)- RT_conf%T_range(1)
       return
     endif
     !
     RT_conf%T_range(2)  = RT_conf%T_range(1)+ T_range
     !
     if (RT_conf%T_range(2)> RT_conf%T_n_steps*RT_conf%delta_T) then
       RT_conf%T_range(2)  = RT_conf%T_n_steps*RT_conf%delta_T
       RT_conf%T_range(1)  = RT_conf%T_range(2)-T_range
     endif 
     !
     i_t_start=nint(RT_conf%T_range(1)/RT_conf%delta_T)+1
     i_t_end  =nint(RT_conf%T_range(2)/RT_conf%delta_T)
     !
   end function 
   !
end subroutine RT_X_inversion

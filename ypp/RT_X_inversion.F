!
! Copyright (C) 2000-2011 M. Gruning, A. Marini and the YAMBO team 
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
subroutine RT_X_inversion(RT_P_Probe,Susceptibility,Probe_Frequency)
 !
 use pars,           ONLY:SP,pi
 use YPP,            ONLY:N_probes,X_order,N_probe_frequencies,X_rt,RT_initialize,i_t_start,i_t_end
 use fields,         ONLY:EtoT,Efield,Divide_by_Field,i_Probe
 use real_time,      ONLY:NE_steps,RT_step
 use units,          ONLY:FS2AUT
 use com,            ONLY:msg
 use timing,         ONLY:live_timing
 implicit none
 !
 real(SP)      :: RT_P_probe(NE_steps,N_probes,3),Probe_Frequency(N_probe_frequencies)
 complex(SP)   :: Susceptibility(X_order+1,N_probe_frequencies)
 !
 ! Work Space
 !
 real(SP)          :: T_period,time_range_save(2)
 integer           :: i_c,i_p,i_f,i_order
 complex(SP), allocatable :: X_effective(:,:,:)
 !
 call section("=",'Response function(s) by Inversion')
 !====================================================
 !
 allocate(X_effective(X_order+1,N_probe_frequencies,3))
 !
 X_effective=(0.,0.)
 !
 time_range_save=X_rt%time_range
 !
 if (N_probes>1) then
   !==================================
   ! SEVERAL PROBES/ 1 FREQUENCY each
   !==================================
   !
   call live_timing('X effective :',N_probes)
   !
   do i_p=1,N_probes
     !
     T_period=T_range(i_p)
     !
     !call msg('s','[RT] Time range [fs]:',(/real(i_t_start),real(i_t_end)/)*RT_step/FS2AUT)
     !
     do i_c = 1,3
       !
       call RT_coefficients_Inversion(1,X_order+1,RT_P_probe(:,i_p,i_c),X_effective(:,i_p,i_c),&
&                                     (/Probe_Frequency(i_p)/),T_period)
       !
     end do
     !
     call live_timing(steps=1)
     !
   enddo
   !
   call live_timing()
   !
 else
   !
   !===============================
   ! 1 PROBE/ SEVERAL FREQUENCIES
   !===============================
   !
   T_period=T_range(-1)
   !
   call msg('s','[RT] Time range [fs]:',(/real(i_t_start),real(i_t_end)/)*RT_step/FS2AUT)
   !
   do i_c = 1,3
     !
     call RT_coefficients_Inversion(N_probe_frequencies,N_probe_frequencies,&
&                                   RT_P_probe(:,1,i_c),X_effective(2,:,i_c),Probe_Frequency,T_period)
     !
   end do
   !
 endif
 !
 do i_order=0,X_order
   !
   do i_f=1,N_probe_frequencies
     !
     Susceptibility(i_order+1,i_f)=4.*pi*dot_product( Efield(i_Probe)%versor(:),X_effective(i_order+1,i_f,:) )*&
&                                                     Divide_by_Field(Efield(i_Probe),i_order)
     !
   enddo
   ! 
 enddo
 !
 deallocate(X_effective)
 !
 contains 
   !
   real(SP) function T_range(i_f)
     !
     integer :: i_f
     !
     X_rt%time_range=time_range_save
     !
     if (i_f>  0) T_range = EtoT(E=Probe_Frequency(i_p))
     if (i_f== 0) T_range = EtoT(E=minval(Probe_Frequency))
     if (i_f==-1) then
       T_range = X_rt%time_range(2)- X_rt%time_range(1)
       return
     endif
     !
     X_rt%time_range(2)  = X_rt%time_range(1)+ T_range
     !
     if (X_rt%time_range(2)> NE_steps*RT_step) then
       X_rt%time_range(2)  = NE_steps*RT_step
       X_rt%time_range(1)  = X_rt%time_range(2)-T_range
     endif 
     !
     i_t_start=int(X_rt%time_range(1)/RT_step)+1
     i_t_end  =int(X_rt%time_range(2)/RT_step)
     !
   end function 
   !
end subroutine RT_X_inversion

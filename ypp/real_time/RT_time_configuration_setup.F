!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine RT_time_configuration_setup(what,GENERAL_RT_IO_t)
 !
 use units,          ONLY:AUT2FS,pi,HA2EV
 use YPP_real_time,  ONLY:RT_conf,RT_time
 use real_time,      ONLY:G_mem_steps
 use pars,           ONLY:SP
 use com,            ONLY:msg
 use real_time,      ONLY:RT_step
 use RT_control,     ONLY:TIME_adjust,RT_IO_t
 !
#include<memory.h>
 !
 character(*),  intent(in) :: what
 type(RT_IO_t), intent(in) :: GENERAL_RT_IO_t
 !
 ! Work Space
 !
 integer  :: i_t,i_ref(2),i_c,j_ref,step_ratio
 logical  :: UNIFORM,l_carriers,l_current,l_G_lesser,l_rho_f,l_rho_G
 real(SP) :: STEP,TIME,TIME_diff,REF_diff(2)
 integer, allocatable :: i_table(:)
 !
 ! Definitions
 !==============
 !
 l_carriers=(what=="CARRIERS")
 l_current =(what=="CURRENT")
 l_G_lesser=(what=="Glesser")
 l_rho_G   =(what=="G-DENSITY")
 l_rho_f   =(what=="f-DENSITY")
 !
 TIME  = GENERAL_RT_IO_t%Time(GENERAL_RT_IO_t%N)
 STEP  = RT_step
 if (GENERAL_RT_IO_t%N> 1) then
   STEP  = (GENERAL_RT_IO_t%Time(GENERAL_RT_IO_t%N)-GENERAL_RT_IO_t%Time(GENERAL_RT_IO_t%N-1))
   if (l_rho_G .or. l_G_lesser) then
    TIME_diff = (GENERAL_RT_IO_t%Time(GENERAL_RT_IO_t%N)-GENERAL_RT_IO_t%Time(GENERAL_RT_IO_t%N-G_MEM_steps))
    if( .not. abs(TIME_diff-STEP*G_MEM_steps)<1.E-5) STEP=TIME_diff
   endif
 endif
 !
 ! Time range ...
 !===============
 !
 if (any(RT_conf%T_range>0._SP)) then
   !
   RT_conf%T_range(1)=TIME_adjust( RT_conf%T_range(1), STEP )
   RT_conf%T_range(2)=TIME_adjust( RT_conf%T_range(2), STEP )
   !
   i_ref      = 0
   REF_diff   = STEP
   !
   do i_t=1,GENERAL_RT_IO_t%N-1
     !
     TIME_diff=abs(GENERAL_RT_IO_t%Time(i_t)-RT_conf%T_range(1))
     if ( TIME_diff<REF_diff(1) ) then
       REF_diff(1)=TIME_diff
       i_ref(1) = i_t
     endif
     TIME_diff=abs(GENERAL_RT_IO_t%Time(i_t+1)-RT_conf%T_range(2))
     if ( TIME_diff<REF_diff(2) ) then
       REF_diff(2)=TIME_diff
       i_ref(2) = i_t+1
     endif
     !
   enddo
   !
   if (i_ref(1)==0) i_ref(1)=1
   if (i_ref(2)==0) i_ref(2)=GENERAL_RT_IO_t%N
   !
 else
   !
   i_ref           = (/1,GENERAL_RT_IO_t%N/)
   !
 endif
 !
 RT_conf%T_range=(/GENERAL_RT_IO_t%Time(i_ref(1)),GENERAL_RT_IO_t%Time(i_ref(2))/)
 !
 ! Uniform time sampling?
 !========================
 !
 UNIFORM=.TRUE.
 do i_t=1,GENERAL_RT_IO_t%N-1
   TIME_diff= abs ( GENERAL_RT_IO_t%Time(i_t+1)-GENERAL_RT_IO_t%Time(i_t) - STEP )
   if (TIME_diff>RT_step) UNIFORM=.FALSE.
 enddo
 !
 ! Time step ...
 !============== 
 !
 if (RT_conf%delta_T> 0._SP) then
   if (GENERAL_RT_IO_t%N>1) then
     RT_conf%delta_T=max(TIME_adjust(RT_conf%delta_T,STEP),STEP)
     REF_diff(1)= STEP
     YAMBO_ALLOC(i_table,(GENERAL_RT_IO_t%N))
     i_table=0
     i_table(1)=i_ref(1)
     !
     j_ref=i_ref(1)
     !
     RT_conf%T_n_steps=1
     do i_t=i_ref(1)+1,i_ref(2)-1
       TIME_diff= abs ( (GENERAL_RT_IO_t%Time(j_ref)+RT_conf%delta_T) -GENERAL_RT_IO_t%Time(i_t) )
       if (abs(TIME_diff)<REF_DIFF(1) ) then
         REF_diff(1)=TIME_diff
         i_table(RT_conf%T_n_steps+1)=i_t
       endif
       TIME_diff= abs ( (GENERAL_RT_IO_t%Time(j_ref)+RT_conf%delta_T) -GENERAL_RT_IO_t%Time(i_t+1) )
       if ( abs(TIME_diff)>REF_diff(1) .and. i_table(RT_conf%T_n_steps+1)>0 ) then
         REF_diff(1)= GENERAL_RT_IO_t%Time(i_t+1)-GENERAL_RT_IO_t%Time(i_t-1)
         RT_conf%T_n_steps=RT_conf%T_n_steps+1
         j_ref= i_t
       endif
     enddo
     i_table(RT_conf%T_n_steps+1)=i_ref(2)
     RT_conf%T_n_steps=RT_conf%T_n_steps+1
     !
     YAMBO_ALLOC(RT_time,(RT_conf%T_n_steps))
     !
     do i_t=1,RT_conf%T_n_steps
       RT_time(i_t)=GENERAL_RT_IO_t%Time(i_table(i_t))
     enddo
     !
     YAMBO_FREE(i_table)
     !
   else
     RT_conf%T_n_steps=1
     YAMBO_ALLOC(RT_time,(RT_conf%T_n_steps))
     RT_time=0._SP
     RT_conf%delta_T=0._SP
   endif
 else
   RT_conf%T_n_steps = GENERAL_RT_IO_t%N
   YAMBO_ALLOC(RT_time,(RT_conf%T_n_steps))
   RT_time=GENERAL_RT_IO_t%Time
   RT_conf%delta_T   = STEP
 endif
 !
 ! Messaging
 !===========
 !
 call msg('s','['//what//'] Uniform grid       :',UNIFORM)
 call msg('s','['//what//'] Time range     [fs]:',RT_conf%T_range*AUT2FS)
 call msg('s','['//what//'] Time step      [fs]:',RT_conf%delta_T*AUT2FS)
 call msg('s','['//what//'] Time steps         :',RT_conf%T_n_steps)
 !
 if (.not.(l_current.or.l_G_lesser)) return
 !
 call msg('s','['//what//'] Damping       [meV]:',RT_conf%damp_factor*1000._SP*HA2EV)
 !
 if (.not.l_current) return
 !
 ! Field deduced dumping
 !=======================
 RT_conf%Field_damp_factor= 2._SP*pi/(real(RT_conf%T_n_steps-1,SP)*RT_conf%delta_T)
 call msg('s','['//what//'] Field damping [meV]:',RT_conf%Field_damp_factor*1000._SP*HA2EV)
 !
end subroutine RT_time_configuration_setup

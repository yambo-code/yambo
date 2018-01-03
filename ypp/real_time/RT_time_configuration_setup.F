!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 use pars,           ONLY:SP
 use com,            ONLY:msg
 use real_time,      ONLY:RT_step
 use RT_control,     ONLY:TIME_adjust,RT_IO_t
 !
#include<memory.h>
 !
 character(*) :: what 
 type(RT_IO_t):: GENERAL_RT_IO_t
 !
 ! Work Space
 ! 
 !
 integer  :: i_t,i_ref(2),i_range(2),i_io(2),i_c,j_ref
 logical  :: UNIFORM
 real(SP) :: STEP,TIME,TIME_diff
 !
 if (GENERAL_RT_IO_t%N> 1) STEP  = (GENERAL_RT_IO_t%Time(GENERAL_RT_IO_t%N)-GENERAL_RT_IO_t%Time(GENERAL_RT_IO_t%N-1))
 if (GENERAL_RT_IO_t%N==1) STEP  = 1._SP
 STEP  = max(TIME_adjust(STEP),STEP)
 TIME  = TIME_adjust(GENERAL_RT_IO_t%Time(GENERAL_RT_IO_t%N))
 !
 ! Time range ...
 !===============
 if (any(RT_conf%T_range>0._SP)) then
   !
   if  (RT_conf%T_range(1) < 0)  RT_conf%T_range(1) = GENERAL_RT_IO_t%Time(1)
   if  (RT_conf%T_range(2) < 0)  RT_conf%T_range(2) = TIME
   RT_conf%T_range(1)=TIME_adjust( RT_conf%T_range(1) )
   RT_conf%T_range(2)=TIME_adjust( RT_conf%T_range(2) )
   ! 
   i_range(:) = nint( RT_conf%T_range(:)/RT_step )
   i_ref      = 0
   !
   do i_t=1,GENERAL_RT_IO_t%N-1
     !
     i_io(1) = nint( GENERAL_RT_IO_t%Time(i_t)/RT_step )
     i_io(2) = nint( GENERAL_RT_IO_t%Time(i_t+1)/RT_step )
     !
     do i_c=1,2
       if (i_range(i_c)==i_io(1)) then
         i_ref(i_c)=i_t
       else if (i_range(i_c)==i_io(2)) then
         i_ref(i_c)=i_t+1
       else if (i_range(i_c)>i_io(1).and.i_range(i_c)<i_io(2)) then
         if (i_range(i_c)-i_io(1)< i_io(2)-i_range(i_c)) i_ref(i_c)=i_t
         if (i_range(i_c)-i_io(1)>=i_io(2)-i_range(i_c)) i_ref(i_c)=i_t+1
       endif
     enddo
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
 if (GENERAL_RT_IO_t%N==nint(TIME/STEP)+1) UNIFORM=.TRUE.
 if (GENERAL_RT_IO_t%N/=nint(TIME/STEP)+1) UNIFORM=.FALSE.
 !
 ! Time step ...
 !
 ! N.B.: This is needed for the carriers post-processing only
 !============================================================
 !
 if ( (RT_conf%delta_T>0._SP.or..not.UNIFORM) .and. what/="CURRENT") then
   !
   if (UNIFORM) then
     RT_conf%delta_T=max(TIME_adjust(RT_conf%delta_T,STEP),STEP)
   else 
     !
     STEP            = GENERAL_RT_IO_t%Time(i_ref(2))-GENERAL_RT_IO_t%Time(i_ref(2)-1)
     RT_conf%delta_T = nint( RT_conf%delta_T/STEP )*STEP
     if ( nint( RT_conf%delta_T/STEP ) ==0 ) RT_conf%delta_T = STEP
     !
   endif
   !
   do i_c=1,2
     !
     RT_conf%T_n_steps=1
     !
     if (i_c==1) then
       !
       j_ref=i_ref(2)
       !
       do i_t=i_ref(2)-1,i_ref(1),-1
         TIME_diff=GENERAL_RT_IO_t%Time(j_ref)-GENERAL_RT_IO_t%Time(i_t)
         i_range(1) = nint(TIME_diff/RT_step)
         if (i_range(1) == nint(RT_conf%delta_T/RT_step)) then
           RT_conf%T_n_steps=RT_conf%T_n_steps+1
           j_ref            = i_t
         endif
       enddo
       !
       YAMBO_ALLOC(RT_time,(RT_conf%T_n_steps))
       !
       i_ref(1)          =j_ref
       RT_conf%T_range(1)=GENERAL_RT_IO_t%Time(i_ref(1))
       !
     else
       !
       j_ref            =i_ref(1)
       RT_time(1)       =GENERAL_RT_IO_t%Time(i_ref(1))
       !
       do i_t=i_ref(1)+1,i_ref(2)
         TIME_diff=GENERAL_RT_IO_t%Time(i_t)-GENERAL_RT_IO_t%Time(j_ref)
         i_range(1) = nint(TIME_diff/RT_step)
         if (i_range(1) == nint(RT_conf%delta_T/RT_step)) then
           RT_conf%T_n_steps=RT_conf%T_n_steps+1
           j_ref            = i_t
           RT_time(RT_conf%T_n_steps)=GENERAL_RT_IO_t%Time(i_t)
         endif
       enddo
       !
     endif
     !
   enddo
   !
 else
   !
   RT_conf%delta_T   = STEP
   RT_conf%T_n_steps = i_ref(2)-i_ref(1)+1
   !
   YAMBO_ALLOC(RT_time,(RT_conf%T_n_steps))
   do i_t=i_ref(1),i_ref(2)
     RT_time(i_t-i_ref(1)+1) = GENERAL_RT_IO_t%Time(i_t)
   enddo
   !
 endif
 !
 ! Field deduced dumping
 !=======================
 RT_conf%Field_damp_factor= 2._SP*pi/(real(RT_conf%T_n_steps-1,SP)*RT_conf%delta_T)
 !
 ! Messaging
 !===========
 !
 call msg('s','['//what//'] Uniform grid       :',UNIFORM)
 call msg('s','['//what//'] Time range     [fs]:',RT_conf%T_range*AUT2FS)
 call msg('s','['//what//'] Time step      [fs]:',RT_conf%delta_T*AUT2FS)
 call msg('s','['//what//'] Time steps         :',RT_conf%T_n_steps)
 !
 if (what/="CURRENT") return
 !
 call msg('s','['//what//'] Damping       [meV]:',RT_conf%damp_factor*1000._SP*HA2EV)
 call msg('s','['//what//'] Field damping [meV]:',RT_conf%Field_damp_factor*1000._SP*HA2EV)
 !
end subroutine RT_time_configuration_setup

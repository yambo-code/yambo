!
!        Copyright (C) 2000-2020 the YAMBO team
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
 integer  :: i_t,i_ref(2),i_range(2),i_io(2),i_c,j_ref,step_ratio
 logical  :: UNIFORM,l_carriers,l_current,l_G_lesser
 real(SP) :: STEP,TIME,TIME_diff
 !
 !call RT_time_configuration_setup_NEW(what,GENERAL_RT_IO_t)
 !return
 !
 ! Definitions
 !==============
 !
 TIME  = GENERAL_RT_IO_t%Time(GENERAL_RT_IO_t%N)
 STEP  = RT_step
 if (GENERAL_RT_IO_t%N> 1) STEP  = (GENERAL_RT_IO_t%Time(GENERAL_RT_IO_t%N)-GENERAL_RT_IO_t%Time(GENERAL_RT_IO_t%N-1))
 !
 l_carriers=(what=="CARRIERS")
 l_current =(what=="CURRENT")
 l_G_lesser=(what=="Glesser")
 !
 ! Time range ...
 !===============
 !
 if (any(RT_conf%T_range>0._SP)) then
   !
   RT_conf%T_range(1)=TIME_adjust( RT_conf%T_range(1), STEP )
   RT_conf%T_range(2)=TIME_adjust( RT_conf%T_range(2), STEP )
   !
   i_range(:) = nint( RT_conf%T_range(:)/STEP )
   i_ref      = 0
   !
   do i_t=1,GENERAL_RT_IO_t%N-1
     !
     i_io(1) = nint( GENERAL_RT_IO_t%Time(i_t)/STEP )
     i_io(2) = nint( GENERAL_RT_IO_t%Time(i_t+1)/STEP )
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
 !
 ! N.B.: This is needed for the carriers post-processing only
 !============================================================
 !
 if ( (RT_conf%delta_T>0._SP .or. .not.UNIFORM) .and. l_carriers ) then
   !
   if (.not.UNIFORM) STEP = GENERAL_RT_IO_t%Time(i_ref(2))-GENERAL_RT_IO_t%Time(i_ref(2)-1)
   !
   RT_conf%delta_T=max(TIME_adjust(RT_conf%delta_T,STEP),STEP)
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
         TIME_diff= abs ( GENERAL_RT_IO_t%Time(j_ref)-GENERAL_RT_IO_t%Time(i_t) - RT_conf%delta_T )
         ! Here it is checked against zero, a treshold should be given instead
         if (TIME_diff-RT_step<0._SP) then
           RT_conf%T_n_steps=RT_conf%T_n_steps+1
           j_ref            = i_t
         endif
       enddo
       !
       YAMBO_ALLOC(RT_time,(RT_conf%T_n_steps))
       !
       i_ref(1)          =j_ref
       !
     else
       !
       j_ref            =i_ref(1)
       RT_time(1)       =GENERAL_RT_IO_t%Time(i_ref(1))
       !
       do i_t=i_ref(1)+1,i_ref(2)
         TIME_diff= abs ( GENERAL_RT_IO_t%Time(i_t)-GENERAL_RT_IO_t%Time(j_ref) - RT_conf%delta_T )
         if (TIME_diff<RT_step) then
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
   if(RT_conf%delta_T==0._SP) RT_conf%delta_T   = STEP
   if(RT_conf%delta_T> 0._SP) RT_conf%delta_T   = max(TIME_adjust(RT_conf%delta_T,STEP),STEP)
   !
   step_ratio=nint(RT_conf%delta_T/STEP)
   do i_c=1,step_ratio
     if(mod(i_ref(2)-i_ref(1),step_ratio)==0) exit
     i_ref(2)=i_ref(2)-i_c
   enddo
   RT_conf%T_n_steps = (i_ref(2)-i_ref(1))/step_ratio+1
   !
   YAMBO_ALLOC(RT_time,(RT_conf%T_n_steps))
   j_ref=0
   do i_t=i_ref(1),i_ref(2)
     if(mod(i_t-1,step_ratio)/=0) cycle
     j_ref=j_ref+1
     RT_time(j_ref) = GENERAL_RT_IO_t%Time(i_t)
   enddo
   !
 endif
 !
 RT_conf%T_range=(/GENERAL_RT_IO_t%Time(i_ref(1)),GENERAL_RT_IO_t%Time(i_ref(2))/)
 !
 ! Messaging
 !===========
 !
 call msg('s','['//what//'] Uniform grid       ',UNIFORM)
 call msg('s','['//what//'] Time range     [fs]',RT_conf%T_range*AUT2FS)
 call msg('s','['//what//'] Time step      [fs]',RT_conf%delta_T*AUT2FS)
 call msg('s','['//what//'] Time steps         ',RT_conf%T_n_steps)
 !
 if (.not.(l_current.or.l_G_lesser)) return
 !
 call msg('s','['//what//'] Damping       [meV]',RT_conf%damp_factor*1000._SP*HA2EV)
 !
 if (.not.l_current) return
 !
 ! Field deduced dumping
 !=======================
 RT_conf%Field_damp_factor= 2._SP*pi/(real(RT_conf%T_n_steps-1,SP)*RT_conf%delta_T)
 call msg('s','['//what//'] Field damping [meV]',RT_conf%Field_damp_factor*1000._SP*HA2EV)
 !
end subroutine RT_time_configuration_setup

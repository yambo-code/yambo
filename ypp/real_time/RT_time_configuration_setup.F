!
!        Copyright (C) 2000-2016 the YAMBO team
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
!
subroutine RT_time_configuration_setup(what,TIME_step,TIME_range,N_times)
 !
 use units,          ONLY:AUT2FS
 use pars,           ONLY:SP
 use com,            ONLY:msg
 use real_time,      ONLY:RT_step
 use RT_control,     ONLY:JPC_RT_IO_t
 implicit none
 !
 character(*) :: what 
 real(SP)     :: TIME_step,TIME_range(2)
 integer      :: N_times
 !
 ! Work Space
 ! 
 real(SP) :: STEP
 !
 STEP        =JPC_RT_IO_t%Time(JPC_RT_IO_t%N)-JPC_RT_IO_t%Time(JPC_RT_IO_t%N-1)
 !
 if (what=="CURRENT") then
   !
   N_times     =JPC_RT_IO_t%N
   !
   TIME_range  = (/0._SP,JPC_RT_IO_t%Time(JPC_RT_IO_t%N)/)
   !
   TIME_step   = STEP
   !
 endif
!   !
!   if (nint(NE_time/RT_step)+1/=NE_steps) then
!     call warning(" Interrupted DB found. Time steps reduced.")
!     NE_steps=nint(NE_time/RT_step)+1
!   endif
!   !
!   N_times      = (NE_steps-1)/JPC_RT_IO_t%INTERVAL_steps+1
!   TIME_step    = RT_step*real(JPC_RT_IO_t%INTERVAL_steps,SP)
!   TIME_range   = (/0._SP,real(N_times-1,SP)*TIME_step/)
!   !
! endif
 !
 if (what=="CARRIERS") then
   !
   if (TIME_step<STEP) then
     ! 
     TIME_step=STEP
     ! 
   else
     !
     TIME_step=nint(TIME_step/STEP)*STEP
     !
   endif
   !
   N_times=nint( (TIME_range(2)-TIME_range(1))/TIME_step )+1
   !
   if (N_times<0) return
   !
   if (JPC_RT_IO_t%Time(JPC_RT_IO_t%N)<TIME_range(2)) then
     TIME_range(2)=JPC_RT_IO_t%Time(JPC_RT_IO_t%N)
     if(TIME_step>JPC_RT_IO_t%Time(JPC_RT_IO_t%N)) TIME_step=JPC_RT_IO_t%Time(JPC_RT_IO_t%N)
     N_times=nint(TIME_range(2)/TIME_step)+1
   endif
   !
 endif
 !
 JPC_RT_IO_t%INTERVAL_steps=TIME_step/RT_step
 !
 call msg('s','['//what//'] Time range [fs]:',TIME_range*AUT2FS)
 call msg('s','['//what//'] Time step  [fs]:',TIME_step*AUT2FS)
 call msg('s','['//what//'] Time steps     :',N_times)
 !
end subroutine RT_time_configuration_setup

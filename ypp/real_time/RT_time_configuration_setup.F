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
subroutine RT_time_configuration_setup(what,TIME_step,TIME_range,N_times)
 !
 use units,          ONLY:AUT2FS
 use pars,           ONLY:SP
 use com,            ONLY:msg,error
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
 integer  :: i_t,i_cycle
 real(SP) :: STEP,TIME,OFF_set
 logical  :: FOUND
 !
 STEP  = (JPC_RT_IO_t%Time(JPC_RT_IO_t%N)-JPC_RT_IO_t%Time(JPC_RT_IO_t%N-1))
 STEP  = nint(STEP/RT_step)*RT_step
 TIME  = nint(JPC_RT_IO_t%Time(JPC_RT_IO_t%N)/RT_step)*RT_step
 !
 i_cycle=0
 !
 loop: do while (TIME>0)
   FOUND = .FALSE.
   do i_t=1,JPC_RT_IO_t%N
     if (nint( (JPC_RT_IO_t%Time(i_t)-TIME)/RT_step) == 0 ) FOUND=.TRUE.
   enddo
   if (.not.FOUND) then
     if (i_cycle==5) call error('Impossible to find a compatibile and constant I/O Time step')
     STEP=STEP*2._SP
     TIME  = nint(JPC_RT_IO_t%Time(JPC_RT_IO_t%N)/RT_step)*RT_step
     i_cycle=i_cycle+1
     cycle loop
   endif
   TIME=TIME-STEP
 enddo loop
 !
 OFF_set = nint((TIME+STEP)/RT_step)*RT_step
 TIME    = nint(JPC_RT_IO_t%Time(JPC_RT_IO_t%N)/RT_step)*RT_step
 !
 if (what=="CURRENT") then
   !
   TIME_range  = (/OFF_set,TIME/)
   !
   TIME_step   = STEP
   !
 endif
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
   TIME_range(1) = nint( (TIME_range(1)-OFF_set)/TIME_step)*TIME_step+OFF_set
   TIME_range(2) = nint( (TIME_range(2)-OFF_set)/TIME_step)*TIME_step+OFF_set
   !
 endif
 !
 N_times=nint( (TIME_range(2)-TIME_range(1))/TIME_step )+1
 !
 call msg('s','['//what//'] Time range [fs]:',TIME_range*AUT2FS)
 call msg('s','['//what//'] Time step  [fs]:',TIME_step*AUT2FS)
 call msg('s','['//what//'] Time steps     :',N_times)
 !
end subroutine RT_time_configuration_setup

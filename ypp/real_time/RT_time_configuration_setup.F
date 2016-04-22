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
subroutine RT_time_configuration_setup(what)
 !
 use units,          ONLY:AUT2FS,pi,HA2EV
 use YPP,            ONLY:RT_conf,RT_time,i_t_start,i_t_end
 use pars,           ONLY:SP
 use com,            ONLY:msg
 use real_time,      ONLY:RT_step
 use RT_control,     ONLY:JPC_RT_IO_t,TIME_adjust
 implicit none
 !
 character(*) :: what 
 !
 ! Work Space
 ! 
 integer  :: i_t,i_ref(2),i_range(2),i_io(2),i_c
 real(SP) :: STEP,TIME
 !
 STEP  = (JPC_RT_IO_t%Time(JPC_RT_IO_t%N)-JPC_RT_IO_t%Time(JPC_RT_IO_t%N-1))
 STEP  = TIME_adjust(STEP)
 TIME  = TIME_adjust(JPC_RT_IO_t%Time(JPC_RT_IO_t%N))
 !
 if (JPC_RT_IO_t%N==nint(TIME/STEP)+1) then
   call msg('s','['//what//'] uniform grid   : yes')
 else
   call msg('s','['//what//'] uniform grid   : no')
 endif
 !
 if (any(RT_conf%T_range>0.)) then
   !
   RT_conf%T_range(1)=TIME_adjust( RT_conf%T_range(1) )
   RT_conf%T_range(2)=TIME_adjust( RT_conf%T_range(2) )
   ! 
   i_range(:) = nint( RT_conf%T_range(:)/RT_step )
   i_ref      = 0
   !
   do i_t=1,JPC_RT_IO_t%N-1
     !
     i_io(1) = nint( JPC_RT_IO_t%Time(i_t)/RT_step )
     i_io(2) = nint( JPC_RT_IO_t%Time(i_t+1)/RT_step )
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
   if (i_ref(2)==0) i_ref(2)=JPC_RT_IO_t%N
   !
   RT_conf%T_range=(/JPC_RT_IO_t%Time(i_ref(1)),JPC_RT_IO_t%Time(i_ref(2))/)
   !
 else
   !
   i_ref           = (/1,JPC_RT_IO_t%N/)
   RT_conf%T_range = (/JPC_RT_IO_t%Time(1),TIME/)
   !
 endif
 !
 RT_conf%delta_T   = STEP
 RT_conf%T_n_steps = i_ref(2)-i_ref(1)+1
 !
 allocate(RT_time(RT_conf%T_n_steps))
 do i_t=i_ref(1),i_ref(2)
   RT_time(i_t-i_ref(1)+1) = JPC_RT_IO_t%Time(i_t)
 enddo
 !
 i_t_start=i_ref(1)
 i_t_end  =i_ref(2)
 !
 ! Field deduced dumping
 !=======================
 RT_conf%Field_damp_factor= 2._SP*pi/(real(RT_conf%T_n_steps-1,SP)*RT_conf%delta_T)
 !
 ! Messaging
 !===========
 !
 call msg('s','['//what//'] Time range     [fs]:',RT_conf%T_range*AUT2FS)
 call msg('s','['//what//'] Time step      [fs]:',RT_conf%delta_T*AUT2FS)
 call msg('s','['//what//'] Time steps         :',RT_conf%T_n_steps)
 call msg('s','['//what//'] Damping       [meV]:',RT_conf%damp_factor*1000.*HA2EV)
 call msg('s','['//what//'] Field damping [meV]:',RT_conf%Field_damp_factor*1000.*HA2EV)
 !
end subroutine RT_time_configuration_setup

!
! Copyright (C) 2000-2010 C. Attaccalite, A. Marini and the YAMBO team 
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
subroutine RT_databases(en)
 !
 use YPP,            ONLY:RT_J_t,RT_P_t
 use units,          ONLY:SPEED_OF_LIGHT
 use pars,           ONLY:DP,SP,pi
 use electrons,      ONLY:levels
 use com,            ONLY:warning
 use memory_m,       ONLY:mem_est
 use timing,         ONLY:live_timing
 use IO_m,           ONLY:OP_RD,OP_RD_CL,io_control,NONE,DUMP,LOG
 use real_time,      ONLY:NE_steps,NE_i_time,current_cache,RT_ind_J,max_rt_steps_disk,&
&                         current_cache_size,RT_P
 use fields,         ONLY:Efield,n_ext_fields
 implicit none
 !
 type(levels)  :: en
 !
 ! Work Space
 !
 integer            :: io_J,ID,i_f
 integer, external  :: io_SC_components
 !
 call section("+",'Current and Polarization Database')
 !====================================================
 !
 ! Check for saved current and polarization database
 !---------------------------------------------------
 !
 call io_control(ACTION=OP_RD_CL,COM=LOG,MODE=DUMP,SEC=(/1/),ID=ID)
 io_J=io_SC_components('J' ,en,ID)
 if( io_J/=0 ) then
   call warning(" Error while reading databases")
   return
 endif
 !
 ! I/O of Current and Vector Potential
 !=====================================
 !
 allocate(RT_J_t(NE_steps,3),RT_P_t(NE_steps,3))
 allocate(current_cache(current_cache_size,max_rt_steps_disk))
 call mem_est("RT_P_t",(/size(RT_J_t)/),(/2*SP/))
 call mem_est("RT_J_t",(/size(RT_J_t)/),(/2*SP/))
 call mem_est("Current_Cache",(/size(current_cache)/),(/SP/))
 !
 RT_J_t        =(0._DP,0._DP)
 RT_P_t        =(0._DP,0._DP)
 !
 call live_timing('I/O:',NE_steps)
 !
 do NE_i_time=1,NE_steps
   !      
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/NE_i_time+1/),ID=ID) 
   io_J=io_SC_components('J' ,en,ID)
   !
   RT_J_t(NE_i_time,:) = RT_ind_J(:)
   RT_P_t(NE_i_time,:) = RT_P(:)
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 ! Define Efield amplitude
 !
 do i_f=1,n_ext_fields
   Efield(i_f)%amplitude=sqrt(Efield(i_f)%intensity*(8._SP*pi)/SPEED_OF_LIGHT)
 enddo
 !
 deallocate(current_cache)
 call mem_est("Current_Cache")
 !
end subroutine

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
 use YPP,            ONLY:RT_J_t,current_ext_t,A_vecpot_vel_t,RT_P_t
 use units,          ONLY:SPEED_OF_LIGHT
 use pars,           ONLY:DP,SP,pi
 use electrons,      ONLY:levels
 use com,            ONLY:warning
 use memory_m,       ONLY:mem_est
 use timing,         ONLY:live_timing
 use IO_m,           ONLY:OP_RD,OP_RD_CL,io_control,NONE,DUMP
 use real_time,      ONLY:NE_steps,i_time,current_cache,RT_ind_J,j_current_ext,max_rt_steps_disk,&
&                         current_cache_size,RT_P
 use fields,         ONLY:A_vecpot,Efield,n_ext_fields
 implicit none
 !
 type(levels)  :: en
 !
 ! Work Space
 !
 integer            :: io_J,io_G,ID(2),i_f
 integer, external  :: io_SC_components
 !
 call section("+",'RT Databases')
 !===============================
 !
 ! Check for saved current/G_lesser and vector potential
 !-----------------
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID(1)) ! Current response
 io_J=io_SC_components('J' ,en,ID(1))
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID(2)) 
 io_G=io_SC_components('G' ,en,ID(2))
 !
 if(io_J/=0) then
   call warning(" Error while reading J database")
   return
 else if(io_G/=0) then
   call warning(" Error while reading G lesser database")
   return
 endif
 !
 if(any((/io_G,io_J/)/=0)) return
 !
 ! I/O of Current and Vector Potential
 !=====================================
 !
 allocate(RT_J_t(NE_steps,3),current_ext_t(NE_steps,3),RT_P_t(NE_steps,3))
 allocate(A_vecpot_vel_t(NE_steps,3),current_cache(current_cache_size,max_rt_steps_disk))
 call mem_est("RT_P_t",(/size(RT_J_t)/),(/2*SP/))
 call mem_est("RT_J_t",(/size(RT_J_t)/),(/2*SP/))
 call mem_est("Current_ext_t",(/size(current_ext_t)/),(/2*SP/))
 call mem_est("A_vecpot_vel_t",(/size(A_vecpot_vel_t)/),(/2*DP/))
 call mem_est("Current_Cache",(/size(current_cache)/),(/SP/))
 !
 RT_J_t        =(0._DP,0._DP)
 RT_P_t        =(0._DP,0._DP)
 current_ext_t =(0._SP,0._SP)
 A_vecpot_vel_t=(0._DP,0._DP)
 !
 call live_timing('I/O:',NE_steps)
 do i_time=1,NE_steps
   !      
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/i_time+1/),ID=ID(1)) 
   io_J=io_SC_components('J' ,en,ID(1))
   !
   RT_J_t(i_time,:)        = RT_ind_J(:)
   RT_P_t(i_time,:)        = RT_P(:)
   current_ext_t(i_time,:) = j_current_ext(:)
   A_vecpot_vel_t(i_time,:)= A_vecpot%vecpot_vel(:)
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

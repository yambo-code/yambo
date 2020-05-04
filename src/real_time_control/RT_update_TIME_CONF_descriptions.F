!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_update_TIME_CONF_descriptions( )
 ! 
 use units,          ONLY:AUT2FS
 use real_time,      ONLY:NE_time,RT_life_extrap_times,RT_dyn_step,RT_is_dynamically_dephased,&
&                         NE_time_step_update_jump
 use RT_control,     ONLY:CARR_RT_IO_t,OBS_RT_IO_t,OUTPUT_RT_IO_t,GF_RT_IO_t,IO_OBS_cache_INTERVAL_time
 use stderr,         ONLY:real2ch
 use RT_output_m,    ONLY:TIME_STEP_desc
 use descriptors,    ONLY:IO_desc_add
 !
 implicit none
 !
 ! Work Space
 !
 character(11) :: ch
 !
 ch=" [T="//trim(real2ch(NE_time*AUT2FS))
 !
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] Time step",R=(/RT_dyn_step*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] Update time",R=(/NE_time_step_update_jump*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] I/O cache(J-P-M)",R=(/IO_OBS_cache_INTERVAL_time*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] I/O steps(J-P-M)",R=(/OBS_RT_IO_t%INTERVAL_time*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] I/O steps(Carr)",R=(/CARR_RT_IO_t%INTERVAL_time*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] I/O steps(OUT)",R=(/OUTPUT_RT_IO_t%INTERVAL_time*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] I/O steps(GF)",R=(/GF_RT_IO_t%INTERVAL_time*AUT2FS/),term="fs")
 !
 if (RT_is_dynamically_dephased) then
   call IO_desc_add(TIME_STEP_desc,SZ=2,kind='r',str=ch//"] Life Inter-Intra",R=RT_life_extrap_times*AUT2FS,term="fs")
 endif
 !
end subroutine RT_update_TIME_CONF_descriptions

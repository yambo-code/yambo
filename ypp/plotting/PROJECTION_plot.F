! 
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine PROJECTION_plot( what, N, bands, W, TIME, OBS_W, OBS_B)
 !
 use pars,           ONLY:SP,lchlen,schlen
 use vec_operate,    ONLY:c2a,v_norm
 use units,          ONLY:HA2EV
 use com,            ONLY:msg
 use LIVE_t,         ONLY:live_timing
 use YPP_interfaces, ONLY:PROJECTION_k_on_the_path
 use YPPm,           ONLY:PROJECT_depth,l_PROJECT_line,CIRCUIT_k_label,PROJECT_threshold,&
&                         l_PROJECT_plane,PROJECT_plane_X,PROJECT_plane_Y
 use interpolate,    ONLY:GRID_k
 use electrons,      ONLY:n_spin
 use stderr,         ONLY:set_real_printed_length
 !
#include<memory.h>
 !
 character(*)       :: what
 integer            :: N,bands(2)
 real(SP)  ,optional:: W(N),TIME(N)
 real(SP)  ,optional:: OBS_W(GRID_k%nbz/PROJECT_depth,N,n_spin)
 real(SP)  ,optional:: OBS_B(bands(1):bands(2),GRID_k%nbz/PROJECT_depth,n_spin)
 !
 ! Work Space
 !
 character(lchlen)  :: of_string
 character(schlen)  :: of_name
 integer       :: ik,i_W,is,ik_path,of_unit
 real(SP)      :: O(3),k(3),kdist(GRID_k%nbz),max_OBS,projection(2)
 integer         ,external   :: find_the_unit
 !
 ! Allocation
 !============
 !
 call set_real_printed_length(f_length=10,g_length=10)
 !
 if (what=="DOS"  ) of_name="dos"
 if (what=="BANDS") of_name="bands_projected"
 !
 if (l_PROJECT_line) then
   !================
   !
   ! |k| distance along the path
   !-----------------------------
   kdist=0._SP
   O=GRID_k%pt(1,:)
   do ik=2,GRID_k%nbz
     ik_path=PROJECTION_k_on_the_path(ik)
     if (ik_path==0) cycle
     k=GRID_k%pt(ik,:)-O
     call c2a(v_in=k,mode='ki2c')
     kdist(ik_path)=kdist(ik_path)+v_norm(k)
     if (len_trim( CIRCUIT_k_label(ik_path))>0) then
       if (ik_path>1) kdist(ik_path+1:)=kdist(ik_path)
       O=GRID_k%pt(ik,:)
     endif
   enddo
   !
   call live_timing('Printing the projected '//what,GRID_k%nbz/PROJECT_depth)
   !
   of_unit=find_the_unit(trim(of_name))
   !
   max_OBS=maxval(OBS_W)
   !
   do ik=1,GRID_k%nbz
     !
     ik_path=PROJECTION_k_on_the_path(ik)
     if (ik_path==0) cycle
     !
     do i_W=1,N
       !
       if (.not.any((/OBS_W(ik_path,i_W,:)> max_OBS*PROJECT_threshold/100./))) cycle
       !
       if (n_spin==1) then
         call msg('o '//trim(of_name),'',(/kdist(ik_path),W(i_W)*HA2EV,OBS_W(ik_path,i_W,1)/),INDENT=-2,USE_TABS=.true.,&
&                 formatted_msg=of_string)
       else if (n_spin==2) then
         call msg('o '//trim(of_name),'',(/kdist(ik_path),W(i_W)*HA2EV,OBS_W(ik_path,i_W,:),  &
&                             OBS_W(ik_path,i_W,1)+OBS_W(ik_path,i_W,2)/),INDENT=-2,USE_TABS=.true.,formatted_msg=of_string)
       endif
       if (len_trim(CIRCUIT_k_label(ik_path))>0) of_string=trim(of_string)//"  ["//trim(CIRCUIT_k_label(ik_path))//"]"
       call msg_deliver_fast(of_unit,of_string)
       !
     enddo
     !
     call live_timing(steps=1)
     !
     call msg('o '//trim(of_name)," ")
     !
   enddo
   !
   call live_timing()
   !
 endif
 !
 if (l_PROJECT_plane) then
   !================
   !
   call live_timing('Printing the projected '//what,GRID_k%nbz/PROJECT_depth)
   !
   of_unit=find_the_unit(trim(of_name))
   !
   do ik=1,GRID_k%nbz/PROJECT_depth
     !
     k=GRID_k%pt(ik,:)
     call c2a(v_in=k,mode='ki2c')
     projection(1)=dot_product(k,PROJECT_plane_X)
     projection(2)=dot_product(k,PROJECT_plane_Y)
     !
     call msg('o '//trim(of_name),'',(/projection,OBS_B(bands(1):bands(2),ik,1)*HA2EV/),INDENT=-2,USE_TABS=.true.,&
&             formatted_msg=of_string)
     call msg_deliver_fast(of_unit,trim(of_string))
     !
   enddo
   !
   call live_timing(steps=1)
   !
   call live_timing()
   !
 endif
 !
 call set_real_printed_length()
 !
end subroutine

!
! Copyright (C) 2000-2012 C. Attaccalite and the YAMBO team 
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
subroutine COLLISIONS_IO_store(i_qp)
 !
 use pars,            ONLY:SP
 use QP_m,            ONLY:QP_n_states
 use collisions_IO,   ONLY:COLLISIONS_cutoff,TEMP_COLLISIONS_v,COLLISIONS_table,&
&                          COLLISIONS_v,n_collisions,io_COLLISIONS,COLLISIONS_range
 use IO_m,            ONLY:io_control,REP,OP_WR_CL,OP_APP_WR_CL
 use drivers,         ONLY:l_carrier_dynamics
 !
 implicit none                                              
 !
 integer, intent(in) :: i_qp
 !
 ! Work Space
 !
 integer   :: ic,i1,i2,i3
 integer   :: io_err,ID
 logical   :: local_condition
 real(SP)  :: COLLISIONS_max
 !
 ! Reduction of tiny elements of tmp_sx.
 !
 COLLISIONS_max=maxval(abs(TEMP_COLLISIONS_v(:,:,:)))        
 !
 ic=0
 !
 do i1=COLLISIONS_range(1,1),COLLISIONS_range(1,2)
   do i2=COLLISIONS_range(2,1),COLLISIONS_range(2,2)
     do i3=COLLISIONS_range(3,1),COLLISIONS_range(3,2)
       local_condition=&
&         abs(TEMP_COLLISIONS_v(i1,i2,i3))>=COLLISIONS_max*COLLISIONS_cutoff.or.COLLISIONS_cutoff<0.0&
&         .or.l_carrier_dynamics
       if (local_condition) ic=ic+1
     enddo
   enddo
 enddo
 !
 n_collisions    =ic
 COLLISIONS_table(:,:,:)=0
 ic=0
 !
 do i1=COLLISIONS_range(1,1),COLLISIONS_range(1,2)
   do i2=COLLISIONS_range(2,1),COLLISIONS_range(2,2)
     do i3=COLLISIONS_range(3,1),COLLISIONS_range(3,2)
       local_condition=&
&         abs(TEMP_COLLISIONS_v(i1,i2,i3))>=COLLISIONS_max*COLLISIONS_cutoff.or.COLLISIONS_cutoff<0.0&
&         .or.l_carrier_dynamics
       if (local_condition) then
         ic=ic+1
         COLLISIONS_table(i1,i2,i3)=1
         COLLISIONS_v(ic)          =TEMP_COLLISIONS_v(i1,i2,i3) 
       endif
     enddo      
   enddo      
 enddo      
 !
 call IO_and_Messaging_switch("+DB_is_fragmented")
 !
 if (i_qp==1) call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 if (i_qp/=1) call io_control(ACTION=OP_APP_WR_CL,SEC=(/2/),ID=ID)
 io_err=io_COLLISIONS(i_qp,ID)
 !
 if (i_qp==QP_n_states) call IO_and_Messaging_switch("-DB_is_fragmented")
 !
end subroutine COLLISIONS_IO_store

!
! Copyright (C) 2000-2013 C. Attaccalite and the YAMBO team 
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
subroutine COLLISIONS_store(i_qp,COLL)
 !
 use pars,            ONLY:SP
 use QP_m,            ONLY:QP_n_states
 use collision,       ONLY:COLLISIONS_element,COLLISIONS_cutoff
 use IO_m,            ONLY:io_control,REP,OP_WR_CL,OP_APP_WR_CL,IO_and_Messaging_switch
 use drivers,         ONLY:l_elel_scatt
 use interfaces,      ONLY:io_COLLISIONS
 !
 implicit none                                              
 !
 integer,                  intent(in)    :: i_qp
 type(COLLISIONS_element), intent(inout) :: COLL
 !
 ! Work Space
 !
 integer   :: ic,i1,i2,i3,i_cycle,io_err,ID
 logical   :: local_condition
 real(SP)  :: COLLISIONS_max
 !
 ! Reduction of tiny elements of tmp_sx.
 !
 COLLISIONS_max=maxval(abs(COLL%v3(:,:,:)))        
 COLL%table(:,:,:)=0
 !
 do i_cycle=1,2
   ic=0
   do i1=COLL%D(1,1),COLL%D(1,2)
     do i2=COLL%D(2,1),COLL%D(2,2)
       do i3=COLL%D(3,1),COLL%D(3,2)
         local_condition=abs(COLL%v3(i1,i2,i3))>=COLLISIONS_max*COLLISIONS_cutoff.or.&
&                        COLLISIONS_cutoff<0.0.or.l_elel_scatt
         if (local_condition) then
           ic=ic+1
           if (i_cycle==2) then
             COLL%table(i1,i2,i3)=1
             COLL%v(ic)          =COLL%v3(i1,i2,i3) 
           endif
         endif
       enddo
     enddo
   enddo
 enddo
 COLL%N=ic
 !
 call IO_and_Messaging_switch("+DB_is_fragmented")
 !
 if (i_qp==1) call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 if (i_qp/=1) call io_control(ACTION=OP_APP_WR_CL,SEC=(/2/),ID=ID)
 io_err=io_COLLISIONS(i_qp,ID,COLL=COLL)
 !
 if (i_qp==QP_n_states) call IO_and_Messaging_switch("-DB_is_fragmented")
 !
end subroutine COLLISIONS_store

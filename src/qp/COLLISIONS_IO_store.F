!
! Copyright (C) 2000-2009 C. Attaccalite and the YAMBO team 
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
 use pars,            ONLY:SP
 use QP_m,            ONLY:QP_nk,QP_nb,QP_n_states
 use collisions_IO,   ONLY:io_COLLISIONS,COLLISIONS_cutoff,TEMP_COLLISIONS_v,COLLISIONS_table,&
&                          COLLISIONS_v,n_collisions
 use IO_m,            ONLY:io_control,OP_WR_CL,REP,OP_WR,dbs_fragment
 !
 implicit none                                              
 !
 integer, intent(in) :: i_qp
 !
 ! Work Space
 !
 integer   :: ic,jb,jb2,ik
 integer   :: ID,io_err
 real(SP)  :: COLLISIONS_max
 logical   :: dbs_fragment_save
 !
 ! Reduction of tiny elements of tmp_sx.
 !
 COLLISIONS_max=maxval(abs(TEMP_COLLISIONS_v(:,:,:)))        
 !
 ic=0
 !
 do jb=1,QP_nb
   do jb2=1,QP_nb
     do ik=1,QP_nk
       if(abs(TEMP_COLLISIONS_v(jb,jb2,ik))>=COLLISIONS_max*COLLISIONS_cutoff.or.COLLISIONS_cutoff<0.0)  ic=ic+1
     enddo
   enddo
 enddo
 !
 n_collisions    =ic
 COLLISIONS_table(:,:,:)=0
 ic=0
 !
 do jb=1,QP_nb
   do jb2=1,QP_nb
     do ik=1,QP_nk
       if(abs(TEMP_COLLISIONS_v(jb,jb2,ik))>=COLLISIONS_max*COLLISIONS_cutoff.or.COLLISIONS_cutoff<0.0)  then
         ic=ic+1
         COLLISIONS_table(jb,jb2,ik)=1
         COLLISIONS_v(ic)           =TEMP_COLLISIONS_v(jb,jb2,ik)
       endif
     enddo      
   enddo      
 enddo      
 !
 dbs_fragment_save=dbs_fragment
 dbs_fragment=.TRUE.
 if (i_qp==1) then
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=io_COLLISIONS(i_qp,ID)
 else
   call io_control(ACTION=OP_WR_CL,SEC=(/2/),ID=ID)
   io_err=io_COLLISIONS(i_qp,ID)
 endif
 if (i_qp==QP_n_states)  dbs_fragment=dbs_fragment_save
 !
end subroutine COLLISIONS_IO_store

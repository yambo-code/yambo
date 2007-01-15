!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine elph_epsm1_load(X_ng)
!================================
 use pars,           ONLY:SP
 use IO_m,           ONLY:io_control,OP_RD,DUMP,RD_CL_IF_END
 use X_m,            ONLY:X_mat,X_t,X_reset,X_alloc
 use R_lattice,      ONLY:nqibz
 use frequency,      ONLY:w_samp,W_reset
 implicit none
 integer           ::X_ng
 !
 !Work Space GPL_END
 !
 type(X_t)         ::X
 type(w_samp)      ::Xw
 integer           ::ID,io_err,iq,ig1,ig2
 integer, external ::ioX
 complex(SP), allocatable :: X_mat_temp(:,:,:)
 !
 ! Resets
 !
 call X_reset(X,2)
 call W_reset(Xw)
 !
 ! I/O
 !
 call io_control(ACTION=OP_RD,MODE=DUMP,SEC=(/1/),ID=ID)
 io_err=ioX(X,Xw,ID)
 !
 if (io_err/=0) return
 !
 ! Allocation
 !
 allocate(X_mat_temp(X%ng,X%ng,nqibz))
 call X_alloc('X',(/X%ng,X%ng,1/))
 !
 do iq=1,nqibz
   !   
   call io_control(ACTION=RD_CL_IF_END,SEC=(/2*iq,2*iq+1/),ID=ID)
   ! 
   io_err=ioX(X,Xw,ID)
   !   
   ! Bare interaction added   
   !   
   forall(ig1=1:X%ng) X_mat(ig1,ig1,1)=X_mat(ig1,ig1,1)+1.
   !
   X_mat_temp(:,:,iq)=X_mat(:,:,1)
   !
 enddo
 !
 ! Transfer
 !
 X_ng=X%ng
 !
 call X_alloc('X')
 call X_alloc('X',(/X%ng,X%ng,nqibz/))
 !
 do iq=1,nqibz
   X_mat(:,:,iq)=X_mat_temp(:,:,iq)
 enddo
 !
 ! CLEAN
 !
 deallocate(X_mat_temp)

 ! GPL_START

 end subroutine


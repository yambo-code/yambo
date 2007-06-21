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
subroutine ELPH_K_print(E,k,q)
 !
 ! This routine calculates and report the total elph to the DBSE kernel 
 !
 use pars,          ONLY:SP,schlen
 use memory_m,      ONLY:mem_est
 use R_lattice,     ONLY:bz_samp
 use com,           ONLY:msg,of_open_close
 use IO_m,          ONLY:io_control,OP_RD_CL,VERIFY,REP
 use electrons,     ONLY:levels
 use BS,            ONLY:BS_K_dim,BS_eh_table,BS_mat,BS_bands,BSS_write_eig_2_db
 use ELPH,          ONLY:elph_nb,EXCPH_mat
 use timing,        ONLY:live_timing
 implicit none
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 !
 ! Work Space ...
 !
 complex(SP) ::BS_R(BS_K_dim),BS_E(BS_K_dim),WS(BS_K_dim,BS_K_dim)
 integer     ::bsID,ii1,ii2,io_err
 integer, external::ioBSS_diago,eval_ELPH_K
 !
 ! ... dummies
 !
 character(schlen) :: o_file
 logical           :: in_exc_basis
 !
 in_exc_basis=.FALSE.
 !
 ! Static, W=0 ELPH Kernel
 !
 if (eval_ELPH_K(E,k,q,.FALSE.,(0.,0.))/=0) return
 !
 ! DIAGONALIZATION DB (tables and eigenvectors)
 !
 allocate (BS_mat(BS_K_dim,BS_K_dim))
 call mem_est('BS_mat WS',(/BS_K_dim**2,BS_K_dim**2/))
 !
 BSS_write_eig_2_db=.TRUE.
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=bsID)
 io_err=ioBSS_diago(1,BS_K_dim,BS_E,BS_R,bsID)
 !
 if (io_err==0.and.BS_bands(2)<=elph_nb) then
   !
   call section('=','ELPH Kernel report')
   !
   ! <l|X|l'> = <l|k>EXCPH_mat(k,k')<k'|l'>
   !
   if (in_exc_basis) then
     call cgemm('c','n',BS_K_dim,BS_K_dim,BS_K_dim,(1.,0.),&
&               BS_mat,BS_K_dim,EXCPH_mat,BS_K_dim,(0.,0.),&
&               WS,BS_K_dim)
     call cgemm('n','n',BS_K_dim,BS_K_dim,BS_K_dim,(1.,0.),&
&               WS,BS_K_dim,BS_mat,BS_K_dim,(0.,0.),&
&               EXCPH_mat,BS_K_dim)
   endif
   !
   ! Output file I/O
   !
   o_file='excph_mat'
   call of_open_close(trim(o_file),'ot')
   do ii1=1,BS_K_dim
     do ii2=1,BS_K_dim
       call msg('o excph_mat','',(/real(ii1),real(ii2),&
&               abs(EXCPH_mat(ii1,ii2))/),INDENT=-2,USE_TABS=.TRUE.)
     enddo
   enddo
   call of_open_close(trim(o_file))
 endif
 !
 ! CLEAN
 !
 deallocate(EXCPH_mat,BS_mat)
 call mem_est("EXCPH_mat BS_mat WS")
 !
end subroutine

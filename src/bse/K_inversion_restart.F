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
subroutine K_inversion_restart(iq,W)
 !
 use pars,           ONLY:SP,cZERO
 use X_m,            ONLY:Epsilon_ii
 use BS,             ONLY:BSS_dr,BSS_er,BSS_n_freqs,BSS_INV_eps,BSS_INV_err,BSS_INV_iw_full,BSS_INV_n_freqs
 use IO_m,           ONLY:io_control,NONE,OP_RD_CL,REP,VERIFY
 use parallel_m,     ONLY:ncpu
 use com,            ONLY:msg
 use frequency,      ONLY:w_samp,W_reset
 !
 implicit none
 !
 integer,             intent(inout) :: iq
 type(w_samp)        ,intent(inout) :: W
 !
 ! Work Space
 !
 integer                  :: ID,iw,i_err,iw_tab
 integer,        external :: io_BSS_invert
 integer,     allocatable :: inv_err_db(:)
 integer,     allocatable :: freq_table(:)
 complex(SP), allocatable :: DB_eps_pl(:,:)
 type(w_samp)             :: W_db
 !
 ! Restart DB I/O
 !================
 !
 call W_reset(W_db)
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 i_err=io_BSS_invert(iq,W_db,ID)
 !
 ! When the damping range is not uniform then changing energy borders means changing damping :(
 !
 if (W_db%dr(1)/=W_db%dr(2).or.W%dr(1)/=W%dr(2)) then
   if (W%er(1)/=W_db%er(1).or.W%er(2)/=W_db%er(2)) i_err=-1
 endif
 !
 if (i_err==0) then
   !
   W_db%n_freqs=BSS_INV_n_freqs
   !
   allocate(BSS_INV_err(W_db%n_freqs),BSS_INV_eps(W_db%n_freqs,2))
   allocate(freq_table(W_db%n_freqs+W%n_freqs),inv_err_db(W_db%n_freqs),&
&           DB_eps_pl(W_db%n_freqs,2))
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   i_err=io_BSS_invert(iq,W_db,ID)
   !
   ! Shadows and re-allocation
   !
   inv_err_db=BSS_INV_err
   DB_eps_pl =BSS_INV_eps
   !
   deallocate(BSS_INV_err,BSS_INV_eps)
   !
   ! Here I create a new energy array that merges the db to the current energies. 
   !
   call FREQUENCIES_merge(W_db,W,freq_table)
   !
   BSS_n_freqs=W%n_freqs
   BSS_er=W%er
   BSS_dr=W%dr
   !
   allocate(BSS_INV_err(W%n_freqs),BSS_INV_eps(W%n_freqs,2))
   BSS_INV_eps=cZERO
   BSS_INV_err   =-1
   !
   do iw=1,W%n_freqs
     !
     ! Load eps/pl only for converged frequencies
     !
     iw_tab=freq_table(iw)
     !
     if (iw_tab>0) then
       BSS_INV_eps(iw,2)=DB_eps_pl(iw_tab,2)
       BSS_INV_err(iw)=inv_err_db(iw_tab)
       if (BSS_INV_err(iw)==0) then
         BSS_INV_eps(iw,1)=DB_eps_pl(iw_tab,1)
       endif
     else
       BSS_INV_eps(iw,1)=cZERO
       BSS_INV_eps(iw,2)=Epsilon_ii(iabs(iw_tab),3)
       BSS_INV_err(iw)=-1
     endif
   enddo
   !
   call msg('rs','[BSE INV] Frequencies read         :',count(inv_err_db==0))
   call msg('rs','[BSE INV]                     [o/o]:',real(count(inv_err_db==0))/real(W%n_freqs)*100._SP)
   call msg('rs','[BSE INV] Frequencies used         :',W%n_freqs)
   !
   ! De/Allocate Epsilon_ii
   !
   deallocate(Epsilon_ii)
   allocate(Epsilon_ii(BSS_n_freqs,4))
   Epsilon_ii(:,1)=W%p(:)
   Epsilon_ii(:,2)=BSS_INV_eps(:,1)/real(ncpu,SP)
   Epsilon_ii(:,3)=BSS_INV_eps(:,2)
   !
   deallocate(inv_err_db,BSS_INV_eps,freq_table,DB_eps_pl)
   !
 else
   !
   allocate(BSS_INV_err(W%n_freqs))
   BSS_INV_err=-1
   Epsilon_ii(:,2)=cZERO
   !
 endif
 !
 allocate(BSS_INV_iw_full(W%n_freqs))
 allocate(BSS_INV_eps(W%n_freqs,2)) ! int no-int
 BSS_INV_eps     =cZERO
 BSS_INV_eps(:,2)=Epsilon_ii(:,3)
 !
end subroutine K_inversion_restart

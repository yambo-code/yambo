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
subroutine K_inversion_restart(iq,i_err,nw_conv,W,W_disk)
 !
 use pars,           ONLY:SP,cZERO
 use X_m,            ONLY:Epsilon_ii
 use BS,             ONLY:BSS_dr,BSS_er,BSS_n_freqs,K_INV_eps_pl,K_INV_err,K_INV_iw_full
 use IO_m,           ONLY:io_control,NONE,OP_RD_CL
 use parallel_m,     ONLY:ncpu
 use com,            ONLY:msg
 use frequency,      ONLY:w_samp
 !
 implicit none
 !
 integer,             intent(inout) :: i_err,iq
 integer,             intent(inout) :: nw_conv
 type(w_samp)        ,intent(inout) :: W,W_disk
 !
 ! Work Space
 !
 integer                  :: ID,iw
 integer,        external :: io_BSS_invert
 integer,     allocatable :: inv_err_disk(:)
 integer,     allocatable :: disk_freq_table(:)
 complex(SP), allocatable :: EPS_merged(:,:)
 !
 if (i_err==0) then
   !
   allocate(inv_err_disk(W_disk%n_freqs),disk_freq_table(W_disk%n_freqs+W%n_freqs),&
&           K_INV_eps_pl(W_disk%n_freqs,2))
   K_INV_eps_pl=cZERO
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   i_err=io_BSS_invert(iq,W_disk,ID)
   !
   ! Here I create a new energy array that merges the disk 
   ! and present energies. In addition the corresponding K_INV_err
   ! and eps_pl_disk are alligned to the disk values.
   !
   call FREQUENCIES_merge(W_disk,W,disk_freq_table)
   !
   BSS_n_freqs=W%n_freqs
   BSS_er=W%er
   BSS_dr=W%dr
   !
   allocate(K_INV_err(W%n_freqs),K_INV_iw_full(W%n_freqs),EPS_merged(W%n_freqs,2))
   EPS_merged=cZERO
   K_INV_err =-1
   !
   nw_conv=0
   do iw=1,W%n_freqs
     !
     ! Load eps/pl only for converged frequencies
     !
     if (disk_freq_table(iw)>0) then
       K_INV_err(iw)=inv_err_disk(disk_freq_table(iw))
       if (K_INV_err(iw)==0) then
         EPS_merged(iw,1)=K_INV_eps_pl(disk_freq_table(iw),1)/real(ncpu,SP)
         EPS_merged(iw,2)=K_INV_eps_pl(disk_freq_table(iw),2)/real(ncpu,SP)
         nw_conv=nw_conv+1
       endif
     endif
   enddo
   call msg('rs','[BSE INV] Frequencies read    [o/o]:',real(nw_conv)/real(W%n_freqs)*100._SP)
   !
   ! De/Allocate Epsilon_ii
   !
   deallocate(Epsilon_ii)
   allocate(Epsilon_ii(BSS_n_freqs,4))
   Epsilon_ii(:,1)=W%p(:)/real(ncpu,SP)
   Epsilon_ii(:,2)=EPS_merged(:,1)/real(ncpu,SP)
   Epsilon_ii(:,3)=EPS_merged(:,3)/real(ncpu,SP)
   !
   deallocate(inv_err_disk,K_INV_eps_pl,disk_freq_table,EPS_merged)
   !
 else
   !
   allocate(K_INV_err(W%n_freqs),K_INV_iw_full(W%n_freqs))
   K_INV_err=-1
   Epsilon_ii(:,2)=cZERO
   !
 endif
 !
end subroutine K_inversion_restart

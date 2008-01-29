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
integer function X_em1(Xen,Xk,q,X,Xw,APPEND_NO_VERIFY)
 ! 
 ! Calculates and stores on file the dielectric matrix.
 !
 use pars,          ONLY:SP
 use drivers,       ONLY:l_alda_fxc,l_gw0
 use X_m,           ONLY:X_t,X_alloc,X_mat,X_poles_tab,self_detect_E_range
 use memory_m,      ONLY:mem_est
 use frequency,     ONLY:w_samp,rg_index_bg,bg_npts,cg_pt,cg_index_bg
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels
 use par_proc_m,    ONLY:pp_redux_wait
 use wave_func,     ONLY:WF_free
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_APP_WR_CL,OP_WR_CL,VERIFY,REP
 use com,           ONLY:depth
 implicit none
 type(levels) ::Xen 
 type(bz_samp)::Xk,q
 type(X_t)    ::X
 type(w_samp) ::Xw
 logical      ::APPEND_NO_VERIFY
 !
 ! Work Space
 !
 integer :: iq,Xdb,i_err,iq_to_start
 integer, external :: ioX
 character(1)      :: sec_mode
 !
 self_detect_E_range=.false.
 !
 ! TDDFT SETUP (-1 argument)
 !
 ! X%f_xc is setup when using ALDA, so that 
 ! the comparison in ioX can be done safely
 !
 call tddft_do_X_W_typs(-1,X,Xw)
 ! 
 ! Sectioning
 !
 sec_mode='*'
 if (depth>0) sec_mode='='
 !
 X_em1=-1
 !
 if (.not.APPEND_NO_VERIFY) then
   if (X%whoami==2) then
     call section(sec_mode,'Static Dielectric Matrix')
     Xw%n=1
   else if (X%whoami==3) then
     call section(sec_mode,'Dynamical Dielectric Matrix')
     if (l_gw0) self_detect_E_range=.true.
   else if (X%whoami==4) then
     call section(sec_mode,'Dynamic Dielectric Matrix (PPA)')
     Xw%n=2
     Xw%er=(/epsilon(1._SP),0._SP/)
     Xw%dr=(/0._SP,X%ppaE/)
     X%ordering='c'
   endif
 endif
 !
 call X_pre_setup(Xen,X)
 !
 ! Build frequency range only if Xw%p was not already allocated
 ! like in LifeTimes calculations
 !
 call freqs_setup(Xw)
 !
 ! Prepare the IO ( and check if q has been already done)
 !
 iq_to_start=X%iq(1)
 if (.not.APPEND_NO_VERIFY) then
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=Xdb)
   X_em1=ioX(X,Xw,Xdb)
   if (X_em1==0) return
   if (X_em1> 0) iq_to_start=X_em1
 endif
 !
 ! Allocation
 !    
 call X_alloc('X',(/X%ng,X%ng,Xw%n(2)/))
 !
 do iq=iq_to_start,X%iq(2)
   !
   ! TDDFT setup
   !
   call tddft_do_X_W_typs(iq,X,Xw)
   if (l_alda_fxc) call tddft_alda_g_space(Xen,Xk)
   !
   call X_os(X_mat,iq,(/1,Xw%n(1)/),Xen,Xk,Xw,X)  
   !
   ! When X%whoami == 1 X is Xo (ACFDT). 
   !
   if (X%whoami/=1) call X_s(iq,(/1,Xw%n(1)/),X,Xw)
   !
   call pp_redux_wait
   if (iq==1) then
     call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=Xdb) 
   else
     call io_control(ACTION=OP_APP_WR_CL,COM=REP,SEC=(/2*iq,2*iq+1/),ID=Xdb)
   endif
   i_err=ioX(X,Xw,Xdb)
   !
   ! As only the master node enters ioX 2 write the ng_db components must 
   ! be initialized for all the nodes
   !
   X%ng_db=X%ng
   !
   ! CLEAN (1)
   !
   if (associated(Xw%p)) then
     deallocate(Xw%p)
     call mem_est("W-p")
   endif
   if (allocated(rg_index_bg)) deallocate(rg_index_bg)
   deallocate(X_poles_tab,bg_npts,cg_pt,cg_index_bg)
   call mem_est("X_poles_tab RGi BGn CGp CGi")
   !
 enddo
 !
 ! CLEAN (2)
 !
 call X_alloc('X')
 call X_alloc('OptOsc')
 if (.not.APPEND_NO_VERIFY.or.X%iq(2)==q%nibz) call WF_free()
 call pp_redux_wait
 !
end function

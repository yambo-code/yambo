!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine K_solvers(iq,Ken,Xk,q,X_static,W_bss)
 !
 use pars,           ONLY:SP,schlen
 use memory_m,       ONLY:mem_est
 use com,            ONLY:warning
 use frequency,      ONLY:w_samp
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use drivers,        ONLY:l_bs_fxc,l_rpa_IP
 use IO_m,           ONLY:io_control,REP,NONE,OP_RD_CL
 use X_m,            ONLY:X_t,X_epsilon,X_drude_term
 use BS,             ONLY:BS_not_const_eh_f,BSS_mode,BSS_n_descs
#if defined _DISTRIBUTED
 use BS,             ONLY:ioBS_DISTRIBUTED
#endif
#if defined _SC
 use fields,         ONLY:global_gauge
#endif
 !
 implicit none
 type(levels) ::Ken
 type(bz_samp)::Xk,q
 type(X_t)    ::X_static
 type(w_samp) ::W_bss
 integer      ::iq
 !
 ! Work Space
 !
 real(SP)          ::v(3)
 logical           ::run_Haydock,run_Diago,Pert_inversion
 character(schlen) ::ch
 integer           :: ID,ioBS_err
 integer, external ::ioBS
 !
 call section('*','BSE solver(s)')
 !
 if(.not.l_rpa_IP) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID)
#if defined _DISTRIBUTED
   ioBS_err=ioBS_DISTRIBUTED(iq,X_static,ID)
#else
   ioBS_err=ioBS(iq,X_static,ID)
#endif
   if(ioBS_err/=0) return
 endif
 !
 ! GPL_EXCLUDE_START
 !
 ! The TDDFT way 
 !
 if (index(BSS_mode,'t')/=0) then
   call tddft_BSK_disk_2_FXC(iq,Xk,W_bss,X_static) 
   l_bs_fxc=.true.
 endif
 !
 ! GPL_EXCLUDE_END
 !
 ! Solvers
 !
 run_Diago           =index(BSS_mode,'d')/=0
#if defined _SC
 run_Haydock         =index(BSS_mode,'h')/=0.and.trim(global_gauge)/='velocity'
#else
 run_Haydock         =index(BSS_mode,'h')/=0
#endif
 !
#if defined _SC
 Pert_inversion      =index(BSS_mode,'i')/=0.and.trim(global_gauge)/='velocity'
#else
 Pert_inversion      =index(BSS_mode,'i')/=0
#endif
 !
 if (run_Haydock.and.associated(Ken%W)) then
   call warning('K with QP widths not supported in the Haydock solver')
   run_Haydock=.false.
 endif
 !
 if (run_Haydock) then
   !
   if (BS_not_const_eh_f) call warning('Fractional occupations still unsupported (Haydock)')
   !
   if (.not.BS_not_const_eh_f) then
     call K_output_file(iq,-1)
     call K_Haydock(iq,W_bss)
     BSS_n_descs=BSS_n_descs-4 
   endif
 endif
 !
 if (run_Diago)          call K_diagonalization(iq,W_bss)
 !
 if (Pert_inversion)     call K_inversion(iq,W_bss,Ken,Xk,q)
 !
 if (l_rpa_IP) then
   !  
   X_epsilon(1,:)=W_bss%p(:)
   X_epsilon(2,:)=0.
   !
   ! Initialize & write the output file
   !
   call K_output_file(iq,-4)
   call K_output_file(iq,4)
   !
 endif
 !
 ! CLEAN
 !
 deallocate(X_epsilon,X_drude_term)
 !
end subroutine

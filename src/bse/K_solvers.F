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
subroutine K_solvers(iq,Ken,Xk,q,X_static,W_bss)
 !
 use com,            ONLY:warning
 use frequency,      ONLY:w_samp
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use X_m,            ONLY:X_t,Epsilon_ii,Joint_DOS,X_drude_term
 use BS,             ONLY:BS_not_const_eh_f,BSS_mode,BSS_n_descs,BSE_mode,&
&                         BS_K_has_been_calculated_loaded
 use parallel_m,     ONLY:master_cpu
 use IO_m,           ONLY:IO_and_Messaging_switch
#if defined _KERR
 use drivers,        ONLY:l_kerr
 use X_m,            ONLY:Epsilon_ij
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
 logical           ::run_Haydock,run_Diago,run_inversion,run_Slepc
 logical           ::stop_Haydock
 !
 ! Check first if the BS database has been calculated/loaded in K.F
 !
 if (.not. BS_K_has_been_calculated_loaded ) return
 !
 call section('*','BSE solver(s)')
 !
 ! GPL_EXCLUDE_START
 !
 ! The TDDFT way 
 !
! if (index(BSS_mode,'t')/=0) then
!   call tddft_BSK_disk_2_FXC(iq,Xk,W_bss,X_static) 
!   l_bs_fxc=.true.
! endif
 !
 ! GPL_EXCLUDE_END
 !
 ! Solvers
 !
 run_Diago     = index(BSS_mode,'d')/=0
 run_Haydock   = index(BSS_mode,'h')/=0
 run_Slepc     = index(BSS_mode,'s')/=0
 run_inversion = index(BSS_mode,'i')/=0
 !
 ! I/O. All read. Only master writes.
 !
 call IO_and_Messaging_switch("+io_out",CONDITION=master_cpu)
 !
 if (run_Haydock.and.associated(Ken%W)) then
   call warning('K with QP widths not supported in the Haydock solver')
   run_Haydock=.false.
 endif
 !
 if (run_Haydock) then
   !
   stop_Haydock = (BS_not_const_eh_f.and.index(BSE_mode,'coupling')>0)
   if (stop_Haydock) call warning('Fractional occupations and coupling still unsupported (Haydock)')
   !
   if(.not.stop_Haydock) then
     call K_output_file(iq,"open Haydock")
     call K_Haydock(iq,W_bss)
     BSS_n_descs=BSS_n_descs-4 
   endif
   !
 endif
 !
 if (run_Diago)          call K_diago_driver(iq,W_bss)
 !
 if (run_Slepc)          call K_slepc(iq,W_bss)
 !
 if (run_inversion)      call K_inversion(iq,W_bss,Ken,Xk,q)
 !
 ! CLEAN
 !
 deallocate(Epsilon_ii,Joint_DOS,X_drude_term)
#if defined _KERR
 if(l_kerr) deallocate(Epsilon_ij)
#endif
 !
end subroutine

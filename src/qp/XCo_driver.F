!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine XCo_driver(E,k,Xk,q)
 !
 use drivers,       ONLY:l_acfdt,l_rho_io,l_sc_run
 use pars,          ONLY:schlen
 use units,         ONLY:HARTREE
 use electrons,     ONLY:levels,n_sp_pol,spin
 use stderr,        ONLY:intc,real2ch
 use R_lattice,     ONLY:bz_samp
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use QP_m,          ONLY:QP_table,QP_Vxc,QP_Sx,QP_n_states
 !
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 ! WorkSpace
 !
 ! Reporting
 !
 integer           ::i_qp,ib,ik,ibp
 character(schlen) ::sec_mode_and_message(2),ch,ch_spin,nloc_fmt,loc_fmt
 !
 ! IO
 !
 integer           :: ID,io_err
 integer, external :: ioXXVXC
 !
 ! Section
 !
 sec_mode_and_message(1)='*'
 sec_mode_and_message(2)='Bare local and non-local Exchange-Correlation'
 if (l_acfdt)   sec_mode_and_message(1)='p'
 if (l_rho_IO)  sec_mode_and_message(2)='Hartree-Fock extended oscillators'
 if (.not.l_sc_run) call section(trim(sec_mode_and_message(1)),trim(sec_mode_and_message(1)))
 !
 ! Main DB I/O
 !
 io_err=-1
 if (.not.l_rho_IO) then
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
   io_err=ioXXVXC(ID)
 endif
 !
 if (io_err==0) then
   !
   call XCo_report_and_IO()
   !
   return
   !
 else
   !
   call XCo_Hartree-Fock(E,k,xk,q)
   !
 endif
 !
 ! Vxc
 !
 if (.not.l_sc_run) then
   !
   call XCo_local(E,Xk)
   call WF_free()
   !
 endif
 !
 call XCo_report_and_IO()
 !
 contains
   !
   subroutine XCo_report_and_IO()
     !
     ik=QP_table(1,3)
     call msg('nr','XC non-loc/loc [eV] @ K ['//trim(intc(ik))//'] (iku):',k%pt(ik,:))
     !
     do i_qp=1,QP_n_states
       !
       ib =QP_table(i_qp,1)
       ibp=QP_table(i_qp,2)
       if (QP_table(i_qp,3)/=ik) then
         ik=QP_table(i_qp,3)
         call msg('nr','XC non-loc/local [eV] @ K ['//trim(intc(ik))//'] (iku):',k%pt(ik,:))
       endif
       !
       ch_spin=' '
       !
       if (n_sp_pol==2.and.spin(QP_table(i_qp,:))==1) ch_spin='(up)'
       if (n_sp_pol==2.and.spin(QP_table(i_qp,:))==2) ch_spin='(dn)'
       ! 
       nloc_fmt='<'//trim(intc(ib))//trim(ch_spin)//'|non-loc|'//trim(intc(ibp))&
&         //trim(ch_spin)//'> ='
       loc_fmt='<'//trim(intc(ib))//trim(ch_spin)//'|local|'//trim(intc(ibp))&
&         //trim(ch_spin)//'> ='
       !
       ch=trim(nloc_fmt)//' '//&
&        trim(real2ch(real(QP_Sx(i_qp)*HARTREE)))//' '//&
&        trim(real2ch(aimag(QP_Sx(i_qp)*HARTREE)))//' '//&
&        trim(loc_fmt)//' '//&
&        trim(real2ch(real(QP_Vxc(i_qp)*HARTREE)))//' '//&
&        trim(real2ch(aimag(QP_Vxc(i_qp)*HARTREE)))
       !
       if (i_qp/=QP_n_states) call msg('r',trim(ch))
       if (i_qp==QP_n_states) call msg('rn',trim(ch))
       !
     enddo
     !
     ! I/O
     !
     if (io_err/=0) then
       call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
       io_err=ioXXVXC(ID)
     endif
     !
     ! MPI barrier
     !
     call pp_redux_wait
     !
   end subroutine
   !
end subroutine

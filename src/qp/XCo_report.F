!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine XCo_report(qp,E,k)
 !
 use drivers,       ONLY:l_real_time,l_gw0
 use units,         ONLY:HA2EV
 use parallel_m,    ONLY:PP_redux_wait
 use pars,          ONLY:schlen
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_sp_pol,spin,E_reset,E_duplicate
 use QP_m,          ONLY:QP_table,QP_Vxc,QP_Vnl_xc,QP_n_states,Vxc_kind,Vnlxc_kind,&
&                        QP_t,QP_nb,QP_nk,QP_reset,report_Vnlxc,report_Sc,report_dSc,report_Vxc
 use com,           ONLY:msg
 use stderr,        ONLY:intc,real2ch
 use xc_functionals,ONLY:E_xc_val
 !
#include<memory.h>
 !
 type(QP_t)    ::qp
 type(levels)  ::E
 type(bz_samp) ::k
 !
 ! Work Space
 !
 type(levels)      ::E_dummy
 integer           ::i_qp,ib,ik,ibp,ikp,i_s
 character(schlen) ::sec_mode_and_message(2),ch,ch_spin,nloc_fmt,loc_fmt
 logical           ::l_Vxc,l_Vnl_xc
 !
 ! Logicals
 !
 l_Vxc   =allocated(QP_Vxc)
 l_Vnl_xc=allocated(QP_Vnl_xc)
 !
 if (l_Vxc) then
   call msg('nr','[xc] E_xc :  '//trim(real2ch(E_xc_val))//' [Ha]')
   call msg( 'r','     E_xc :  '//trim(real2ch(E_xc_val*2.0))//' [Ry]')
 endif
 !
 call QP_reset(qp)
 call E_reset(E_dummy)
 call E_duplicate(E,E_dummy)
 !
 qp%nk=QP_nk
 qp%nb=QP_nb
 qp%n_states=QP_n_states
 qp%desc%n=0
 !
 report_Sc   =.FALSE.
 report_Vnlxc=allocated(QP_Vnl_xc)
 report_Vxc  =allocated(QP_Vxc)
 report_dSc  =.FALSE.
 !
 YAMBO_ALLOC(qp%E,(qp%n_states))
 YAMBO_ALLOC(qp%E_bare,(qp%n_states))
 !
 call msg('nr',' Local XC kind (lXC) ',trim(Vxc_kind))
 call msg('r',' Non-Local XC kind (nlXC) ',trim(Vnlxc_kind))
 !
 ik=-1
 do i_qp=1,QP_n_states
   !
   ib =QP_table(i_qp,1)
   ibp=QP_table(i_qp,2)
   ikp=QP_table(i_qp,3)
   i_s=spin(QP_table(i_qp,:))
   !
   if (ib==ibp) then
     E_dummy%E(ib,ikp,i_s)=E%E(ib,ikp,i_s)
     if (l_Vnl_xc) E_dummy%E(ib,ikp,i_s)=E_dummy%E(ib,ikp,i_s)+real(QP_Vnl_xc(i_qp))
     if (l_Vxc)    E_dummy%E(ib,ikp,i_s)=E_dummy%E(ib,ikp,i_s)-real(QP_Vxc(i_qp))
     qp%E(i_qp)           =E_dummy%E(ib,ikp,i_s)
     qp%E_bare(i_qp)      =E%E(ib,ikp,i_s)
   endif
   !
   if (ikp/=ik) then
     ik=ikp
     loc_fmt =' '
     nloc_fmt=' '
     call msg('nr',' Corrections @ K ['//trim(intc(ik))//'] ',' [eV] ')
   endif
   !
   ch_spin=' '
   !
   if (n_sp_pol==2.and.i_s==1) ch_spin='(up)'
   if (n_sp_pol==2.and.i_s==2) ch_spin='(dn)'
   ! 
   nloc_fmt='<'//trim(intc(ib))//trim(ch_spin)//'|nlXC|'//trim(intc(ibp))&
      //trim(ch_spin)//'> ='
   loc_fmt='<'//trim(intc(ib))//trim(ch_spin)//'|lXC|'//trim(intc(ibp))&
      //trim(ch_spin)//'> ='
   !
   if (l_Vnl_xc) then
     ch=trim(nloc_fmt)//' '//&
&      trim(real2ch(real(QP_Vnl_xc(i_qp)*HA2EV)))//' '//&
&      trim(real2ch(aimag(QP_Vnl_xc(i_qp)*HA2EV)))
   endif
   if (l_Vxc) then
     ch=trim(ch)//' '//&
&      trim(loc_fmt)//' '//&
&      trim(real2ch(real(QP_Vxc(i_qp)*HA2EV)))//' '//&
&      trim(real2ch(aimag(QP_Vxc(i_qp)*HA2EV)))
   endif
   !
   if (i_qp/=QP_n_states) call msg('r',trim(ch))
   if (i_qp==QP_n_states) call msg('rn',trim(ch))
   !
 enddo
 !
 ! Direct GAP
 !
 call section('+',trim(Vnlxc_kind)//' occupations report')
 call REPORT_Occupations(trim(Vnlxc_kind),E_dummy)
 !
 ! Output 
 !
 if (.not.l_gw0.and..not.l_real_time) call QP_of(qp,E,-1,'hf')
 !
 ! MPI barrier
 !
 call PP_redux_wait
 !
 ! Clean up
 !
 YAMBO_FREE(qp%E)
 YAMBO_FREE(qp%E_bare)
 call E_reset(E_dummy)
 call QP_reset(qp)
 !
end subroutine

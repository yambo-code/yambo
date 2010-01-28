!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine QP_of(qp,en,QPdb_read_err)
 !
 use pars,      ONLY:SP,lchlen,schlen,pi
 use units,     ONLY:HARTREE,HA2FSM1
 use drivers,   ONLY:l_life,l_ph_corr
 use com,       ONLY:msg,of_open_close
 use electrons, ONLY:levels,spin,n_sp_pol
 use QP_m,      ONLY:QP_t,QP_Vxc,QP_Sc,QP_dSc,QP_Vnl_xc,QP_table,&
&                    QP_solver,QP_n_states,l_extended_output,Vxc_kind,&
&                    QP_G_dr,QP_Sc_steps,use_GreenF_to_eval_QP,Vnlxc_kind,&
&                    report_Vnlxc,report_Sc,report_dSc,QP_G_amplitude_integral,QP_states_simmetrize 
 use stderr,    ONLY:set_real_printed_length
 use frequency, ONLY:w_samp,W_reset
 implicit none
 type(QP_t)   ::qp
 type(levels) ::en
 integer      ::QPdb_read_err
 !
 ! Work Space
 !
 integer           :: i1,i2,qp_b,qp_k,qp_isp
 character(lchlen) :: file_name,G_Sc_name
 type(w_samp)      :: Sc_W
 logical           ::state_is_2do(QP_n_states)
 !
 !o.qp titles
 !
 integer, parameter:: tpar=14
 integer       :: ntitles,titles_order(tpar)
 real(SP)      :: values(tpar)
 character(schlen) :: titles(tpar)
 !
 titles(1)  = trim(Vxc_kind) ! 1 
 titles(2)  = 'Sc(Eo)' ! 2
 titles(3)  = 'Sc(Eqp)' ! 3
 titles(4)  = 'Eo'  ! 4
 titles(5)  = 'Eqp' ! 5
 titles(6)  =  'Z' ! 6
 titles(7)  =  'Sc`(Eo)' ! 7
 titles(8)  = 'E-Eo' ! 8
 titles(9)  = 'K-point' ! 9
 titles(10) = 'Band' ! 10
 titles(11) = trim(Vnlxc_kind) ! 11
 titles(12) = 'Width[meV]' ! 12
 titles(13) = 'Width[fs]' ! 13
 titles(14) = 'Spin_Pol' ! 14
 !
 ntitles=5
 titles_order(:ntitles)=(/9,10,4,8,2/)
 if (.not.report_Sc.or. ( trim(QP_solver)=='g'.and.use_GreenF_to_eval_QP ))   then
   ntitles=6
   titles_order(4:6)=(/5,8,6/)
 endif
 if (trim(QP_solver)=='g'.and.use_GreenF_to_eval_QP) then
   ntitles=6
   titles_order(:ntitles)=(/9,10,4,5,6/)
 endif
 if (l_ph_corr) then
   ntitles=ntitles+1
   titles_order(ntitles)=12
 endif
 if (l_life) then
   ntitles=5
   titles_order(:ntitles)=(/9,10,4,12,13/)
 endif
 !
 if (l_extended_output) then
   ntitles=13
   titles_order(:ntitles)=(/9,10,4,5,8,1,11,2,3,7,6,12,13/)
   !
   ! When QP components are read from DB some of the variabels are not available
   !
   if (QPdb_read_err==0) then
     ntitles=10
     titles_order(:ntitles)=(/9,10,4,5,8,1,11,6,12,13/)
   endif
 endif
 !
 if (n_sp_pol==2) then
   ntitles=ntitles+1
   titles_order(ntitles)=14
 endif
 !
 if ( trim(QP_solver)=='n'.or.trim(QP_solver)=='s'.or.l_life.or.&
&    (trim(QP_solver)=='g'.and.use_GreenF_to_eval_QP)             ) then
   !
   file_name='qp'
   call of_open_close(file_name,'ot')
   if (trim(QP_solver)=='n') call msg('o qp','# GW [Newton Solver]')
   if (trim(QP_solver)=='s') call msg('o qp','# GW [Secant method Solver]')
   if (trim(QP_solver)=='g') call msg('o qp','# GW [Green`s Function Solver]')
   !
   call msg('o qp','#')
   do i1=2,qp%n_descs
     call msg('o qp','# ',trim(qp%description(i1)),INDENT=0)
   enddo
   !
   call msg('o qp','#')
   call msg('o qp','#',titles(titles_order(:ntitles)),INDENT=0,USE_TABS=.TRUE.)
   call msg('o qp','#')
   !
   qp%k=0.
   !
   do i1=1,QP_n_states
     !
     values=0.
     qp_b  =QP_table(i1,1)
     qp_k  =QP_table(i1,3)
     qp_isp=spin(QP_table(i1,:))
     !
     if (.not.l_life) then
       if (report_Sc) values(2)=real(QP_Sc(i1,1))*HARTREE
       if (report_dSc) then
         values(7:8)=(/real(QP_dSc(i1,1)),real((qp%E(i1)-qp%E_bare(i1))*HARTREE)/) 
         values(3  )=real(QP_Sc(i1,1)+QP_dSc(i1,1)*(qp%E(i1)-qp%E_bare(i1)))*HARTREE
       else
         values(8)=real(qp%E(i1)-qp%E_bare(i1))*HARTREE
       endif
       if (report_Vnlxc) then
         values(1)=real(QP_Vxc(i1))*HARTREE
         values(11)=real(QP_Vnl_xc(i1))*HARTREE
       endif
     endif
     values(4:6)=(/qp%E_bare(i1)*HARTREE,real(qp%E(i1)*HARTREE),real(qp%Z(i1))/)
     values(9:10)=(/real(qp_k),real(qp_b)/)
     values(12:13)=(/aimag(qp%E(i1)*HARTREE/1.E-3),1./(HA2FSM1*2*pi*aimag(qp%E(i1)))/)
     if (qp_isp==1) values(14)= 1.
     if (qp_isp==2) values(14)=-1.
     if (aimag(qp%E(i1))==0.) values(12:13)=0.
     call msg('o qp','',values(titles_order(:ntitles)),INDENT=-2,USE_TABS=.TRUE.)
   enddo
   call of_open_close(file_name)
   !
   ! GPL_EXCLUDE_START 
   !
 else if (trim(QP_solver)=='g') then
   !
   call QP_states_simmetrize(en,state_is_2do=state_is_2do)
   !
   call set_real_printed_length(f_length=12,g_length=12)
   !
   call W_reset(Sc_W)
   Sc_W%n=QP_Sc_steps
   Sc_W%dr=QP_G_dr
   !
   do  i1=1,QP_n_states
     !
     if (.not.state_is_2do(i1)) cycle
     !
     call FREQUENCIES_Green_Function(i1,Sc_W,en%E)
     !
     write (G_Sc_name,'(2(a,i3.3))') 'G_Sc_band_',QP_table(i1,1),'_k_',QP_table(i1,3)
     call of_open_close(G_Sc_name,'ot')
     call msg('o G_Sc','# GW [Green`s function & Self-Energy]')
     call msg('o G_Sc','#')
     do i2=2,qp%n_descs
       call msg('o G_Sc','# ',trim(qp%description(i2)),INDENT=0)
     enddo
     call msg('o G_Sc','#')
     call msg('o G_Sc','# Spectral function at this point integrates to:',QP_G_amplitude_integral(i1),&
&             INDENT=0)
     call msg('o G_Sc','#')
     titles(1)='Energy'
     titles(2:3)=(/'Re[G]','Im[G]'/)
     titles(4:5)=(/'Re[S_tot]','Im[S_tot]'/)
     titles(6)  ='Re[S_c]'
     if (report_Vnlxc) then
       call msg('o G_Sc','#',titles(:6),INDENT=0,USE_TABS=.TRUE.)
     else
       call msg('o G_Sc','#',titles(:5),INDENT=0,USE_TABS=.TRUE.)
     endif
     call msg('o G_Sc','#')
     do i2=1,QP_Sc_steps
       values=0.
       values(1)  =real(Sc_W%p(i2))*HARTREE
       values(2:3)=(/real(qp%GreenF(i1,i2)),aimag(qp%GreenF(i1,i2))/)/HARTREE
       values(4:5)=(/real(qp%S_total(i1,i2)),aimag(qp%S_total(i1,i2))/)*HARTREE
       values(6)=values(4)
       if (report_Vnlxc) then
         values(6)=values(6)-real(QP_Vnl_xc(i1)+QP_Vxc(i1))*HARTREE
         call msg('o G_Sc','',values(:6),INDENT=-2,USE_TABS=.TRUE.)
       else
         call msg('o G_Sc','',values(:5),INDENT=-2,USE_TABS=.TRUE.)
       endif
     enddo
     call of_open_close(G_Sc_name)
   enddo
   call W_reset(Sc_W)
   !
   call set_real_printed_length()
   !
   ! GPL_EXCLUDE_END 
   !
 endif
 !
end subroutine

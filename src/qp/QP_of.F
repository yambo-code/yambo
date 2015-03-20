!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine QP_of(qp,en,QPdb_read_err)
 !
 use pars,      ONLY:SP,lchlen,schlen
 use units,     ONLY:HA2EV,HBAR_eVfs,HA2GHZ
 use drivers,   ONLY:l_life,l_elph_corr
 use com,       ONLY:msg,of_open_close
 use electrons, ONLY:levels,spin,n_sp_pol
 use QP_m,      ONLY:QP_t,QP_Vxc,QP_Sc,QP_dSc,QP_Vnl_xc,QP_table,&
&                    QP_solver,QP_n_states,l_extended_output,Vxc_kind,&
&                    QP_Sc_steps,use_GreenF_to_eval_QP,Vnlxc_kind,&
&                    report_Vnlxc,report_Sc,report_dSc,QP_G_amplitude_integral,&
&                    QP_states_simmetrize,QP_G_info
 use stderr,    ONLY:set_real_printed_length
 implicit none
 type(QP_t)   ::qp
 type(levels) ::en
 integer      ::QPdb_read_err
 !
 ! Work Space
 !
 integer           :: i1,i2,qp_b,qp_k,qp_isp
 character(lchlen) :: file_name,G_Sc_name
 logical           ::state_is_2do(QP_n_states)
 !
 !o.qp titles
 !
 integer, parameter:: tpar=25
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
 titles(15) = 'Z(Re)' ! 15
 titles(16) = 'Z(Im)' ! 16
 titles(17) = 'E_25%(low)' ! 17
 titles(18) = 'E_25%(high)' ! 18
 titles(19) = 'E_50%(low)' ! 19
 titles(20) = 'E_50%(high)' ! 20
 titles(21) = 'E_75%(low)' ! 21
 titles(22) = 'E_75%(high)' ! 22
 titles(23) = 'Integral' ! 23
 titles(24) = 'E-Eo(Mhz)' ! 24
 titles(25) = 'E-Eo(meV)' ! 25
 !
#if defined _QED
 ntitles=5
 titles_order(:ntitles)=(/9,10,25,24/)
#else
 ntitles=5
 titles_order(:ntitles)=(/9,10,4,8,2/)
#endif
 if (.not.report_Sc)   then
   ntitles=6
   titles_order(4:6)=(/5,8,6/)
 endif
 if (trim(QP_solver)=='g'.and.use_GreenF_to_eval_QP) then
   ntitles=14
   titles_order(:14)=(/9,10,5,8,12,15,16,17,18,19,20,21,22,23/)
 else if (l_elph_corr) then
   ntitles=ntitles+1
   titles_order(ntitles)=12
 endif
 if (l_life) then
   ntitles=5
   titles_order(:ntitles)=(/9,10,4,12,13/)
 endif
 !
 if (l_extended_output) then
   !
   ntitles=14
   titles_order(:ntitles)=(/9,10,4,5,8,1,11,2,3,7,15,16,12,13/)
   !
   ! When QP components are read from DB some of the variabels are not available
   !
   if (QPdb_read_err==0) then
     ntitles=11
     titles_order(:ntitles)=(/9,10,4,5,8,1,11,15,16,12,13/)
   endif
   !
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
       if (report_Sc) values(2)=real(QP_Sc(i1,1))*HA2EV
       if (report_dSc) then
         values(7:8)=(/real(QP_dSc(i1,1)),real((qp%E(i1)-qp%E_bare(i1))*HA2EV)/) 
         values(3  )=real(QP_Sc(i1,1)+QP_dSc(i1,1)*(qp%E(i1)-qp%E_bare(i1)))*HA2EV
       else
         values(8)=real(qp%E(i1)-qp%E_bare(i1))*HA2EV
       endif
       if (report_Vnlxc) then
         values(1)=real(QP_Vxc(i1))*HA2EV
         values(11)=real(QP_Vnl_xc(i1))*HA2EV
       endif
     endif
     !
     values(4:6)  =(/qp%E_bare(i1)*HA2EV,real(qp%E(i1)*HA2EV),real(qp%Z(i1))/)
     values(9:10) =(/real(qp_k),real(qp_b)/)
     values(12:13)=(/aimag(qp%E(i1)*HA2EV/1.E-3),HBAR_eVfs/aimag(qp%E(i1))/HA2EV/)
     values(15:16)=(/real(qp%Z(i1)),aimag(qp%Z(i1))/)
     !
     if (allocated(QP_G_amplitude_integral)) then
       values(23)=QP_G_amplitude_integral(i1)
     endif
#if defined _QED
     values(24)=real(QP_Sc(i1,1))*HA2GHZ*1000._SP
     values(25)=real(QP_Sc(i1,1))*1000._SP
#endif
     if (allocated(QP_G_info)) then
       values(17:18)=QP_G_info(i1,1,:)*HA2EV
       values(19:20)=QP_G_info(i1,2,:)*HA2EV
       values(21:22)=QP_G_info(i1,3,:)*HA2EV
     endif
     !
     if (qp_isp==1) values(14)= 1.
     if (qp_isp==2) values(14)=-1.
     if (aimag(qp%E(i1))==0.) values(12:13)=0.
     call msg('o qp','',values(titles_order(:ntitles)),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
   call of_open_close(file_name)
   !
 else if (trim(QP_solver)=='g') then
   !
   call QP_states_simmetrize(en,state_is_2do=state_is_2do)
   !
   call set_real_printed_length(f_length=12,g_length=12)
   !
   do  i1=1,QP_n_states
     !
     if(spin(QP_table(i1,:))==1) write (G_Sc_name,'(2(a,i3.3),a)') 'G_Sc_band_',QP_table(i1,1),'_k_',QP_table(i1,3),'_s1'
     if(spin(QP_table(i1,:))==2) write (G_Sc_name,'(2(a,i3.3),a)') 'G_Sc_band_',QP_table(i1,1),'_k_',QP_table(i1,3),'_s2'
     if(n_sp_pol==1) write (G_Sc_name,'(2(a,i3.3))') 'G_Sc_band_',QP_table(i1,1),'_k_',QP_table(i1,3)
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
     titles(2:3)=(/' Re[G] ','|Im(G)|'/)
     titles(4)=    'Re(S_tot)'
     titles(5:6)=(/'|Im(S_c)|',' Re(S_c) '/)
     if (report_Vnlxc) then
       call msg('o G_Sc','#',titles(:6),INDENT=0,USE_TABS=.TRUE.)
     else
       call msg('o G_Sc','#',titles(:5),INDENT=0,USE_TABS=.TRUE.)
     endif
     call msg('o G_Sc','#')
     do i2=1,QP_Sc_steps
       values=0.
       values(1)  =real(qp%GreenF_W(i1,i2))*HA2EV
       values(2:3)=(/real(qp%GreenF(i1,i2)),aimag(qp%GreenF(i1,i2))/)/HA2EV
       values(4:5)=(/real(qp%S_total(i1,i2)),aimag(qp%S_total(i1,i2))/)*HA2EV
       values(6)=values(4)
       if (report_Vnlxc) then
         values(6)=values(6)-real(QP_Vnl_xc(i1)-QP_Vxc(i1))*HA2EV
         call msg('o G_Sc','',values(:6),INDENT=-2,USE_TABS=.TRUE.)
       else
         call msg('o G_Sc','',values(:5),INDENT=-2,USE_TABS=.TRUE.)
       endif
     enddo
     call of_open_close(G_Sc_name)
   enddo
   !
   call set_real_printed_length()
   !
 endif
 !
end subroutine

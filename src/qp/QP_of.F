!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine QP_of(qp,en,QPdb_read_err,what)
 !
 use pars,      ONLY:SP,lchlen,schlen
 use units,     ONLY:HA2EV,HA2GHZ
 use drivers,   ONLY:l_life,l_elph_corr
 use com,       ONLY:msg,of_open_close
 use electrons, ONLY:levels,spin,n_sp_pol
 use QP_m,      ONLY:QP_t,QP_Vxc,QP_Sc,QP_dSc,QP_Vnl_xc,QP_table,&
&                    QP_solver,QP_n_states,l_extended_output,Vxc_kind,&
&                    use_GreenF_to_eval_QP,Vnlxc_kind,report_Vxc,&
&                    report_Vnlxc,report_Sc,report_dSc,QP_G_amplitude_integral,&
&                    QP_states_simmetrize,QP_G_info,Time_of
 use stderr,    ONLY:set_real_printed_length
 implicit none
 type(QP_t)   ::qp
 type(levels) ::en
 integer      ::QPdb_read_err
 character(*) ::what
 !
 ! Work Space
 !
 integer           ::i1,i2,qp_b,qp_k,qp_isp,i_desc_min
 character(lchlen) ::file_name,G_Sc_name
 logical           ::state_is_2do(QP_n_states)
 !
 !o.qp titles
 !
 integer, parameter :: tpar=31
 integer            :: ntitles,titles_order(tpar)
 real(SP)           :: values(tpar)
 character(schlen)  :: titles(tpar)
 !
 titles(1)  = trim(Vxc_kind) 
 titles(2)  = 'Sc|Eo' 
 titles(3)  = 'Sc|E' 
 titles(4)  = 'Eo'  
 titles(5)  = 'E' 
 titles(6)  = 'Z' 
 titles(7)  = 'dSc/dw|Eo' 
 titles(8)  = 'E-Eo' 
 titles(9)  = 'K-point' 
 titles(10) = 'Band' 
 titles(11) = trim(Vnlxc_kind) 
 titles(12) = 'Width[meV]' 
 titles(13) = 'Width[fs]' 
 titles(14) = 'Spin_Pol' 
 titles(15) = 'Z(Re)' 
 titles(16) = 'Z(Im)' 
 titles(17) = 'E_25%(low)' 
 titles(18) = 'E_25%(high)' 
 titles(19) = 'E_50%(low)' 
 titles(20) = 'E_50%(high)' 
 titles(21) = 'E_75%(low)' 
 titles(22) = 'E_75%(high)' 
 titles(23) = 'Integral' 
 titles(24) = 'E-Eo(Mhz)' 
 titles(25) = 'E-Eo(meV)' 
 titles(26) = 'Ehf' 
 titles(27) = 'Sc|Eo(meV)' 
 titles(28) = 'Width[ps]' 
 titles(29) = 'E-Eo(Ghz)' 
 titles(30) = 'Width[ns]'
 titles(31) = 'dV/V[%]' 
 !
#if defined _QED
 ntitles=11
 titles_order(:ntitles)=(/9,10,4,25,29,27,6,12,28,30,31/)
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
#if defined _QED
   ntitles=7
   titles_order(:ntitles)=(/9,10,4,12,13,28,30/)
#else
   ntitles=5
   titles_order(:ntitles)=(/9,10,4,12,13/)
#endif
 endif
 if (what=='hf') then
   ntitles=4
   titles_order(:ntitles)=(/9,10,4,26/)
   if (report_Vxc) then
     ntitles=ntitles+1
     titles_order(ntitles)=1
   endif 
   if (report_Vnlxc) then
     ntitles=ntitles+1
     titles_order(ntitles)=11
   endif 
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
&    (trim(QP_solver)=='g'.and.use_GreenF_to_eval_QP).or.&
&    trim(QP_solver)=='mixed'.or.what=='hf') then
   !
   file_name=what
   call of_open_close(file_name,'ot')
   !
   i_desc_min=1
   select case ( trim(QP_solver) )
     case ("n")
       call msg('o '//what,'# GW [Newton Solver]')
       i_desc_min=2
     case ("s")
       call msg('o '//what,'# GW [Secant method Solver]')
       i_desc_min=2
     case ("g")
       call msg('o '//what,'# GW [Green`s Function Solver]')
       i_desc_min=2
   end select
   !
   if (what==        'hf') call msg('o '//what,'# Hartree-Fock Components and Corrections')
   !
   if (index(what,"qp")>0) call msg('o '//what,'#')
   !
   do i1=i_desc_min,qp%n_descs
     call msg('o '//what,'#',trim(qp%description(i1)),INDENT=0)
   enddo
   !
   call msg('o '//what,'#')
   call msg('o '//what,'#',titles(titles_order(:ntitles)),INDENT=0,USE_TABS=.TRUE.)
   call msg('o '//what,'#')
   !
   if (index(what,"qp")>0) then
     qp%k=0.
   endif
   !
   do i1=1,QP_n_states
     !
     values=0.
     qp_b  =QP_table(i1,1)
     qp_k  =QP_table(i1,3)
     qp_isp=spin(QP_table(i1,:))
     !
     if (index(what,"qp")>0) then
       !
       if (.not.l_life) then
           if (report_Sc) values(2)=real(QP_Sc(i1,1))*HA2EV
           if (report_dSc) then
             values(7:8)=(/real(QP_dSc(i1,1)),real((qp%E(i1)-qp%E_bare(i1))*HA2EV)/) 
             values(3  )=real(QP_Sc(i1,1)+QP_dSc(i1,1)*(qp%E(i1)-qp%E_bare(i1)))*HA2EV
           else
             values(8)=real(qp%E(i1)-qp%E_bare(i1))*HA2EV
           endif
         if (report_Vxc)     values(1) =real(QP_Vxc(i1))*HA2EV
         if (report_Vnlxc)   values(11)=real(QP_Vnl_xc(i1))*HA2EV
       endif
       !
       values(4:6)  =(/qp%E_bare(i1)*HA2EV,real(qp%E(i1)*HA2EV),real(qp%Z(i1))/)
       values(9:10) =(/real(qp_k),real(qp_b)/)
       values(12:13)=(/aimag(qp%E(i1))*HA2EV/1.E-3,Time_of(aimag(qp%E(i1)),"fs")/)
       values(28)   =Time_of(aimag(qp%E(i1)),"ps")
       values(15:16)=(/real(qp%Z(i1)),aimag(qp%Z(i1))/)
       !
       if (allocated(QP_G_amplitude_integral)) then
         values(23)=QP_G_amplitude_integral(i1)
       endif
#if defined _QED
       values(30)=Time_of(aimag(qp%E(i1)),"ns")
       if (.not.l_life) then
         values(29)=real(qp%E(i1)-qp%E_bare(i1))*HA2GHZ
         values(24)=real(qp%E(i1)-qp%E_bare(i1))*HA2GHZ*1000._SP
         values(25)=real(qp%E(i1)-qp%E_bare(i1))*1000._SP*HA2EV
         values(27)=real(QP_Sc(i1,1))*1000._SP*HA2EV
         values(31)=real(QP_dSC(i1,1))*100
       endif
#endif
       if (allocated(QP_G_info)) then
         values(17:18)=QP_G_info(i1,1,:)*HA2EV
         values(19:20)=QP_G_info(i1,2,:)*HA2EV
         values(21:22)=QP_G_info(i1,3,:)*HA2EV
       endif
       !
     else if (what=="hf") then
       values(4:5)  =(/qp%E_bare(i1)*HA2EV,real(qp%E(i1)*HA2EV)/)
       values(8)=real(qp%E(i1)-qp%E_bare(i1))*HA2EV
       values(9:10) =(/real(qp_k),real(qp_b)/)
       if (report_Vxc)   values(1)=real(QP_Vxc(i1))*HA2EV
       if (report_Vnlxc) values(11)=real(QP_Vnl_xc(i1))*HA2EV
       values(26)=values(5)
     endif
     !
     if (qp_isp==1) values(14)= 1._SP
     if (qp_isp==2) values(14)=-1._SP
     !
     if (l_elph_corr) call set_real_printed_length(f_length=10,g_length=10)
     !
     call msg('o '//what,'',values(titles_order(:ntitles)),INDENT=-2,USE_TABS=.TRUE.)
     !
     if (l_elph_corr) call set_real_printed_length( )
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
     if (spin(QP_table(i1,:))==1) write (G_Sc_name,'(2(a,i3.3),a)') 'G_Sc_band_',QP_table(i1,1),'_k_',QP_table(i1,3),'_s1'
     if (spin(QP_table(i1,:))==2) write (G_Sc_name,'(2(a,i3.3),a)') 'G_Sc_band_',QP_table(i1,1),'_k_',QP_table(i1,3),'_s2'
     if (n_sp_pol==1) write (G_Sc_name,'(2(a,i3.3))') 'G_Sc_band_',QP_table(i1,1),'_k_',QP_table(i1,3)
     call of_open_close(G_Sc_name,'ot')
     call msg('o G_Sc','# GW [Green`s function & Self-Energy]')
     call msg('o G_Sc','#')
     do i2=2,qp%n_descs
       call msg('o G_Sc','#',trim(qp%description(i2)),INDENT=0)
     enddo
     if (allocated(QP_G_amplitude_integral)) then
       call msg('o G_Sc','#')
       call msg('o G_Sc','# Spectral function at this point integrates to',QP_G_amplitude_integral(i1),&
&               INDENT=0)
       call msg('o G_Sc','#')
     endif
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
     do i2=1,qp%GreenF_n_steps
       values=0._SP
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

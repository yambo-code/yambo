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
subroutine QP_of(qp,en,QPdb_read_err,what)
 !
 use pars,          ONLY:SP,lchlen,schlen
 use units,         ONLY:HA2EV,HA2GHZ
 use drivers,       ONLY:l_life
 use com,           ONLY:msg,of_open_close
 use electrons,     ONLY:levels,spin,n_sp_pol
 use QP_m,          ONLY:QP_t,QP_Vxc,QP_Sc,QP_Vnl_xc,QP_table,&
&                        QP_solver,QP_n_states,l_extended_output,Vxc_kind,&
&                        use_GreenF_to_eval_QP,Vnlxc_kind,report_Vxc,&
&                        report_Vnlxc,QP_G_amplitude_integral,&
&                        QP_states_simmetrize,QP_G_info,Time_of
 use OUTPUT_simple, ONLY:OUTPUT_add_column
 use interfaces,    ONLY:DESC_write
 !
 implicit none
 !
 type(QP_t)   ::qp
 type(levels) ::en
 integer      ::QPdb_read_err
 character(*) ::what
 !
 ! Work Space
 !
 integer           ::i_qp,i_w
 character(lchlen) ::G_Sc_name
 logical           ::state_is_2do(QP_n_states)
 real(SP)          ::qp_spin
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
 call OUTPUT_add_column(what="reset",name=what)
 call OUTPUT_add_column(what="open",name=what)
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
   if (index(what,"qp")>0) qp%k=0.
   !
   do i_qp=1,QP_n_states
     !
     ! Fill the COL type with values
     !===============================
     call OUTPUT_add_column(TITLES=(/"K-point","Band   "/),VALUES=(/real(QP_table(i_qp,3)),real(QP_table(i_qp,1))/))
     if (allocated(qp%E)) then
       call OUTPUT_add_column(TITLES=(/"Eo","E "/),VALUES=(/qp%E_bare(i_qp)*HA2EV,real(qp%E(i_qp)*HA2EV)/))
       call OUTPUT_add_column(TITLES=(/"E-Eo"/),VALUES=(/real((qp%E(i_qp)-qp%E_bare(i_qp))*HA2EV)/))
       call OUTPUT_add_column(TITLES=(/"Ehf"/),VALUES=(/real(qp%E(i_qp)*HA2EV)/))
     endif
     if (allocated(QP_sc)) then
       call OUTPUT_add_column(TITLES=(/"Sc|Eo"/),VALUES=(/real(QP_Sc(i_qp,1))*HA2EV/))
     endif
     if (allocated(QP_Vxc))    call OUTPUT_add_column(TITLES=(/"Vxc"/),VALUES=(/real(QP_Vxc(i_qp))*HA2EV/))
     if (allocated(QP_Vnl_xc)) call OUTPUT_add_column(TITLES=(/"Vnlxc"/),VALUES=(/real(QP_Vnl_xc(i_qp))*HA2EV/))
     if (n_sp_pol==2) then
       if (spin(QP_table(i_qp,:))==1) qp_spin= 1._SP
       if (spin(QP_table(i_qp,:))==2) qp_spin=-1._SP
       call OUTPUT_add_column(TITLES=(/"Spin_Pol"/),VALUES=(/qp_spin/))
     endif
     !
     ! Select the colummns to print
     !==============================
     call OUTPUT_add_column(ORDER=(/"K-point","Band   ","Eo     "/))
     !
     if (index(what,"qp")>0) then

!#if defined _QED
! ntitles=11
! titles_order(:ntitles)=(/9,10,4,25,29,27,6,12,28,30,31/)
!#else
! ntitles=5
! titles_order(:ntitles)=(/9,10,4,8,2/)
!#endif
! if (.not.report_Sc)   then
!   ntitles=6
!   titles_order(:)=(/9,10,4,5,8,6/)
! endif
! if (trim(QP_solver)=='g'.and.use_GreenF_to_eval_QP) then
!   ntitles=14
!   titles_order(:14)=(/9,10,5,8,12,15,16,17,18,19,20,21,22,23/)
! else if (l_elph_corr) then
!   ntitles=ntitles+1
!   titles_order(ntitles)=12
! endif
       !
       if (.not.l_life) then
         call OUTPUT_add_column(ORDER=(/"E-Eo ","Sc|Eo"/))
         !if (report_Sc) call OUTPUT_add_column(TITLES=(/"Sc|Eo"/),VALUES=(/real(QP_Sc(i_qp,1))*HA2EV/))
         !if (report_dSc) then
         !  values(7:8)=(/real(QP_dSc(i_qp,1)),real((qp%E(i_qp)-qp%E_bare(i_qp))*HA2EV)/) 
         !  values(3  )=real(QP_Sc(i_qp,1)+QP_dSc(i_qp,1)*(qp%E(i_qp)-qp%E_bare(i_qp)))*HA2EV
         !else
         !  values(8)=real(qp%E(i_qp)-qp%E_bare(i_qp))*HA2EV
         !endif
         !if (report_Vxc)     values(1) =real(QP_Vxc(i_qp))*HA2EV
         !if (report_Vnlxc)   values(11)=real(QP_Vnl_xc(i_qp))*HA2EV
         if (report_Vxc)   call OUTPUT_add_column(ORDER=(/"Vxc"/))
         if (report_Vnlxc) call OUTPUT_add_column(ORDER=(/"Vnlxc"/))
       endif
! !
! if (l_life) then
!#if defined _QED
!   ntitles=7
!   titles_order(:ntitles)=(/9,10,4,12,13,28,30/)
!#else
!   ntitles=5
!   titles_order(:ntitles)=(/9,10,4,12,13/)
!   call OUTPUT_add_column(ORDER=(/"K-point","Band   "/))
!#endif
! endif
       !
       values(4:6)  =(/qp%E_bare(i_qp)*HA2EV,real(qp%E(i_qp)*HA2EV),real(qp%Z(i_qp))/)
       values(12:13)=(/aimag(qp%E(i_qp))*HA2EV/1.E-3,Time_of(aimag(qp%E(i_qp)),"fs")/)
       values(28)   =Time_of(aimag(qp%E(i_qp)),"ps")
       values(15:16)=(/real(qp%Z(i_qp)),aimag(qp%Z(i_qp))/)
       !
       if (allocated(QP_G_amplitude_integral)) then
         values(23)=QP_G_amplitude_integral(i_qp)
       endif
#if defined _QED
       values(30)=Time_of(aimag(qp%E(i_qp)),"ns")
       if (.not.l_life) then
         values(29)=real(qp%E(i_qp)-qp%E_bare(i_qp))*HA2GHZ
         values(24)=real(qp%E(i_qp)-qp%E_bare(i_qp))*HA2GHZ*1000._SP
         values(25)=real(qp%E(i_qp)-qp%E_bare(i_qp))*1000._SP*HA2EV
         values(27)=real(QP_Sc(i_qp,1))*1000._SP*HA2EV
         values(31)=real(QP_dSC(i_qp,1))*100
       endif
#endif
       if (allocated(QP_G_info)) then
         values(17:18)=QP_G_info(i_qp,1,:)*HA2EV
         values(19:20)=QP_G_info(i_qp,2,:)*HA2EV
         values(21:22)=QP_G_info(i_qp,3,:)*HA2EV
       endif
       !
     endif
     !
     ! HF
     !----
     if (what=="hf") then
       call OUTPUT_add_column(ORDER=(/"Ehf"/))
       if (report_Vxc)   call OUTPUT_add_column(ORDER=(/"Vxc"/))
       if (report_Vnlxc) call OUTPUT_add_column(ORDER=(/"Vnlxc"/))
     endif
     !
     call OUTPUT_add_column(what="write")
     !
   enddo
   !
   call OUTPUT_add_column(what="close")
   !
 else if (trim(QP_solver)=='g') then
   !
   call QP_states_simmetrize(en,state_is_2do=state_is_2do)
   !
   do  i_qp=1,QP_n_states
     !
     if (spin(QP_table(i_qp,:))==1) write (G_Sc_name,'(2(a,i3.3),a)') 'G_Sc_band_',QP_table(i_qp,1),'_k_',QP_table(i_qp,3),'_s1'
     if (spin(QP_table(i_qp,:))==2) write (G_Sc_name,'(2(a,i3.3),a)') 'G_Sc_band_',QP_table(i_qp,1),'_k_',QP_table(i_qp,3),'_s2'
     if (n_sp_pol==1) write (G_Sc_name,'(2(a,i3.3))') 'G_Sc_band_',QP_table(i_qp,1),'_k_',QP_table(i_qp,3)
     call of_open_close(G_Sc_name,'ot')
     !call msg('o G_Sc','# GW [Green`s function & Self-Energy]')
     call DESC_write('o G_Sc','#',qp%desc)
     if (allocated(QP_G_amplitude_integral)) then
       call msg('o G_Sc','#')
       call msg('o G_Sc','# Spectral function at this point integrates to',QP_G_amplitude_integral(i_qp),&
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
     do i_w=1,qp%GreenF_n_steps
       values=0._SP
       values(1)  =real(qp%GreenF_W(i_qp,i_w))*HA2EV
       values(2:3)=(/real(qp%GreenF(i_qp,i_w)),aimag(qp%GreenF(i_qp,i_w))/)/HA2EV
       values(4:5)=(/real(qp%S_total(i_qp,i_w)),aimag(qp%S_total(i_qp,i_w))/)*HA2EV
       values(6)=values(4)
       if (report_Vnlxc) then
         values(6)=values(6)-real(QP_Vnl_xc(i_qp)-QP_Vxc(i_qp))*HA2EV
         call msg('o G_Sc','',values(:6),INDENT=-2,USE_TABS=.TRUE.)
       else
         call msg('o G_Sc','',values(:5),INDENT=-2,USE_TABS=.TRUE.)
       endif
     enddo
     call of_open_close(G_Sc_name)
   enddo
   !
 endif
 !
end subroutine

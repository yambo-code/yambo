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
subroutine QP_report_and_write(k,qp,en,QPdb_read_err)
 !
 use drivers,       ONLY:l_elph_corr,l_elel_corr,l_life,l_cohsex
 use units,         ONLY:HA2EV
 use pars,          ONLY:SP,schlen,lchlen
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use QP_m,          ONLY:QP_t,QP_dSc,QP_Sc,QP_Vnl_xc,QP_Vxc,QP_table,&
&                        QP_n_states,QP_dSc_steps,use_GreenF_to_eval_QP,On_Mass_Shell_approx,&
&                        QP_solver,report_Vnlxc,report_Sc,report_dSc,Vxc_kind,Vnlxc_kind,&
&                        l_QP_Expand,QP_reset,Time_of
#if defined _ELPH
 use ELPH,          ONLY:QP_DW,QP_OMS_Fan
#endif
 use electrons,     ONLY:n_sp_pol,spin,levels
 use stderr,        ONLY:gen_fmt
 use IO_m,          ONLY:io_control,OP_WR_CL,REP
 use R_lattice,     ONLY:bz_samp
 implicit none
 !
 type(QP_t)       ::qp
 type(bz_samp)    ::k
 type(levels)     ::en
 integer          ::QPdb_read_err
 !
 ! Shadow values
 integer          ::i_band,i_k(QP_n_states),i_spin,n_shadows
 real(SP)         ::dSc_diff
 !
 ! Formats & Strings
 character(schlen)::shadow_fmt(10),data_fmt,spin_string,shadow_string(10)
 character(lchlen)::line_string,dummy_string
 !
 ! Work Space
 integer          ::iqp,i_sh
 real(SP)         ::shadow(10)
 logical          ::print_title
 type(QP_t)       ::qp_expanded
 !
 !I/O
 integer, external::io_QP_and_GF
 logical          ::do_IO
 integer          ::ID,io_err
 !
 ! GLOBAL Logicals
 !
 ! In some combinations of logical
 ! and flags some components of the Correlation
 ! are not loaded/read. To avoid meaningles
 ! numbers to be reported here I perform a selective deallocation
 !============================================================
 !
 report_Sc=.not.QPdb_read_err==0.and..not.On_Mass_Shell_approx.and..not.use_GreenF_to_eval_QP
 report_Vnlxc=l_elel_corr.and..not.l_life
 report_dSc=.not.On_Mass_Shell_approx.and..not.trim(QP_solver)=='g'.and..not.use_GreenF_to_eval_QP.and..not.l_cohsex
 do_IO=.TRUE.
 if (l_life.or.QPdb_read_err==0) then
   do_IO=.FALSE.
   report_dSc=.FALSE.
 endif
 !
 ! TITLES
 !========
 !
 if (trim(QP_solver)/='g'.or.use_GreenF_to_eval_QP) then
   call section('=','QP properties and I/O')
   call msg(' r','Legend (energies in eV)')
   call msg(' r','- B  : Band       - Eo  : bare energy')
 else
   !
   ! Plot the Green Functions
   !
   call QP_of(qp,en,QPdb_read_err,'qp')
   !
   if (do_IO) then
     call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=ID)
     io_err=io_QP_and_GF('G',qp,ID) 
   endif
   return
 endif
 !
 if (l_life) then
   call msg(' r','- Z  : Renormalization factor')
   call msg('rn','- Gm : Width [meV] - Gf  : Width [fs]')
 else 
   call msg(' r','- E  : QP energy  - Z   : Renormalization factor')
   if (report_Sc) call msg(' r','- So : Sc(Eo)     - S   : Sc(E)')
   if (l_elph_corr) call msg('r', '- Gm : Width [meV]- Gf  : Width [fs]')
   if (l_elph_corr) call msg('r', '- F  : Fan [eV]  - DW  : DW [eV]')
   if (report_dSc) call msg('rn','- dSp: Sc derivative precision')
   if (report_Vnlxc) then
     call msg('r','- lXC: Starting Local XC ('//trim(Vxc_kind)//')')
     call msg('r','-nlXC: Starting non-Local XC ('//trim(Vnlxc_kind)//')')
   endif
 endif 
 !
 ! Store temporary shadows of the Self-energy components
 !=========================
 ! and define a logical that define the first band of each
 ! k-point
 !
 forall (iqp=1:QP_n_states) i_k(iqp)=QP_table(iqp,3)
 !
 i_spin=0
 !
 do iqp=1,QP_n_states
   !
   i_band=QP_table(iqp,1)
   if (n_sp_pol==2) i_spin=spin(QP_table(iqp,:))
   !
   ! SHADOWS
   ! =======
   n_shadows=5
   shadow(:5)=(/qp%E_bare(iqp)*HA2EV,real(qp%E(iqp))*HA2EV,&
&               real(qp%E(iqp)-qp%E_bare(iqp))*HA2EV,&
&               real(qp%Z(iqp)),aimag(qp%Z(iqp))/)
   shadow_string(1)=' Eo='
   shadow_fmt(1)   ='(a,f6.2)'
   shadow_string(2)=' E='
   shadow_fmt(2)   ='(a,f6.2)'
   shadow_string(3)=' E-Eo='
   shadow_fmt(3)   ='(a,f6.2)'
   shadow_string(4)=' Re(Z)='
   shadow_fmt(4)   ='(a,f4.2)'
   shadow_string(5)=' Im(Z)='
   shadow_fmt(5)   ='(a,f4.2)'
   !
   if (report_Vnlxc) then
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' nlXC='
     shadow(n_shadows)=real(QP_Vnl_xc(iqp))*HA2EV
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' lXC='
     shadow(n_shadows)=real(QP_Vxc(iqp))*HA2EV
   endif
   !
   if (report_Sc) then
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' So='
     shadow(n_shadows)=real(QP_Sc(iqp,1))*HA2EV
   endif
   !
   if (report_dSc.and.QP_dSc_steps>2) then
     dSc_diff=abs(QP_dSc(iqp,2)-QP_dSc(iqp,1))/abs(QP_dSc(iqp,1)) 
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' dSp='
     shadow(n_shadows)=100._SP-int(dSc_diff*100._SP)
   endif
   !
   ! GPL_EXCLUDE_START
   !
#if defined _ELPH
   if (l_elph_corr) then
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' F ='
     shadow(n_shadows)=QP_OMS_Fan(iqp)*HA2EV
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' DW='
     shadow(n_shadows)=QP_DW(iqp)*HA2EV
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' Gm='
     shadow(n_shadows)=aimag(qp%E(iqp))*HA2EV/1.E-3
     if (shadow(n_shadows)>0.) then
       n_shadows=n_shadows+1
       shadow_string(n_shadows)=' Gf='
       shadow(n_shadows)=Time_of(aimag(qp%E(iqp)),"fs")
     endif
   endif 
#endif
   !
   ! GPL_EXCLUDE_END
   !
   if (l_life) then
     !
     ! SHADOWS
     ! =======
     !
     n_shadows=5
     shadow(:3)=(/qp%E_bare(iqp)*HA2EV,real(qp%Z(iqp)),aimag(qp%Z(iqp))/)
     shadow(4)=aimag(qp%E(iqp))*HA2EV/1.E-3
     shadow(5)=0._SP
     !
     if (abs(shadow(4))>0._SP) shadow(5)=Time_of(abs(aimag(qp%E(iqp))),"fs")
     !
     shadow_string(2)=' Re(Z)='
     shadow_fmt(2)   ='(a,f4.2)'
     shadow_string(3)=' Im(Z)='
     shadow_fmt(3)   ='(a,f4.2)'
     shadow_string(4)=' Gm='
     shadow_string(5)=' Gf='
   endif
   !
   ! Write to Report File
   !======================
   !
   !  Data Format
   !
   if (l_life) then
     data_fmt=gen_fmt(r_v=shadow(3:n_shadows))
     shadow_fmt(3:n_shadows) ='(a,'//trim(data_fmt)//')'
   else
     data_fmt=gen_fmt(r_v=shadow(5:n_shadows))
     shadow_fmt(5:n_shadows) ='(a,'//trim(data_fmt)//')'
   endif
   !
   ! Spin Strings
   !
   spin_string= ' '
   if (i_spin==1) spin_string='[up]'
   if (i_spin==2) spin_string='[dn]'
   !
   ! Title
   !
   if (iqp==1) then
     print_title=.TRUE.
   elseif(i_k(iqp)/=i_k(iqp-1)) then
     print_title=.TRUE.
   else
     print_title=.FALSE.
   endif
   !  
   if(print_title) then 
     if (l_life) then
       call msg('nr','Lifetimes @ K ['//trim(intc(i_k(iqp)))//'] (iku)',k%pt(i_k(iqp),:))
     else
       call msg('nr','QP [eV] @ K ['//trim(intc(i_k(iqp)))//'] (iku)',k%pt(i_k(iqp),:))
     endif
   endif
   !
   ! Final String
   !
   dummy_string=' B='//trim(intc(i_band))
   do i_sh=1,n_shadows
     write (line_string,trim(shadow_fmt(i_sh))) trim(dummy_string)//trim(shadow_string(i_sh)),shadow(i_sh)
     dummy_string=line_string
   enddo
   !
   if (iqp/=QP_n_states) call msg('r',trim(line_string))
   if (iqp==QP_n_states) call msg('rn',trim(line_string))
   !
 enddo
 !
 ! QP Expantion
 !==============
 !
 call QP_reset(qp_expanded)
 !
 ! GPL_EXCLUDE_START
 if (l_QP_Expand) call QP_expand(k,qp,qp_expanded)
 ! GPL_EXCLUDE_END
 !
 ! QP DATABASE i/o
 !=================
 if (do_IO) then
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=ID)
   if (.not.allocated(qp_expanded%E)) io_err=io_QP_and_GF('QP',qp,ID)
   if (     allocated(qp_expanded%E)) io_err=io_QP_and_GF('QP',qp_expanded,ID)
 endif
 !
 call QP_reset(qp_expanded)
 !
 ! Output File
 !==============
 !
 call QP_of(qp,en,QPdb_read_err,'qp')
 !
end subroutine

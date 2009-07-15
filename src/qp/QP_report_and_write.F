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
subroutine QP_report_and_write(k,qp,en,QPdb_read_err)
 !
 use drivers,       ONLY:l_ph_corr,l_el_corr,l_life
 use units,         ONLY:HARTREE,HA2FSm1
 use pars,          ONLY:SP,schlen,pi,lchlen
 use com,           ONLY:msg
 use memory_m,      ONLY:mem_est
 use stderr,        ONLY:intc
 use QP_m,          ONLY:QP_t,QP_dSc,QP_G,QP_Sc,QP_Sx,QP_Vxc,QP_rep_string,QP_table,QP_state,&
&                        QP_n_states,QP_dSc_steps,use_GreenF_to_eval_QP,On_Mass_Shell_approx,&
&                        QP_solver
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
 integer          ::iqp,i_sh,i_ph_position
 real(SP)         ::shadow(10)
 !
 !I/O
 integer, external::ioQP
 integer          ::ID,io_err
 !
 !Global logicals
 logical          ::have_Sc,have_Sx,have_dSc,do_IO
 !
 ! GLOABAL Logicals
 !==================
 !
 have_Sc=.not.QPdb_read_err==0.and..not.On_Mass_Shell_approx
 have_Sx=allocated(QP_Sx)
 have_dSc=.not.On_Mass_Shell_approx.and..not.trim(QP_solver)=='g'
 do_IO=.TRUE.
 if (l_life.or.QPdb_read_err==0) then
   do_IO=.FALSE.
   have_dSc=.FALSE.
 endif
 !
 ! TITLES
 !========
 !
 if (trim(QP_solver)/='g'.or.use_GreenF_to_eval_QP) then
   call section('=','QP properties and I/O')
   call msg(' r','Legend (energies in eV):')
   call msg(' r','- B  : Band       - Eo  : bare energy')
 else
   !
   ! Plot the Green Functions
   !
   call QP_of(k,qp,en,have_Sc,have_Sx,have_dSc)
   !
   if (do_IO) then
     call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=ID)
     io_err=ioQP('G',qp,ID) 
   endif
   return
 endif
 !
 if (l_life) then
   call msg(' r','- Z  : Renormalization factor')
   call msg('rn','- Gm : Width [meV] - Gf  : Width [fs]')
 else 
   call msg(' r','- E  : QP energy  - Z   : Renormalization factor')
   if (have_Sc) call msg(' r','- So : Sc(Eo)     - S   : Sc(E)')
   if (l_ph_corr) call msg('r', '- Gm : Width [meV]- Gf  : Width [fs]')
   if (have_Sx.and.have_dSc)      call msg('rn','- xx/Vxc : Sx/Vxc - dSp : Sc derivative precision')
   if (.not.have_Sx.and.have_dSc) call msg('rn','- dSp : Sc derivative precision')
   if (have_Sx.and..not.have_dSc) call msg('rn','- xx/Vxc : Sx/Vxc')
 endif 
 !
 ! Store temporary shadows of the Self-energy components
 !=========================
 ! and define a logical that define the first band of each
 ! k-point
 !
 forall (iqp=1:QP_n_states) i_k(iqp)=QP_table(iqp,3)
 !
 do iqp=1,QP_n_states
   !
   i_band=QP_table(iqp,1)
   if (n_sp_pol==2) i_spin=spin(QP_table(iqp,:))
   !
   ! SHADOWS
   ! =======
   n_shadows=4
   shadow(:4)=(/qp%E_bare(iqp)*HARTREE,real(qp%E(iqp))*HARTREE,&
                real(qp%E(iqp)-qp%E_bare(iqp))*HARTREE,real(qp%Z(iqp))/)
   shadow_string(1)=' Eo='
   shadow_fmt(1)   ='(a,f6.2)'
   shadow_string(2)=' E='
   shadow_fmt(2)   ='(a,f6.2)'
   shadow_string(3)=' E-Eo='
   shadow_fmt(3)   ='(a,f6.2)'
   shadow_string(4)=' Z='
   shadow_fmt(4)   ='(a,f4.2)'
   !
   if (have_Sx) then
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' XX='
     shadow(n_shadows)=real(QP_Sx(iqp))*HARTREE
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' Vxc='
     shadow(n_shadows)=real(QP_Vxc(iqp))*HARTREE
   endif
   !
   if (have_Sc) then
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' So='
     shadow(n_shadows)=real(QP_Sc(iqp,1))*HARTREE
   endif
   !
   if (have_dSc.and.QP_dSc_steps>2) then
     dSc_diff=abs(QP_dSc(iqp,2)-QP_dSc(iqp,1))/abs(QP_dSc(iqp,1)) 
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' dSp='
     shadow(n_shadows)=100.-int(dSc_diff*100.) 
   endif
   !
   ! GPL_EXCLUDE_START
   !
   if (l_ph_corr) then
     n_shadows=n_shadows+1
     shadow_string(n_shadows)=' Gm='
     shadow(n_shadows)=aimag(qp%E(iqp))*HARTREE/1.E-3
     if (shadow(n_shadows)>0.) then
       n_shadows=n_shadows+1
       shadow_string(n_shadows)=' Gf='
       shadow(n_shadows)=1./(HA2FSm1*2*pi*aimag(qp%E(iqp))) 
     endif
   endif 
   !
   if (l_life) then
     !
     ! Note the definition of Lifetime is
     !
     !  tau = 1/(2 pi Sigma_2) => tau[fs] = 1/(2 pi Sigma_2[H]) / HA2FSm1
     !
     ! now HA2FSm1 = HARTREE*241.798474*10^-3 = > tau[fs]= 1/( 2 Sigma_2[H] HARTREE ) * 
     !                                                     10^3/( pi 241.798474 )
     !
     ! SHADOWS
     ! =======
     !
     n_shadows=4
     shadow(:2)=(/qp%E_bare(iqp)*HARTREE,real(qp%Z(iqp))/)
     shadow(3)=aimag(qp%E(iqp))*HARTREE/1.E-3
     shadow(4)=0.
     if (shadow(3)>0.) shadow(4)=1./(HA2FSm1*2*pi*aimag(qp%E(iqp)))
     shadow_string(2)=' Z='
     shadow_fmt(2)   ='(a,f4.2)'
     shadow_string(3)=' Gm='
     shadow_string(4)=' Gf='
   endif
   !
   ! GPL_EXCLUDE_END
   !
   ! Write to Report File
   !======================
   !
   !  Data Format
   !
   data_fmt=gen_fmt(r_v=shadow(5:n_shadows))
   shadow_fmt(5:n_shadows) ='(a,'//trim(data_fmt)//')'
   if (l_life) then
     data_fmt=gen_fmt(r_v=shadow(3:n_shadows))
     shadow_fmt(3:n_shadows) ='(a,'//trim(data_fmt)//')'
   endif
   !
   ! Spin Strins
   !
   spin_string= ' '
   if (i_spin==1) spin_string='[up]'
   if (i_spin==2) spin_string='[dn]'
   !
   ! Title
   !
   if (iqp==1.or.i_k(iqp)/=i_k(iqp-1)) then
     if (l_life) then
       call msg('nr','Lifetimes @ K ['//trim(intc(i_k(iqp)))//'] (iku):',k%pt(i_k(iqp),:))
     else
       call msg('nr','QP [eV] @ K ['//trim(intc(i_k(iqp)))//'] (iku):',k%pt(i_k(iqp),:))
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
 ! QP DATABASE i/o
 !=================
 if (do_IO) then
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=ID)
   io_err=ioQP('QP',qp,ID)
 endif
 !
 ! Output File
 !==============
 !
 if (.not.have_Sc)           QP_rep_string="kpbne0eqeezf" 
 if (trim(QP_solver)=='g'.and.use_GreenF_to_eval_QP) &
&                            QP_rep_string="kpbne0eqeezf" 
 if (l_ph_corr.and.index(QP_rep_string,'lm')==0)  &
                             QP_rep_string=trim(QP_rep_string)//'lm'
 if (l_life)                 QP_rep_string='kpbne0lmlf'
 if (n_sp_pol==2)            QP_rep_string=trim(QP_rep_string)//'sp'
 !
 call QP_of(k,qp,en,have_Sc,have_Sx,have_dSc)
 !
end subroutine


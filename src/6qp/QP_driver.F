!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine QP_driver(X,Xen,Xk,en,k,q,Xw)
!========================================
 use pars,          ONLY:SP,schlen,IP,HARTREE
 use memory_m,      ONLY:mem_est
 use drivers,       ONLY:l_life,l_ppa,l_el_corr,l_ph_corr
 use electrons,     ONLY:levels
 use stderr,        ONLY:string_pack
 use R_lattice,     ONLY:bz_samp
 use frequency,     ONLY:w_samp
 use com,           ONLY:msg,gen_fmt
 use X_m,           ONLY:X_t
 use parser_m,      ONLY:parser
 use QP_m,          ONLY:QP_t,QP_dSc,QP_dSc_steps,QP_Vxc,QP_Sc,QP_solver,QP_table,&
&                        QP_Sx,pp_taylor_terms,QP_n_states,QP_nb,&
&                        QP_nk,QP_rep_string,QP_state,QP_dSc_test,QP_reset,&
&                        QP_Sc_steps,QP_G_er,QP_G_dr,QP_G
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 implicit none
 type(levels) ::en,Xen
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(w_samp) ::Xw
!ws
 type(QP_t):: qp
 integer ::i1,i3,io_err,ID
 integer, external ::ioQP
 real(SP)::dscp
 character(schlen)::ch1,ch2,lfmt
!----------------------------------------------------------------
! Parser
!--------
 call parser('NewtDchk',QP_dSc_test)
!
! S_x/Vxc ?
!----------
 if (.not.allocated(QP_Sx).and..not.allocated(QP_Vxc).and.l_el_corr.and.&
     .not.l_life) return

 call QP_reset(qp)
 qp%n_descs=1
 if (trim(QP_solver)=='n') then
   write (qp%description(1),'(a)') ' GoWo solver            : Newton'
   call section('*','Dyson equation: Newton solver')
 else if (trim(QP_solver)=='s') then
   write (qp%description(1),'(a)') ' GoWo solver            : Secant'
   call section('*','Dyson equation: non perturbative secant method')
 else if (trim(QP_solver)=='g') then
   write (qp%description(1),'(a)') ' GoWo solver            : Full Green`s function'
   call section('*','Dyson equation: full Green`s function')
   call msg('rn','[Green] Sc steps             :',QP_Sc_steps)
   call msg('r', '[Green] Sc energy range  [ev]:',QP_G_er*HARTREE)
   call msg('r', '[Green] Sc damping range [ev]:',QP_G_dr*HARTREE)
 else if (.not.l_life) then
   return
 endif
 qp%n_descs=2
 if (l_ppa) write (qp%description(2),trim(&
&   string_pack('(a,',gen_fmt(r_v=(/X%ppaE*HARTREE/)),',a,i2)') )) &
&   ' PPA imaginary pt   [ev]:',X%ppaE*HARTREE,' * Taylor Terms ',pp_taylor_terms
 if (.not.l_ppa) qp%description(2)=' == Real Axis GoWo =='
!
!Basic defs
!===========
!
 call QP_table_setup(en)
!
! Here I copy several informations to the qp_t type.
! This is because I want the qp to be completely independent of the run
! in order to be possibly read as QP correction.
!
 qp%nk=QP_nk
 qp%nb=QP_nb
 qp%n_states=QP_n_states
!
! In lifetimes calculations  the X db may be not
! present.
!
 if (l_life.and.X%ng_db==0) X%ng_db=X%ng

 call QP_descriptions(qp,X,Xw)

 allocate(qp%Z(qp%n_states),qp%E(qp%n_states),qp%Eo(qp%n_states),&
&         qp%k(qp%nk,3),qp%table(qp%n_states,3))
 call mem_est("qp_Z qp_E qp_Eo qp_K qp_table",&
&              (/QP_n_states,QP_n_states,QP_n_states,3*QP_n_states,3*QP_n_states/),&
&              (/2*SP,       2*SP,       2*SP,       SP,           IP/))

 qp%table=QP_table
 qp%E=0.

 do i1=1,QP_n_states
   qp%Eo(i1)=en%E(QP_table(i1,1),QP_table(i1,3),1)
   qp%k(QP_table(i1,3),:)=k%pt(QP_table(i1,3),:)
 enddo
!
! Sc energy steps. 2/3 If Newton/Secant. QP_Sc_steps 
! the full Green`s function is requested.
!====================================================
!
 if (trim(QP_solver)=='g') then
   allocate(QP_Sc(QP_n_states,QP_Sc_steps))
   allocate(QP_G(QP_n_states,QP_Sc_steps))
   call mem_est("QP_G",(/size(QP_G)/))
 else
   QP_dSc_steps=2
   if (QP_dSc_test) QP_dSc_steps=3
   allocate(QP_Sc(QP_n_states,QP_dSc_steps),QP_dSc(qp%n_states,QP_dSc_steps-1))
   call mem_est("QP_dSc",(/size(QP_dSc)/))
!----------------------------------------------------------------
! QP i/o
!========
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
   io_err=ioQP('QP',qp,ID)
   if (io_err==0) return
 endif
 call mem_est("QP_Sc",(/size(QP_Sc)/))
 
!--------------------------------------------------------
!Solver "type"
!=============
 if (trim(QP_solver)=='n') call QP_newton(X,Xen,Xk,en,k,q,qp,Xw)
 if (trim(QP_solver)=='s') call QP_secant(X,Xen,Xk,en,k,q,qp,Xw)

!--------------------------------------------------------
!Lifetimes
!=========
 if (l_life) then
   call QP_raxis(X,Xen,Xk,en,k,q,qp,Xw) 
   qp%Z=1.
   call QP_width_report
   return
 endif

!--------------------------------------------------------
!Green`s functions:
!==================
!
! Electronic
!------------
 if (trim(QP_solver)=='g'.and.l_el_corr.and.&
&                                  .not.l_ppa) call QP_raxis(X,Xen,Xk,en,k,q,qp,Xw)

#if defined PJ_PH
!
! El-Phonon
!-----------
 if (trim(QP_solver)=='g'.and.l_ph_corr) then
   if (.not.l_el_corr) then
     QP_Sc=cmplx(0.,0.,SP)
     QP_G=cmplx(0.,0.,SP)
   endif
   call QP_elph(en,k,q,qp)
 endif
#endif

!------------------------------------------------------------------
 call QP_energy_report

 contains 

 subroutine QP_energy_report
!---------------------------
 real(SP) :: xx_dummy,xc_dummy

 if (trim(QP_solver)/='g') then
   call section('=','Q(uasi)P(article) properties')
   call msg(' r','Legend (energies in eV):')
   call msg(' r','- B  : Band       - Eo  : bare energy')
   call msg(' r','- E  : QP energy  - Z   : Renormalization factor')
   call msg(' r','- So : Sc(Eo)     - S   : Sc(E)')
   call msg('rn','- xx/Vxc : Sx/Vxc - dSp : Sc derivative precision')
   i3=QP_table(1,3)
   write (ch1,'(a,i3.3,a)') 'K [',i3,'] (iku):'
   call msg('r',trim(ch1),k%pt(i3,:))
   do i1=1,QP_n_states
   if (l_el_corr) then
     xx_dummy=real(QP_Sx(i1))*HARTREE
     xc_dummy=real(QP_Vxc(i1))*HARTREE
   else
     xx_dummy=0.
     xc_dummy=0.
   endif
     if (QP_table(i1,3)/=i3) then
       i3=QP_table(i1,3)
       write (ch1,'(a,i3.3,a)') 'K [',i3,'] (iku):'
       call msg('nr',trim(ch1),k%pt(i3,:))
     endif
     write (ch1,'(a,i3.3,3(a,f6.2),a,f4.2,4(a,f6.2))') &
&     'B=',QP_table(i1,1),' Eo=',real(qp%Eo(i1))*HARTREE,' E=',real(qp%E(i1))*HARTREE,&
&     ' E-Eo=',real(qp%E(i1)-qp%Eo(i1))*HARTREE,' Z=',real(qp%Z(i1)),&
&     ' So=',real(QP_Sc(i1,1))*HARTREE,' xx=',xx_dummy,' Vxc=',xc_dummy
     if (QP_dSc_steps>2) then
       dscp=abs(QP_dSc(i1,2)-QP_dSc(i1,1))/abs(QP_dSc(i1,1))
       ch2=trim(ch1)
       write (ch1,'(2a,i3.3,a)') trim(ch2),' dSp=',100-int(dscp*100.),'%'
     endif
     if (i1< QP_n_states) call msg('r',ch1)
     if (i1==QP_n_states) call msg('rn',ch1)
   enddo
!
! QP i/o
!========
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=ID)
   io_err=ioQP('QP',qp,ID)
!
 endif
!
! o.qp
!=======
 call QP_of(k,qp)
!
! CLEAN
!
 deallocate(qp%Z,qp%E,qp%Eo,qp%k,qp%table,&
&           QP_Sc,QP_table,QP_state)
 call mem_est("qp_Z qp_E qp_Eo qp_K qp_table QP_table QP_state QP_Sc")
 if (allocated(QP_dSc)) deallocate(QP_dSc)
 if (allocated(QP_G)) deallocate(QP_G)
 if (allocated(QP_Sx)) deallocate(QP_Sx,QP_Vxc)
 call mem_est("QP_dSc QP_G QP_Sx QP_Vxc")

 end subroutine

 subroutine QP_width_report
!--------------------------
 call section('=','Q(uasi)P(article) properties')
 call msg(' r','Legend:')
 call msg(' r','- B  : Band        - Eo  : bare energy')
 call msg(' r','- Z  : Renormalization factor')
 call msg('rn','- Gm : Width [meV] - Gf  : Width [fs]')

 i3=QP_table(1,3)
 write (ch1,'(a,i3.3,a)') 'K [',i3,'] (iku):'
 call msg('r',trim(ch1),k%pt(i3,:))
 do i1=1,QP_n_states
   if (QP_table(i1,3)/=i3) then
     i3=QP_table(i1,3)
     write (ch1,'(a,i3.3,a)') 'K [',i3,'] (iku):'
     call msg('nr',trim(ch1),k%pt(i3,:))
   endif
 write (ch1,'(a,i3.3,a,f6.2,a,f4.2,4(a,f12.5))') &
&  'B=',QP_table(i1,1),' Eo=',real(qp%Eo(i1))*HARTREE,&
&  ' Z=',real(qp%Z(i1)),&
&  ' Gm=',aimag(qp%E(i1))*HARTREE/1.E-3,' Gf=',0.65822/2./aimag(qp%E(i1))/HARTREE
   if (i1< QP_n_states) call msg('r',ch1)
   if (i1==QP_n_states) call msg('rn',ch1)
 enddo
!
! o.qp
!=======
 QP_rep_string='kpbne0el'
 call QP_of(k,qp)
!
! CLEAN
!
 deallocate(qp%Z,qp%E,qp%Eo,qp%k,qp%table,&
&           QP_dSc,QP_Sc,QP_table,QP_state)
 call mem_est("qp_Z qp_E qp_Eo qp_K qp_table QP_table QP_state")
 call mem_est("QP_dSc QP_Sc")
!
 end subroutine

 end subroutine

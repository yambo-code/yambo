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
subroutine QP_of(kp,qp,en)
 !
 use pars,      ONLY:SP,lchlen,pi
 use units,     ONLY:HARTREE,HA2FSM1
 use drivers,   ONLY:l_life,l_el_corr
 use com,       ONLY:msg,of_open_close
 use R_lattice, ONLY:bz_samp
 use electrons, ONLY:levels,spin
 use QP_m,      ONLY:QP_t,QP_Vxc,QP_Sc,QP_dSc,QP_Sx,QP_table,&
&                    QP_rep_string,QP_solver,QP_n_states,&
&                    QP_G,QP_G_er,QP_G_dr,QP_Sc_steps
 use stderr,    ONLY:set_real_printed_length
 use frequency, ONLY:w_samp,W_reset
 implicit none
 type(QP_t)   ::qp
 type(bz_samp)::kp
 type(levels) ::en
 !
 ! Work Space
 !
 integer           :: i1,i2,qp_b,qp_k,qp_isp
 character(lchlen) :: file_name,G_Sc_name
 type(w_samp)      :: Sc_W
 !
 !o.qp titles
 !
 integer, parameter:: tpar=14
 integer       :: ntitles,torder(tpar)
 real(SP)      :: values(tpar)
 character(10) :: titles(tpar)
 character(2)  :: abbrev(tpar)
 !
 data titles/'Vxc','Sc(Eo)','Sc(Eqp)','Eo' ,'Eqp', 'Z', 'Sc`(Eo)', &
&            'E-Eo','Kpt','Band','Sx','Width[meV]','Width[fs]','Spin_Pol'/
 data abbrev/'xc','s0','sq','e0','eq','zf','ds','ee','kp','bn',&
&            'xx','lm','lf','sp'/
 !
 if (trim(QP_solver)=='n'.or.trim(QP_solver)=='s'.or.l_life) then
   !
   file_name='qp'
   call of_open_close(file_name,'ot')
   if (trim(QP_solver)=='n') call msg('o qp','# GW [Newton Solver]')
   if (trim(QP_solver)=='s') call msg('o qp','# GW [Secant method Solver]')
   !
   call msg('o qp','#')
   do i1=2,qp%n_descs
     call msg('o qp','# ',trim(qp%description(i1)),INDENT=0)
   enddo
   !
   ntitles=0
   do i1=1,len_trim(QP_rep_string),2
     do i2=1,tpar
       if (index(QP_rep_string(i1:i1+1),abbrev(i2))/=0) then
         ntitles=ntitles+1 
         torder(ntitles)=i2
       endif
     enddo
   enddo
   call msg('o qp','#')
   call msg('o qp','#',titles(torder(:ntitles)),INDENT=0,USE_TABS=.TRUE.)
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
       values(2:3)=(/real(QP_Sc(i1,1))*HARTREE,&
&                 real(QP_Sc(i1,1)+QP_dSc(i1,1)*(qp%E(i1)-qp%E_bare(i1)))*HARTREE/)
       values(7:8)=(/real(QP_dSc(i1,1)),real((qp%E(i1)-qp%E_bare(i1))*HARTREE)/)
       if (l_el_corr) then
         values(1)=real(QP_Vxc(i1))*HARTREE
         values(11)=real(QP_Sx(i1))*HARTREE
       endif
     endif
     values(4:6)=(/qp%E_bare(i1)*HARTREE,real(qp%E(i1)*HARTREE),real(qp%Z(i1))/)
     values(9:10)=(/real(qp_k),real(qp_b)/)
     values(12:13)=(/aimag(qp%E(i1)*HARTREE/1.E-3),1./(HA2FSm1*2*pi*aimag(qp%E(i1)))/)
     if (qp_isp==1) values(14)= 1.
     if (qp_isp==2) values(14)=-1.
     if (aimag(qp%E(i1))==0.) values(12:13)=0.
     call msg('o qp','',values(torder(:ntitles)),INDENT=-2,USE_TABS=.TRUE.)
   enddo
   call of_open_close(file_name)
   !
 else if (trim(QP_solver)=='g') then
   !
   call set_real_printed_length(f_length=12,g_length=12)
   !
   call W_reset(Sc_W)
   Sc_W%n=QP_Sc_steps
   Sc_W%er=QP_G_er
   Sc_W%dr=QP_G_dr
   call freqs_setup(Sc_W)
   !
   do  i1=1,QP_n_states
     write (G_Sc_name,'(2(a,i3.3))') 'G_Sc_band_',QP_table(i1,1),'_k_',QP_table(i1,3)
     call of_open_close(G_Sc_name,'ot')
     call msg('o G_Sc','# GW [Green`s function & Self-Energy]')
     call msg('o G_Sc','#')
     do i2=2,qp%n_descs
       if (index(qp%description(i2),'dS/dw')/=0) cycle
       call msg('o G_Sc','# ',trim(qp%description(i2)),INDENT=0)
     enddo
     call msg('o G_Sc','#')
     titles(1)='Energy'
     titles(2:3)=(/'Re[G]','Im[G]'/)
     titles(4:5)=(/'Re[S_tot]','Im[S_tot]'/)
     titles(6)  ='Re[S_c]'
     call msg('o G_Sc','#',titles(:6),INDENT=0,USE_TABS=.TRUE.)
     call msg('o G_Sc','#')
     do i2=1,QP_Sc_steps
       values=0.
       values(1:3)=(/real(Sc_W%p(i2)),real(QP_G(i1,i2)),aimag(QP_G(i1,i2))/)*HARTREE
       values(4:5)=(/real(QP_Sc(i1,i2)),aimag(QP_Sc(i1,i2))/)*HARTREE
       values(6)=values(4)
       if (l_el_corr) values(4)=values(4)+real(QP_Sx(i1)-QP_Vxc(i1))*HARTREE
       call msg('o G_Sc','',values(:6),INDENT=-2,USE_TABS=.TRUE.)
     enddo
     call of_open_close(G_Sc_name)
   enddo
   call W_reset(Sc_W)
   !
   call set_real_printed_length()
   !
 endif
 !
end subroutine

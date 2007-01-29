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
 subroutine QP_of(kp,qp)
!=======================
 use pars,      ONLY:HARTREE,SP,lchlen
 use drivers,   ONLY:l_life,l_el_corr
 use com,       ONLY:msg,of_open_close
 use R_lattice, ONLY:bz_samp
 use QP_m,      ONLY:QP_t,QP_Vxc,QP_Sc,QP_dSc,QP_Sx,QP_table,&
&                    QP_rep_string,QP_solver,QP_n_states,&
&                    QP_G,QP_G_er,QP_G_dr,QP_Sc_steps
 use frequency, ONLY:w_samp,W_reset
 implicit none
 type(QP_t)   ::qp
 type(bz_samp)::kp
!ws
 integer           :: i1,i2
 character(lchlen) :: file_name,G_Sc_name
 type(w_samp)      :: Sc_W
!
!o.qp titles
 integer, parameter:: tpar=13
 integer       :: ntitles,torder(tpar)
 real(SP)      :: rstr(tpar)
 character(10) :: titles(tpar)
 character(2)  :: abbrev(tpar)
!
!-------------------------------------------------------------------
 data titles/'Vxc','Sc(Eo)','Sc(Eqp)','Eo' ,'Eqp', 'Z', 'Sc`(Eo)', &
&            'E-Eo','Kpt','Band','Sx','Width[meV]','Width[fs]'/
 data abbrev/'xc','s0','sq','e0','eq','zf','ds','ee','kp','bn',&
&            'xx','lm','lf'/
!-------------------------------------------------------------------
 if (trim(QP_solver)=='n'.or.trim(QP_solver)=='s'.or.l_life) then
!===================================================================
!
 file_name='qp'
 call of_open_close(file_name,'ot')
 if (trim(QP_solver)=='n') call msg('o qp','# GW [Newton Solver]')
 if (trim(QP_solver)=='s') call msg('o qp','# GW [Secant method Solver]')

 call msg('o qp','#')
 do i1=2,qp%n_descs
   call msg('o qp','# ',trim(qp%description(i1)),INDENT=0)
 enddo

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
 qp%k=0.
 do i1=1,QP_n_states
   rstr=0.
   if (.not.l_life) then
     rstr(2:3)=(/real(QP_Sc(i1,1))*HARTREE,&
&               real(QP_Sc(i1,1)+QP_dSc(i1,1)*(qp%E(i1)-qp%Eo(i1)))*HARTREE/)
     rstr(7:8)=(/real(QP_dSc(i1,1)),real((qp%E(i1)-qp%Eo(i1))*HARTREE)/)
     rstr(9)=QP_table(i1,3)
     rstr(10)=QP_table(i1,3)
     if (l_el_corr) then
       rstr(1)=real(QP_Vxc(i1))*HARTREE
       rstr(11)=real(QP_Sx(i1))*HARTREE
     endif
   endif
   rstr(4:6)=(/real(qp%Eo(i1))*HARTREE,real(qp%E(i1)*HARTREE),real(qp%Z(i1))/)
   rstr(9:10)=(/real(QP_table(i1,3)),real(QP_table(i1,1))/)
   rstr(12:13)=(/aimag(qp%E(i1)*HARTREE/1.E-3),0.65822/2./aimag(qp%E(i1))/HARTREE/)
   if (aimag(qp%E(i1))==0.) rstr(12:13)=0.
   call msg('o qp','',rstr(torder(:ntitles)),INDENT=-2,USE_TABS=.TRUE.)
 enddo
 call of_open_close(file_name)
!
!===================================================================
 else if (trim(QP_solver)=='g'.and.l_el_corr) then
!===================================================================

 call W_reset(Sc_W)
 Sc_W%n=QP_Sc_steps
 Sc_W%er=QP_G_er
 Sc_W%dr=QP_G_dr
 call freqs_setup(Sc_W)

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
     rstr=0.
     rstr(1:3)=(/real(Sc_W%p(i2)),real(QP_G(i1,i2)),aimag(QP_G(i1,i2))/)*HARTREE
     rstr(4:5)=(/real(QP_Sc(i1,i2)),aimag(QP_Sc(i1,i2))/)*HARTREE
     rstr(6)=rstr(4)
     if (l_el_corr) rstr(4)=rstr(4)+real(QP_Sx(i1)-QP_Vxc(i1))*HARTREE
     call msg('o G_Sc','',rstr(:6),INDENT=-2,USE_TABS=.TRUE.)
   enddo
   call of_open_close(G_Sc_name)
 enddo
 call W_reset(Sc_W)

!===================================================================
 endif

 end subroutine

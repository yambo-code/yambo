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
subroutine QP_fit_report(i_ACTION,bands,en,k,qp)
 !
 use pars,          ONLY:SP,cONE,lchlen,schlen
 use units,         ONLY:HA2EV
 use com,           ONLY:msg,of_open_close
 use QP_CTL_m,      ONLY:QP_action,QP_ctl_msg_fmt
 use QP_m,          ONLY:QP_t
 use electrons,     ONLY:n_sp_pol,levels,spin
 use R_lattice,     ONLY:bz_samp
 use stderr,        ONLY:string_remove
 use vec_operate,   ONLY:sort
 !
 implicit none
 !
 type(levels)    ::en
 type(QP_t)      ::qp
 type(bz_samp)   ::k
 integer         ::i_ACTION,bands(2)
 !
 ! Work Space
 !
 integer          ::i_spin,i_d,i_s
 real(SP)         ::db_scissor,fit_scissor,E(4),E_err(2),E_ref(2),W(6),W_err(2),W_dos(2),W_dos_err(2)
 character(lchlen)::ch
 complex(SP)      ::Z
 logical          ::l_E,l_W,l_Z
 !
 ! Fit test output
 !
 character(lchlen)::fname(2)
 character(schlen)::titles(20)
 integer :: E_index(qp%n_states),n_titles
 integer :: n_steps=500
 real(SP):: E_sorted(qp%n_states),value(20),Eo
 complex(SP), external ::QP_eval_the_FIT
 !
 if (index(QP_ctl_msg_fmt,"r")==0) return
 !
 l_E=.FALSE.
 l_W=.FALSE.
 l_Z=.FALSE.
 !
 do i_spin=1,n_sp_pol
   !
   db_scissor=QP_action(i_ACTION)%CTL(i_spin)%db_scissor
   fit_scissor=QP_action(i_ACTION)%CTL(i_spin)%fit_scissor
   E=QP_action(i_ACTION)%CTL(i_spin)%E
   E_err=QP_action(i_ACTION)%CTL(i_spin)%E_err
   E_ref=QP_action(i_ACTION)%CTL(i_spin)%E_ref
   W=QP_action(i_ACTION)%CTL(i_spin)%W
   W_dos=QP_action(i_ACTION)%CTL(i_spin)%W_dos
   W_err=QP_action(i_ACTION)%CTL(i_spin)%W_err
   W_dos_err=QP_action(i_ACTION)%CTL(i_spin)%W_dos_err
   Z=QP_action(i_ACTION)%CTL(i_spin)%Z
   !
   if (i_spin==1) then
     ch="= FIT report for "//trim(QP_action(i_ACTION)%desc)//" ="
     call msg(' r',trim(ch))
     ch="= First column (conduction), Second column (valence) ="
     call msg(' rn',trim(ch))
   endif
   !
   if (n_sp_pol==2) then
     if (i_spin==1) ch="= Spin DN ============================================"
     if (i_spin==2) ch="= Spin UP ============================================"
     call msg(' rn',trim(ch))
   endif
   !
   if (db_scissor/=0._SP) then
     call msg(' r',' Gap correction (database) [ev]:',db_scissor*HA2EV)
   endif
   !
   if (fit_scissor/=0._SP) then
     call msg(' r','             (FIT, actual) [ev]:',fit_scissor*HA2EV)
   endif
   !
   if (any(E/=(/0._SP,1._SP,0._SP,1._SP/))) then
     call msg(' r',' Energies 0th order        [ev]:',(/E(1),E(3)/)*HA2EV)
     call msg(' r',' Energies 1st order            :',(/E(2),E(4)/))
     if (any((/E_err/=0._SP/)))then
       call msg(' r','                          Error:',E_err)
     endif
     l_E=.TRUE.
   endif
   !
   if (any((/W/=0._SP/))) then
     call msg(' r',' Widths 0th order          [ev]:',(/W(1),W(4)/)*HA2EV)
     call msg(' r',' Widths 1st order              :',(/W(2),W(5)/)*HA2EV)
     call msg(' r',' Widths 2st order        [1/eV]:',(/W(3),W(6)/)/HA2EV)
     if (any((/W_err(1:2)/=0._SP/))) then
       call msg(' r','                          Error:',W_err(1:2)*HA2EV)
     endif
     l_W=.TRUE.
   endif
   if (any((/W_dos/=0._SP/))) then
     call msg(' r',' Widths dos prop.          [eV]:',(/W_dos(1),W_dos(2)/)*HA2EV)
     if (any((/W_dos_err(1:2)/=0._SP/))) then
       call msg(' r','                          Error:',W_dos_err(1:2)*HA2EV)
     endif
     l_W=.TRUE.
   endif
   if (Z/=cONE) then
     call msg(' r',  ' Renormalization           [Re]:',real(Z))
     call msg(' r',  '                           [Im]:',aimag(Z))
     l_Z=.TRUE.
   endif
   !
   call msg('rn'," ")
   !
   ! Test output file
   !
   !
   fname(2)=QP_action(i_ACTION)%CTL(1)%db
   fname(1)=string_remove(fname(2),"/ndb.QP")
   fname(2)=fname(1)
   fname(1)=string_remove(fname(2),"/",replace="-")
   fname(2)=fname(1)
   fname(1)=string_remove(fname(2),".")
   !
   if (n_sp_pol>1) then 
     if (i_spin==1) fname(2)=trim(fname(1))//"-spin-dn.fit"
     if (i_spin==2) fname(2)=trim(fname(1))//"-spin-up.fit"
     if (i_spin==1) fname(1)=trim(fname(1))//"-spin-dn.data"
     if (i_spin==2) fname(1)=trim(fname(1))//"-spin-up.data"
   else
     fname(2)=trim(fname(1))//".fit"
     fname(1)=trim(fname(1))//".data"
   endif
   call of_open_close(fname(1),'ot')
   call of_open_close(fname(2),'ot')
   call msg('o .fit .data','#')
   do i_d=1,qp%n_descs
     call msg('o .fit .data','#'//trim(qp%description(i_d)))
   enddo
   !
   n_titles=1
   titles(1)="Eo [eV]"
   if (l_E) then
     n_titles=n_titles+1
     titles(n_titles)="DeltaE [eV]"
   endif
   if (l_W) then
     n_titles=n_titles+1
     titles(n_titles)="Width [meV]"
   endif
   if (l_Z) then
     n_titles=n_titles+1
     titles(n_titles)="Re(Z)"
     n_titles=n_titles+1
     titles(n_titles)="Im(Z)"
   endif
   !
   call msg('o .fit .data','#')
   call msg('o .fit .data','#',titles(:n_titles),INDENT=0,USE_TABS=.TRUE.)
   call msg('o .fit .data','#')
   !
   call sort(arrin=qp%E_bare,arrout=E_sorted,indx=E_index)
   !
   do i_s=1,qp%n_states
     !
     if (spin(qp%table( E_index(i_s) ,:))/=i_spin) cycle
     !
     n_titles=1
     value(1)=E_sorted(i_s)*HA2EV
     if (l_E) then
       n_titles=n_titles+1
       value(n_titles)=real(qp%E( E_index(i_s) )-qp%E_bare( E_index(i_s) ))*HA2EV
     endif
     if (l_W) then
       n_titles=n_titles+1
       value(n_titles)=aimag(qp%E( E_index(i_s) ))*HA2EV*1000.
     endif
     if (l_Z) then
       n_titles=n_titles+1
       value(n_titles)=real(qp%Z( E_index(i_s) ))
       n_titles=n_titles+1
       value(n_titles)=aimag(qp%Z( E_index(i_s) ))
     endif
     call msg('o .data','',value(:n_titles),INDENT=-2,USE_TABS=.TRUE.)
   enddo
   !
   do i_s=1,n_steps
     Eo=minval(E_sorted)+( maxval(E_sorted)-minval(E_sorted))/real(n_steps,SP)*real(i_s,SP)
     n_titles=1
     value(1)=Eo*HA2EV
     if (l_E) then
       n_titles=n_titles+1
       value(n_titles)=QP_eval_the_FIT(i_ACTION,i_spin,Eo,E_ref,bands,en,k,"E")*HA2EV
     endif
     if (l_W) then
       n_titles=n_titles+1
       value(n_titles)=QP_eval_the_FIT(i_ACTION,i_spin,Eo,E_ref,bands,en,k,"W")*HA2EV*1000.
     endif
     if (l_Z) then
       n_titles=n_titles+1
       value(n_titles)=real(QP_eval_the_FIT(i_ACTION,i_spin,Eo,E_ref,bands,en,k,"Z"))
       n_titles=n_titles+1
       value(n_titles)=aimag(QP_eval_the_FIT(i_ACTION,i_spin,Eo,E_ref,bands,en,k,"Z"))
     endif
     call msg('o .fit','',value(:n_titles),INDENT=-2,USE_TABS=.TRUE.)
   enddo
   !
   call of_open_close(fname(1))
   call of_open_close(fname(2))
   !
 enddo
 !
end subroutine

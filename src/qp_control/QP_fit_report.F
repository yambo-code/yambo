!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine QP_fit_report(i_ACTION,what)
 !
 use pars,          ONLY:SP,cONE,lchlen
 use units,         ONLY:HA2EV
 use com,           ONLY:msg
 use QP_CTL_m,      ONLY:QP_action
 use electrons,     ONLY:n_sp_pol
 !
 implicit none
 !
 integer         ::i_ACTION
 character(*)    ::WHAT
 !
 ! Work Space
 !
 integer          ::i_spin
 real(SP)         ::db_scissor,fit_scissor,E(4),E_err(2),W(6),W_err(6)
 character(lchlen)::ch
 complex(SP)      ::Z
 !
 do i_spin=1,n_sp_pol
   !
   db_scissor=QP_action(i_ACTION)%CTL(i_spin)%db_scissor
   fit_scissor=QP_action(i_ACTION)%CTL(i_spin)%fit_scissor
   E=QP_action(i_ACTION)%CTL(i_spin)%E
   E_err=QP_action(i_ACTION)%CTL(i_spin)%E_err
   W=QP_action(i_ACTION)%CTL(i_spin)%W
   W_err=QP_action(i_ACTION)%CTL(i_spin)%W_err
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
     call msg(' r','                     (FIT) [ev]:',fit_scissor*HA2EV)
   endif
   !
   if (any(E/=(/0._SP,1._SP,0._SP,1._SP/))) then
     call msg(' r',' Energies 0th order        [ev]:',(/E(1),E(3)/)*HA2EV)
     call msg(' r',' Energies 1st order            :',(/E(2),E(4)/))
     if (any((/E_err/=0._SP/)))then
       call msg(' r','                          Error:',E_err)
     endif
   endif
   !
   if (any((/W/=0._SP/))) then
     call msg(' r',' Widths 0th order          [ev]:',(/W(1),W(4)/)*HA2EV)
     if (any((/W_err(1:2)/=0._SP/))) then
       call msg(' r','                          Error:',W_err(1:2)*HA2EV)
     endif
     if (index(QP_action(i_ACTION)%desc,"E-P")>0) then
       call msg(' r',' Widths dos prop.          [eV]:',(/W(2),W(5)/)*HA2EV)
       if (any((/W_err(3:4)/=0._SP/))) then
         call msg(' r','                          Error:',W_err(3:4)*HA2EV)
       endif
     endif
     call msg(' r',' Widths 2st order        [1/eV]:',(/W(3),W(6)/)/HA2EV)
     if (any((/W_err(5:6)/=0._SP/))) then
       call msg(' r','                          Error:',W_err(5:6)/HA2EV)
     endif
   endif
   if (Z/=cONE) then
     call msg(' r',  ' Renormalization           [Re]:',real(Z))
     call msg(' r',  '                           [Im]:',aimag(Z))
   endif
   !
 enddo
 call msg('rn'," ")
 !
end subroutine

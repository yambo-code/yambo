!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP AM
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
subroutine PH_Transitions_Report( )
 !
 use pars,           ONLY:SP
 use units,          ONLY:HA2EV
 use stderr,         ONLY:intc
 use parallel_m,     ONLY:PARs_PH_Q_ibz
 use parallel_int,   ONLY:PP_redux_wait
 use QP_m,           ONLY:QP_G_damp
 use PHEL,           ONLY:PH_Q,PHEL_broad_factor,PHEL_N_trans_yes,PHEL_N_trans_tot,PHEL_nTrans_percent,PHEL_Q_Radius
 use com,            ONLY:msg
 !
 implicit none
 !
 ! Work Space
 !
 integer  :: iq
 real(SP) :: ratio
 !
 call msg('r','Energy Window',PHEL_broad_factor*QP_G_damp*HA2EV,"[eV]")
 !
 call PP_redux_wait(PHEL_N_trans_yes,COMM=PARs_PH_Q_ibz%COMM_i%COMM)
 call PP_redux_wait(PHEL_N_trans_tot,COMM=PARs_PH_Q_ibz%COMM_i%COMM)
 call PP_redux_wait(PHEL_nTrans_percent,COMM=PARs_PH_Q_ibz%COMM_i%COMM)
 call PP_redux_wait(PHEL_Q_Radius,COMM=PARs_PH_Q_ibz%COMM_i%COMM)
 !
 do iq=PH_Q(1),PH_Q(2)
   !
   call msg('r'," ")
   !
   ! Energy dictated
   if (PHEL_N_trans_tot(iq)>0) then
     ratio = float(PHEL_N_trans_yes(iq))/float(PHEL_N_trans_tot(iq))*100._SP
     call msg('r','[Q='//trim(intc(iq))//'] Transitions included [Energy Constrain]  ',ratio,"[o/o]")
   endif
   !
   ! Momentum dictated
   if (PHEL_nTrans_percent(iq)>0) then
     call msg('r','[Q='//trim(intc(iq))//'] Transitions included [Momentum Constrain]',PHEL_nTrans_percent(iq),"[o/o]")
     call msg('r','[Q='//trim(intc(iq))//'] Radius over smallest radius              ',PHEL_Q_Radius(iq))
   endif
   !
 enddo
 !
end subroutine PH_Transitions_Report

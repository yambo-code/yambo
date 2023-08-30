!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): FP AM
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

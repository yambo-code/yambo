!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP
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
!
subroutine EXCPH_eval_sat(iq,q)
 !
 use pars,           ONLY:SP,cZERO,rZERO
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:nsym
 use ELPH,           ONLY:elph_branches,PH_freqs_sq,FAN_deltaE_treshold,Q_io_map
 use EXCPH,          ONLY:BS_Sat_E,BS_Sat_E_PH_abs,BS_Sat_WEIGHT,BS_Sat_WEIGHT_PH_abs, &
&                         EXCPH_Renorm,EXCPH_Renorm_PH_abs,BS_E,BS_E_in,EXCPH_Gkkp_sq, &
&                         EXCPH_sum,EXCPH_states,DB_Q_map
 !
#include<memory.h>
 !
 integer,       intent(in) :: iq
 type(bz_samp), intent(in) :: q
 !
 ! Work space
 !
 integer  :: i_alpha,i_beta,il,iq_db
 real(SP) :: ph_E
 !
 ! Calculate satellites weights and renormalization factors without double-grid
 !
 do il=elph_branches(1),elph_branches(2)
   !
   !iq_db=DB_Q_map(iq)
   iq_db=Q_io_map(iq)
   ph_E=sqrt(abs(PH_freqs_sq(iq_db,il)))
   if(ph_E<FAN_deltaE_treshold) cycle
   !
   do i_alpha=EXCPH_states(1),EXCPH_states(2)
     do i_beta=EXCPH_sum(1),EXCPH_sum(2)
       !
       BS_Sat_E       (il,i_beta,i_alpha)=BS_E(i_beta)-BS_E_in(i_alpha)+ph_E
       BS_Sat_E_PH_abs(il,i_beta,i_alpha)=BS_E(i_beta)-BS_E_in(i_alpha)-ph_E
       !
       if(abs(BS_Sat_E(il,i_beta,i_alpha))>FAN_deltaE_treshold) &
&         BS_Sat_WEIGHT(il,i_beta,i_alpha)       =EXCPH_Gkkp_sq(il,i_beta,i_alpha)/abs(BS_Sat_E(il,i_beta,i_alpha))**2
       if(abs(BS_Sat_E_PH_abs(il,i_beta,i_alpha))>FAN_deltaE_treshold) &
&         BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)=EXCPH_Gkkp_sq(il,i_beta,i_alpha)/abs(BS_Sat_E_PH_abs(il,i_beta,i_alpha))**2
       !
       EXCPH_Renorm(i_alpha)        = EXCPH_Renorm(i_alpha)        + BS_Sat_WEIGHT(il,i_beta,i_alpha)/real(q%nbz,SP)
       EXCPH_Renorm_PH_abs(i_alpha) = EXCPH_Renorm_PH_abs(i_alpha) + BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)/real(q%nbz,SP)
       !
     enddo
   enddo
   !
 enddo
 !
end subroutine

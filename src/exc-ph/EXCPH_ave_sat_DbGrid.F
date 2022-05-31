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
subroutine EXCPH_ave_sat_DbGrid(iq_bz,q)
 !
 use pars,           ONLY:SP,cZERO,rZERO,cI
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:nsym
 use ELPH,           ONLY:elph_branches,PH_freqs_sq,FAN_deltaE_treshold,Q_io_map
 use EXCPH,          ONLY:BS_Sat_E,BS_Sat_E_PH_abs,BS_Sat_WEIGHT,BS_Sat_WEIGHT_PH_abs, &
&                         BS_E,BS_E_in,EXCPH_Gkkp_sq,EXCPH_sum,EXCPH_states,DB_Q_map,  &
&                         L_damping,EXC_E_DbGd
 !
#include<memory.h>
 !
 integer,       intent(in) :: iq_bz
 type(bz_samp), intent(in) :: q
 !
 ! Work space
 !
 integer     :: i_alpha,i_beta,il,iq_db,iq_fg,nq_around,iq_idx
 real(SP)    :: ph_E,weight_DbGd,E_alpha,E_beta
 complex(SP) :: W_DG,W_DG_PH_abs
 real(SP), external :: get_ph_E_DbGd
 !
 nq_around  =q%FGbz%k_range(iq_bz,2)-q%FGbz%k_range(iq_bz,1)+1 ! Number of fine q-points around iq_bz
 weight_dbgd=1._SP/real(nq_around,SP)
 !
 ! Calculate satellites weights and renormalization factors without double-grid
 !
 do i_alpha=EXCPH_states(1),EXCPH_states(2)
   !
   E_alpha =BS_E_in(i_alpha)
   !
   do i_beta=EXCPH_sum(1),EXCPH_sum(2)
     !
     do il=elph_branches(1),elph_branches(2)
       !
       W_DG=cZERO
       W_DG_PH_abs=cZERO
       !
       ! Renormalize satellite weight with double-grid
       ! 
       do iq_fg=q%FGbz%k_range(iq_bz,1),q%FGbz%k_range(iq_bz,2)
         !
         ph_E=get_ph_E_DbGd(il,iq_fg,iq_bz,q)
         !
         iq_idx=iq_fg-q%FGbz%k_range(iq_bz,1)+1
         !
         ! Excitonic energies in the double-grid
         !
         E_beta  =EXC_E_DbGd(i_beta,iq_fg)
         !
         W_DG       =W_DG + 1._SP/(E_beta-E_alpha+ph_E+cI*L_damping)
         W_DG_PH_abs=W_DG_PH_abs + 1._SP/(E_beta-E_alpha-ph_E+cI*L_damping)
         ! 
       enddo
       !
       W_DG       =W_DG       *weight_dbgd*       abs(BS_Sat_E(il,i_beta,i_alpha))
       W_DG_PH_abs=W_DG_PH_abs*weight_dbgd*abs(BS_Sat_E_PH_abs(il,i_beta,i_alpha))
       !
       BS_Sat_WEIGHT(il,i_beta,i_alpha)       =BS_Sat_WEIGHT(il,i_beta,i_alpha)       *abs(W_DG)**2
       BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)=BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)*abs(W_DG_PH_abs)**2
       !
     enddo  ! il
   enddo  ! i_beta
 enddo ! i_alpha
 !
end subroutine

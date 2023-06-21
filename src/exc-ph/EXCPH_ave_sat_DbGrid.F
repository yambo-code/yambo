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
 use ELPH,           ONLY:elph_branches,FAN_deltaE_treshold,Q_io_map,PH_freqs,PH_freqs_sq
 use EXCPH,          ONLY:BS_Sat_E,BS_Sat_E_PH_abs,BS_Sat_WEIGHT,BS_Sat_WEIGHT_PH_abs,EXCPH_deltaE_treshold, &
&                         BS_E_in,EXCPH_Gkkp_sq,EXCPH_sum,EXCPH_states,L_damping,EXC_E_DbGd
 !
#include<memory.h>
 !
 integer,       intent(in) :: iq_bz
 type(bz_samp), intent(in) :: q
 !
 ! Work space
 !
 integer     :: i_alpha,i_beta,il,iq_db,iE_fg,iq_fg,nq_around
 real(SP)    :: ph_E,weight_dbgd,weight_dbgd_PH_abs,E_alpha,E_beta
 complex(SP) :: W_DG,W_DG_PH_abs
 real(SP), external :: get_ph_E_DbGd
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
       weight_dbgd        =rZERO
       weight_dbgd_PH_abs =rZERO
       !
       ! Renormalize satellite weight with double-grid
       !
       do iq_fg=q%FGbz%k_range(iq_bz,1),q%FGbz%k_range(iq_bz,2)
         !
         if(iq_fg==q%FGbz%k_range(iq_bz,1)) then
           !      
           iq_db=Q_io_map(iq_bz)
           ph_E=sqrt(abs(PH_freqs_sq(iq_db,il)))
           !
           ! If there is the double-grid I read the gamma phonons from there
           ! because they include the LO-TO splitting and the acustic sum rule
           !
           if(iq_bz==1) ph_E=PH_freqs%FG%E(il,1,1)
           !
         else
           iE_fg=q%FGbz%E_map(iq_fg)                      ! Map w(q) from BZ to IBZ
           ph_E =PH_freqs%FG%E(il,iE_fg,1)                ! w(q_db) phonon energy in the D-Grid
         endif
         !
         ! Excitonic energies in the double-grid
         !
         E_beta  =EXC_E_DbGd(i_beta,iq_fg)
         !
         if(abs(E_beta-E_alpha+ph_E)>EXCPH_deltaE_treshold) then
           W_DG        =W_DG + 1._SP/(E_beta-E_alpha+ph_E+cI*L_damping)
           weight_dbgd =weight_dbgd+1._SP
         endif
         !  
         if(abs(E_beta-E_alpha-ph_E)>EXCPH_deltaE_treshold) then
           W_DG_PH_abs        =W_DG_PH_abs       + 1._SP/(E_beta-E_alpha-ph_E+cI*L_damping)
           weight_dbgd_PH_abs =weight_dbgd_PH_abs+1._SP
         endif
         ! 
       enddo
       !
       if(weight_dbgd/=rZERO) then
         W_DG       =W_DG       *1._SP/weight_dbgd       *abs(BS_Sat_E(il,i_beta,i_alpha))
         BS_Sat_WEIGHT(il,i_beta,i_alpha)       =BS_Sat_WEIGHT(il,i_beta,i_alpha)       *abs(W_DG)**2
       endif
       !
       if(weight_dbgd_PH_abs/=rZERO) then
         W_DG_PH_abs=W_DG_PH_abs*1._SP/weight_dbgd_PH_abs*abs(BS_Sat_E_PH_abs(il,i_beta,i_alpha))
         BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)=BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)*abs(W_DG_PH_abs)**2
       endif
       !
     enddo  ! il
   enddo  ! i_beta
 enddo ! i_alpha
 !
end subroutine

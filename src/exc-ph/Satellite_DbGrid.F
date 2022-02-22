!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AC
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
!> @brief Calculate the phonon-assisted satellite for absorption and emission
!         using double-grid
!
! @param[in]       iq                  q-point
! @param[in]       il                  phonon branch
! @param[in]       i_alpha             incoming exciton
! @param[in]       i_beta              outgoing exciton
! @param[in]       W                   photon energy and damping factor
!
! @param[out]      Sat_absorption      Satellite in absorption
! @param[out]      Sat_emission        Satellite in emission
!!
subroutine Satellite_DbGrid(iq_bz,il,i_alpha,i_beta,W,q,Sat_absorption,Sat_emission)
 !
 use frequency,     ONLY:w_samp
 use R_lattice,     ONLY:bz_samp
 use pars,          ONLY:SP,cZERO,rZERO
 use EXCPH,         ONLY:BS_all_E,BS_E_in,min_EXC_E_DbGd,EXC_E_DbGd, &
&                        BS_Sat_W_DG,BS_Sat_W_DG_PH_abs 
 use ELPH,          ONLY:PH_freqs
 use functions,     ONLY:bose_f,boltzman_f
 !
#include<memory.h>
 !
 integer,       intent(in)  :: il,iq_bz,i_alpha,i_beta
 complex(SP),   intent(in)  :: W
 complex(SP),   intent(out) :: Sat_absorption, Sat_emission
 type(bz_samp), intent(in)  :: q
 !
 ! Work Space
 !
 real(SP)    :: PH_E,bose_factor,E_alpha,boltzm_factor,weight_dbg,E_beta
 complex(SP) :: BS_Sat_E_DbGd,BS_Sat_E_DbGd_PH_abs
 integer     :: nq_around,iq_fg,iE_fg
 complex(SP) :: pole
 !
 nq_around =q%FGbz%k_range(iq_bz,2)-q%FGbz%k_range(iq_bz,1) ! Number of fine q-points around iq_bz
 weight_dbg=1._SP/real(nq_around+1._SP,SP)                  ! Total weight of the Db-grid points including the iq_bz
 !
 E_alpha =BS_E_in(i_alpha)
 !
 do iq_fg=q%FGbz%k_range(iq_bz,1)+1,q%FGbz%k_range(iq_bz,2),1
   !
   iE_fg  =q%FGbz%E_map(iq_fg)                             ! Map w(q) from BZ to IBZ
   ph_E   =PH_freqs%FG%E(il,iE_fg,1)                       ! w(q_db) phonon energy in the D-Grid
   !
   ! Bose function for the phonons
   !
   bose_factor=bose_f(PH_E)
   !
   ! Excitonic energies in the double-grid
   !
   E_beta  =EXC_E_DbGd(i_beta,iq_fg)
   !
   ! Boltzman factor for the luminescence
   !
   boltzm_factor=boltzman_f(E_beta-min_EXC_E_DbGd)
   !
   ! Evaluate BS_Sat energies in the double-grid
   ! 
   BS_Sat_E_DbGd       =E_beta-BS_E_in(i_alpha)+ph_E
   BS_Sat_E_DbGd_PH_abs=E_beta-BS_E_in(i_alpha)-ph_E
   !
   pole=E_alpha+BS_Sat_E_DbGd
   !
   ! Phonon emission in absorption
   !
   Sat_absorption=Sat_absorption+BS_Sat_W_DG(il,i_beta,i_alpha)/(W-pole)*(1._SP+bose_factor)
   !
   ! Phonon emission in luminescence
   !
   Sat_emission  =Sat_emission  +BS_Sat_W_DG(il,i_beta,i_alpha)/(W-pole)*bose_factor*boltzm_factor
   !
   pole=E_alpha+BS_Sat_E_DbGd_PH_abs
   !
   ! Phonon absorption in absorption
   !
   Sat_absorption=Sat_absorption+BS_Sat_W_DG_PH_abs(il,i_beta,i_alpha)/(W-pole)*bose_factor 
   ! 
   ! Phonon absorption in luminescence
   !
   Sat_emission=Sat_emission    +BS_Sat_W_DG_PH_abs(il,i_beta,i_alpha)/(W-pole)*(1._SP+bose_factor)*boltzm_factor
   !
 enddo
 !
 ! Average on the q%FGbz points around iq_bz
 ! 
 Sat_emission  =Sat_emission*weight_dbg
 Sat_absorption=Sat_absorption*weight_dbg
 !
end subroutine Satellite_DbGrid

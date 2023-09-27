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
! @param[in]       iq_bz               q-point in the BZ
! @param[in]       il                  phonon branch
! @param[in]       i_alpha             incoming exciton
! @param[in]       i_beta              outgoing exciton
! @param[in]       W                   photon energy and damping factor
!
! @param[out]      Sat_absorption      Satellite in absorption
! @param[out]      Sat_emission        Satellite in emission
!!
subroutine Satellite_Simple(iq_bz,q,il,i_alpha,i_beta,W,Sat_absorption,Sat_emission)
 !
 use frequency,     ONLY:w_samp
 use units,         ONLY:HA2CMm1
 use electrons,     ONLY:nel_cond
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym
 use pars,          ONLY:SP,cZERO,rZERO
 use EXCPH,         ONLY:BS_Sat_E,BS_Sat_WEIGHT,BS_all_E,BS_Sat_E_PH_abs,BS_Sat_WEIGHT_PH_abs,BS_E_in, &
&                        min_EXC_E,DB_Q_map,EXC_occ
 use functions,     ONLY:bose_f,boltzman_f
 use units,         ONLY:HA2CMm1
 use ELPH,          ONLY:PH_freqs_sq,FAN_deltaE_treshold,Q_io_map
 !
#include<memory.h>
 !
 integer,       intent(in)  :: il,iq_bz,i_alpha,i_beta
 complex(SP),   intent(in)  :: W
 complex(SP),   intent(out) :: Sat_absorption, Sat_emission
 type(bz_samp)              :: q
 !
 ! Work Space
 !
 real(SP)    :: PH_E,bose_factor,boltzm_factor
 complex(SP) :: pole,E_alpha
 integer     :: iq_db,iq_ibz
 !
 Sat_absorption=cZERO
 Sat_emission  =cZERO
 !
 ! Bose function for the phonons
 !
 !iq_db=DB_Q_map(iq_bz)
 iq_db=Q_io_map(iq_bz)
 ph_E=sqrt(abs(PH_freqs_sq(iq_db,il)))
 !
 if(ph_E<FAN_deltaE_treshold) return
 !
 bose_factor=bose_f(ph_E)
 !
 E_alpha=BS_E_in(i_alpha)
 !
 ! Boltzman factor for the luminescence
 !
 iq_ibz = q%sstar(iq_bz,1)
 boltzm_factor=boltzman_f(BS_all_E(i_beta,iq_ibz)-min_EXC_E)
 !
 if(nel_cond/=rZERO) boltzm_factor=EXC_occ(i_beta,iq_ibz)
 !
 pole=E_alpha+BS_Sat_E(il,i_beta,i_alpha)
 !
 ! Phonon emission in absorption [photon absorption / phonon emission]
 !
 Sat_absorption=Sat_absorption+BS_Sat_WEIGHT(il,i_beta,i_alpha)/(W-pole)*(1._SP+bose_factor)
 !
 ! Phonon absorption in luminescence [photon emission / phonon absorption]
 !
 Sat_emission=Sat_emission    +BS_Sat_WEIGHT(il,i_beta,i_alpha)/(W-pole)*bose_factor*boltzm_factor
 !
 pole=E_alpha+BS_Sat_E_PH_abs(il,i_beta,i_alpha)
 !
 ! Phonon absorption in absorption [photon absorption / phonon absorption]
 !
 Sat_absorption=Sat_absorption+BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)/(W-pole)*bose_factor 
 ! 
 ! Phonon emission in luminescence [photon emission / phonon emission]
 !
 Sat_emission=Sat_emission   +BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)/(W-pole)*(1._SP+bose_factor)*boltzm_factor
 !
end subroutine Satellite_Simple

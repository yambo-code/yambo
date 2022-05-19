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
! @param[in]       iq_bz               q-point
! @param[in]       il                  phonon branch
! @param[in]       i_alpha             incoming exciton
! @param[in]       i_beta              outgoing exciton
! @param[in]       W                   photon energy and damping factor
!
! @param[out]      Sat_absorption      Satellite in absorption
! @param[out]      Sat_emission        Satellite in emission
!!
subroutine Satellite_DbGrid(iq_bz,q,il,i_alpha,i_beta,W,Sat_absorption,Sat_emission)
 !
 use frequency,     ONLY:w_samp
 use R_lattice,     ONLY:bz_samp
 use units,         ONLY:HA2EV
 use pars,          ONLY:SP,cZERO,rZERO
 use EXCPH,         ONLY:BS_all_E,BS_E_in,min_EXC_E_DbGd,EXC_E_DbGd,BS_Sat_E,l_DbGd_WEIGHTs, &
&                        BS_Sat_W_DG,BS_Sat_W_DG_PH_abs,l_DbGd_PH_only,BS_Sat_E_PH_abs, &
                         BS_Sat_WEIGHT,BS_Sat_WEIGHT_PH_abs,min_EXC_E,l_no_matrix_elements
 use ELPH,          ONLY:PH_freqs,FAN_deltaE_treshold,PH_freqs_sq,Q_io_map
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
 real(SP)    :: PH_E,E_alpha,boltzm_factor,weight_dbgd,E_beta,BS_ave_W,BS_ave_W_PH_abs,bose_factor
 complex(SP) :: BS_Sat_E_DbGd,BS_Sat_E_DbGd_PH_abs
 integer     :: nq_around,iq_fg,iq_idx,iE_fg,iq_ibz,iq_db
 complex(SP) :: pole
 real(SP), external :: get_ph_E_DbGd
 !
 nq_around  =q%FGbz%k_range(iq_bz,2)-q%FGbz%k_range(iq_bz,1)+1 ! Number of fine q-points around iq_bz
 weight_dbgd=1._SP/real(nq_around,SP)                
 !
 ! Calculate the Satellite Weight
 !
 if(l_DbGd_WEIGHTs) then
   call error('Implementation to be checked ')
 else
   BS_ave_W       =BS_Sat_WEIGHT(il,i_beta,i_alpha)
   BS_ave_W_PH_abs=BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)
 endif
 !
 if(l_no_matrix_elements) then
   BS_ave_W       =1._SP
   BS_ave_W_PH_abs=1._SP
 endif
 !
 Sat_absorption=cZERO
 Sat_emission  =cZERO
 !
 E_alpha =BS_E_in(i_alpha)
 !
 do iq_fg=q%FGbz%k_range(iq_bz,1),q%FGbz%k_range(iq_bz,2),1
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
   if(ph_E<FAN_deltaE_treshold) cycle
   !
   bose_factor=bose_f(ph_E)
   !
   if(l_DbGd_PH_only) then
     !      
     ! Excitonic energies in the course grid
     !
     iq_ibz = q%sstar(iq_bz,1)
     E_beta  =BS_all_E(i_beta,iq_ibz)
     !
     boltzm_factor=boltzman_f(E_beta-min_EXC_E)
     !
   else
     !      
     ! Excitonic energies in the fine-grid
     !
     E_beta  =EXC_E_DbGd(i_beta,iq_fg)
     !
     ! Boltzman factor for the luminescence
     !
     boltzm_factor=boltzman_f(E_beta-min_EXC_E_DbGd)
     !
   endif
   !
   ! Evaluate BS_Sat energies in the double-grid
   !
   BS_Sat_E_DbGd       =E_beta-BS_E_in(i_alpha)+ph_E
   BS_Sat_E_DbGd_PH_abs=E_beta-BS_E_in(i_alpha)-ph_E
   !
   pole=E_alpha+BS_Sat_E_DbGd
   !
   ! Bose factor already included in BS_Sat_W_DG and BS_Sat_W_DG_PH_abs
   !
   ! Phonon emission in absorption
   !
   Sat_absorption=Sat_absorption+BS_ave_W/(W-pole)*(1._SP+bose_factor)
   !
   ! Phonon emission in luminescence
   !
   Sat_emission  =Sat_emission  +BS_ave_W/(W-pole)*boltzm_factor*boltzm_factor
   !
   pole=E_alpha+BS_Sat_E_DbGd_PH_abs
   !
   ! Phonon absorption in absorption
   !
   Sat_absorption=Sat_absorption+BS_ave_W_PH_abs/(W-pole)*bose_factor
   ! 
   ! Phonon absorption in luminescence
   !
   Sat_emission=Sat_emission    +BS_ave_W_PH_abs/(W-pole)*boltzm_factor*(1._SP+bose_factor)
   !
 enddo
 !
 ! Average on the q%FGbz points around iq_bz including the point calculated in Satellite_Simple
 ! 
 Sat_emission  =Sat_emission*weight_dbgd
 Sat_absorption=Sat_absorption*weight_dbgd
 !
end subroutine Satellite_DbGrid

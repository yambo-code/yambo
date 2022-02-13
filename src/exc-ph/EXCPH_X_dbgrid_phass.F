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
!> @brief Calculate X(w) including ph-assisted transitions and double-grid
!
! @param[in]       wv                  frequencies range
! @param[in]       EXCPH_gkkp          read from file
! @param[in]       PH_E                read from file
! @param[in]       Bose_Temp           Bose temperature for phonons
! @param[in]       Boltz_Temp          Boltz temperature for excitons
!
! @param[out]      Epsilon_ii          Phonon-assisted dielectric constant
! @param[out]      PL_ii               Phonon-assisted luminescence
!!
subroutine EXCPH_X_dbgrid_phass(wv,q,PH_freqs,Epsilon_ii,PL_ii)
 !
 use frequency,     ONLY:w_samp
 use R_lattice,     ONLY:bz_samp
 use pars,          ONLY:SP,cZERO,rZERO
 !
#include<memory.h>
 !
 type(w_samp),  intent(in)  :: wv
 type(bz_samp), intent(in)  :: q,PH_freqs
 complex(SP),   intent(out) :: Epsilon_ii(wv%n_freqs,2),PL_ii(wv%n_freqs,2)
 !
end subroutine EXCPH_X_dbgrid_phass

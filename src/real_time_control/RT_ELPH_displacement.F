!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_ELPH_displacement(k,E,dG)
 !
 ! Evalute force potential that define the phonon displacement
 !
 ! D(\nu,t) \propto -\sum_ijk <\psi_i(k)(r)| dV_\nu\(r)|\psi_j(k)(r)> \delta \gr_{ji}(t)
 !
 use pars,           ONLY:SP,cZERO,pi,cI
 use R_lattice,      ONLY:bz_samp,nqbz
 use electrons,      ONLY:levels
 use real_time,      ONLY:l_NE_dynamics,l_RT_PH_oscillations,RT_bands,elph_gkkp_gamma,RT_nk,RT_D,RT_PH_displaced
 use ELPH,           ONLY:ph_freqs_sq,elph_branches,ph_modes
 use RT_interfaces,  ONLY:RT_average_operator
 use parallel_m,     ONLY:PAR_Xk_nibz
 !
 implicit none
 !
 type(bz_samp),     intent(in) :: k
 type(levels),      intent(in) :: E
 complex(SP),       intent(in) :: dG(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 !
 ! Work Spave
 !
 integer     :: il
 real(SP)    :: ph_freq
 !
 ! GPL_EXCLUDE_START
 !
 if ( .not. l_NE_dynamics .or. .not. l_RT_PH_oscillations ) return
 !
 RT_D=cZERO
 !
 call RT_average_operator(N_V=ph_modes,V_VALUE=RT_D,V_MATRIX=elph_gkkp_gamma,G=dG,k=k,en=E,nK_mem=PAR_Xk_nibz)
 !
 do il=elph_branches(1),elph_branches(2)
   !
   if (.not.RT_PH_displaced(il)) cycle
   !
   ph_freq=abs(sqrt(ph_freqs_sq(1,il)))
   !
   RT_D(il)=-1/pi*sqrt(ph_freq/nqbz)*RT_D(il)
   !
 enddo
 !
 ! GPL_EXCLUDE_END
 !
end subroutine RT_ELPH_displacement

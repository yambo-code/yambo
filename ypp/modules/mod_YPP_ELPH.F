!
!        Copyright (C) 2000-2017 the YAMBO team
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
module YPP_ELPH
 !
 use pars,     ONLY:SP,schlen
 !
 ! EL-PH
 !
 logical :: l_gkkp
 logical :: l_gkkp_expand
 logical :: l_phonons
 logical :: l_eliashberg
 logical :: l_atomic_amplitude
 integer :: elph_steps
 real(SP):: elph_gamma_broad
 real(SP):: ph_broad
 real(SP):: elph_Ef
 real(SP):: elph_E_max
 real(SP):: elph_residual_treshold = 0.
 character(schlen) :: elph_dbs_path
 character(schlen) :: ph_freqs_file
 character(schlen) :: ph_modes_file
 !
 interface
   !
   subroutine  ELPH_general_gFsq(k,en,Xk,Xen,q,BS_E_degs,lambda_s,n_lambda)
     use R_lattice,     ONLY:bz_samp
     use electrons,     ONLY:levels
     implicit none
     type(bz_samp)      :: k,q,Xk
     type(levels)       :: en,Xen
     integer, optional  :: BS_E_degs(:)
     integer, optional  :: lambda_s(:)
     integer, optional  :: n_lambda
   end subroutine
   !
 end interface
 !
end module YPP_ELPH

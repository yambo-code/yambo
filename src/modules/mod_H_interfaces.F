!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
module H_interfaces
 !
 implicit none
 !
 interface
   !
   subroutine V_real_space_to_H(ik,i_sp_pol,H,WF,V_kind,V,Vxc)
     use pars,           ONLY:SP
     use wave_func,      ONLY:WAVEs
     use electrons,      ONLY:n_spin
     use hamiltonian,    ONLY:H_ref_bands
     use FFT_m,          ONLY:fft_size
     integer,      intent(in)              :: ik,i_sp_pol
     complex(SP),  intent(inout)           :: H(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
     type(WAVEs),  intent(in)              :: WF
     character(*), intent(in)              :: V_kind
     real(SP),     intent(in), optional    :: V(fft_size,n_spin)
     real(SP),     intent(in), optional    :: Vxc(fft_size,n_spin)
   end subroutine
   !
 end interface
 !
end module

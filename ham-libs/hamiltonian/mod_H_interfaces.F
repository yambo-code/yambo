!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
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

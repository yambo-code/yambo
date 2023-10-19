!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
module NL_interfaces
 !
 implicit none
 !
 interface
   !
   subroutine NL_average_operator(O_VALUE,V_VALUE,O_REAL,O_MATRIX,V_MATRIX,PV_MATRIX,V_bands,k,en,TRANS,nK_mem)
     !
     use pars,           ONLY:SP
     use nl_optics,      ONLY:NL_bands
     use R_lattice,      ONLY:bz_samp
     use QP_m,           ONLY:QP_nk
     use electrons,      ONLY:n_sp_pol,n_spin,levels
     use FFT_m,          ONLY:fft_size
     !
     implicit none
     !
     type(bz_samp), intent(in) :: k
     type(levels),  intent(in) :: en
     complex(SP),   intent(in) :: V_bands(NL_bands(2),maxval(en%nbf),QP_nk, n_sp_pol)
     integer,       intent(in) :: nK_mem
     !
     ! Optionals
     !
     complex(SP),     optional, intent(out):: O_VALUE,V_VALUE(3)
     complex(SP),     optional, intent(in) :: PV_MATRIX(3,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nK_mem,n_sp_pol)
     complex(SP),     optional, intent(in) ::  V_MATRIX(3,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nK_mem,n_sp_pol)
     complex(SP),     optional, intent(in) ::  O_MATRIX(  NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nK_mem,n_sp_pol)
     real(SP),        optional, intent(in) :: O_REAL(fft_size,n_spin)
     logical,         optional, intent(in) :: TRANS
     !
   end subroutine
   !
 end interface
 !
end module

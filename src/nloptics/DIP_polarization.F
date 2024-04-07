!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!                  
subroutine DIP_polarization(Xen,Xk,V_bands,Polarization)
 !
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:n_sp_pol,levels
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp
 use QP_m,           ONLY:QP_nk
 use DIPOLES,        ONLY:DIP_iR
 use nl_optics,      ONLY:NL_bands
 use parallel_m,     ONLY:PAR_Xk_nibz
 use NL_interfaces,  ONLY:NL_average_operator
 !
 implicit none           
 type(bz_samp), intent(in)  :: Xk
 type(levels),  intent(in)  :: Xen
 complex(SP),   intent(in)  :: V_bands(NL_bands(2),maxval(Xen%nbf),QP_nk,n_sp_pol)
 complex(SP),   intent(out) :: Polarization(3)
 !
 ! Evaluate polarization in terms of dipoles only
 ! for testing porpouses
 !
 ! p(r,t) = \sum_ijk e/(2m) \psi^*_i(k) r \psi_j(k)
 !
 call NL_average_operator(V_VALUE=Polarization,V_MATRIX=cI*DIP_iR(:,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),:,:), &
&                         V_bands=V_bands,k=Xk,en=Xen,nk_mem=PAR_Xk_nibz,TRANS=.true.)
 !
 Polarization=Polarization/DL_vol
 !
end subroutine DIP_polarization

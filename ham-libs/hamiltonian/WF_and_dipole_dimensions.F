!
! License-Identifier: GPL
!
! Copyright (C) 2010 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine WF_and_dipole_dimensions(X,Xk)
 !
 use drivers,      ONLY:l_real_time,l_sc_fock,l_sc_exx,l_sc_coh,l_sc_sex,l_sc_srpa,&
&                       l_sc_hartree,l_use_Hxc_collisions
 use com,          ONLY:msg
 use X_m,          ONLY:X_t
 use wave_func,    ONLY:wf_ng
 use QP_m,         ONLY:QP_ng_Sx,QP_ng_SH,QP_ng_Vxc,QP_n_G_bands,COHSEX_use_empties
 use R_lattice,    ONLY:qindx_S,G_m_G,bz_samp,qindx_S_max_Go
 use interfaces,   ONLY:eval_G_minus_G
 use hamiltonian,  ONLY:H_ref_bands,WF_Go_indx,WF_G_max,l_sc_XC_is_local
 !
 implicit none
 !
 type(X_t)         :: X(2)
 type(bz_samp)     :: Xk
 !
 ! Dipoles dimensions
 !===================
 !
 X(1)%ib=H_ref_bands
 X(1)%ng=wf_ng
 !
 ! WF SIZE
 !=========
 !
 WF_G_max=0
 WF_Go_indx=1
 !
end subroutine

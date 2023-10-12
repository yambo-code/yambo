!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QP_load_GFs(bands,qp,E)
 !
 ! Using qp%GreenF/qp%GreenF_W I define here the Green Functions
 !
 use electrons,     ONLY:n_sp_pol,spin,levels
 use QP_m,          ONLY:QP_t,GF_is_causal
 !
#include<memory.h>
 !
 type(levels) ::E
 type(QP_t)   ::qp
 integer      ::bands(2)
 !
 ! Work Space
 !
 integer     :: i_qp,i_band,i_k_pt,i_spin
 !
 E%GreenF_is_causal=GF_is_causal
 E%GreenF_n_E_steps=qp%GreenF_n_steps
 !
 YAMBO_ALLOC(E%GreenF,(bands(2),E%nk,n_sp_pol,E%GreenF_n_E_steps))
 YAMBO_ALLOC(E%GreenF_W,(bands(2),E%nk,n_sp_pol,E%GreenF_n_E_steps))
 !
 do i_qp=1,qp%n_states
   i_band=qp%table(i_qp,1)
   i_k_pt=qp%table(i_qp,3)
   i_spin=spin(qp%table(i_qp,:))
   E%GreenF(i_band,i_k_pt,i_spin,:)=qp%GreenF(i_qp,:)
   E%GreenF_W(i_band,i_k_pt,i_spin,:)=qp%GreenF_W(i_qp,:)
 enddo
 !
end subroutine

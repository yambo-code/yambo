!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SCATTERING_GW_kinematics(i_q_bz,i_m,I,k,q,TABLE,E,N,G0,Ek,Ekmq,E_diff)
 !
 ! m,k-q   n,k
 ! --<--.-<--
 !      | 
 !      |
 !     \|/ q,G,\alpha,...
 !      |
 !      
 ! n   =TABLE(1)
 ! k   =TABLE(3)
 !
 use pars,                ONLY:SP
 use electrons,           ONLY:spin,levels
 use R_lattice,           ONLY:qindx_S,bz_samp
 use collision_el,        ONLY:elemental_collision
 !
 implicit none
 !
 integer,       intent(in) ::i_q_bz,i_m
 integer,       intent(in) ::TABLE(:)
 type(elemental_collision) ::I
 type(bz_samp)             ::k,q
 type(levels),  OPTIONAL   ::E
 integer,       OPTIONAL   ::N,G0
 real(SP),      OPTIONAL   ::Ek,Ekmq,E_diff
 !
 ! WorkSpace
 !
 integer  ::i_n,i_k,i_G0,i_k_m_q,i_k_m_q_bz,i_spin,i_k_m_q_s,i_q,i_q_s
 !
 i_n    = table(1)
 i_k    = table(3)
 i_spin = spin(TABLE)
 !
 i_G0       = qindx_S(i_k,i_q_bz,2)
 i_k_m_q_bz = qindx_S(i_k,i_q_bz,1) 
 i_k_m_q    = k%sstar(i_k_m_q_bz,1)
 i_k_m_q_s  = k%sstar(i_k_m_q_bz,2)
 i_q        = q%sstar(i_q_bz,1)
 i_q_s      = q%sstar(i_q_bz,2)
 !
 I%is=(/i_n,i_k,1,i_spin/)
 I%os=(/i_m,i_k_m_q,i_k_m_q_s,i_spin/)
 I%qs=(/i_G0,i_q,i_q_s/)
 !
 if (present(N)) then
   N=i_n
 endif
 if (present(G0)) then
   G0=i_G0
 endif
 if (present(Ek)) then
   Ek=E%E(i_n,i_k,i_spin)
 endif
 if (present(Ekmq)) then
   Ekmq=E%E(i_m,i_k_m_q,i_spin)
 endif
 if (present(E_diff)) then
   E_diff=E%E(i_n,i_k,i_spin)-E%E(i_m,i_k_m_q,i_spin)
 endif
 !
end subroutine SCATTERING_GW_kinematics


!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!                  
subroutine NL_current(k,E,V_input,NL_J)
 !
 ! Evalute current 
 ! The current includes the diamagnetic contribution only in the velocity gauge.
 !
 ! j(r,t) = -\sum_ijk e/(2m) \psi^*_i(k) p \psi_j(k) - e^2/(cm) n(r) A(r,t)
 ! p(r,t) = -\sum_ijk e/(2m) \psi^*_i(k) r \psi_j(k)
 !
 use pars,           ONLY:SP,cZERO
 use units,          ONLY:SPEED_OF_LIGHT
 use R_lattice,      ONLY:bz_samp,k_map
 use D_lattice,      ONLY:DL_vol
 use electrons,      ONLY:nel,levels,n_sp_pol
 use fields,         ONLY:A_ext
 use X_m,            ONLY:global_gauge
 use DIPOLES,        ONLY:DIP_v
 use nl_optics,      ONLY:NL_bands,l_use_DIPOLES
 use QP_m,           ONLY:QP_nk
 use NL_interfaces,  ONLY:NL_average_operator
 use parallel_m,     ONLY:PAR_Xk_nibz
 !
 implicit none          
 ! 
 type(bz_samp),     intent(in)  :: k
 type(levels),      intent(in)  :: E
 complex(SP),       intent(in)  :: V_input(NL_bands(2),maxval(E%nbf),QP_nk,n_sp_pol) 
 complex(SP),       intent(out) :: NL_J(6)
 !
 ! Work Space
 !
 NL_J=cZERO
 !
 ! A : Current
 !
 call NL_average_operator(V_VALUE=NL_J(4:6), V_MATRIX=DIP_v(:,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),:,:),  &
     &  V_bands=V_input,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_Xk_nibz)
 !
 if(trim(global_gauge)=='velocity') then
   !
   ! Velocity gauge I must use the correct definition of the J operator
   !
   NL_J(4:6)=NL_J(4:6)+A_ext%vecpot(:)*real(nel,SP)/(2._SP*SPEED_OF_LIGHT)
   !
   NL_J(1:3)=NL_J(4:6)
   !
 else
   !
   if(l_use_DIPOLES) then
     NL_J(1:3)=NL_J(4:6)
   else
     call NL_Berry_current(E,k,k_map,V_input,NL_J(1:3))
   endif
   !
 endif
 !
 NL_J=NL_J/DL_vol
 !
end subroutine NL_current

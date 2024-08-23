!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_rotate_in_RF(ik,rotation_sign,dG_in)
 !
 ! Comment found here. Dunno why. To be moved [AM 23/08/2024]
 !
 ! It is higly non satisfactory that
 !     Gamma_neq[f_eq] /= Gamma_eq                              (1)
 ! This explains the need for the special definition
 !    sumGAMMA=abs(E%W(i_n,ik,1))+abs(E%W(i_np,ik,1))           (2)
 ! instead of
 !    sumGAMMA=sum(REF_lifetime(i_n,ik,:))                     (3)
 ! with i_n=i_np.
 ! 
 ! This difference is due to two reasons:
 ! (a) For the ee scatt REF_lifetimes are computed with a static approx to
 !     the out-of-equilibrium GW
 ! (b) The Gamma_eq are computed in the \eta->0 limit, i.e. no QP approx,
 !     while the Gamma_neq are computed in the QP approx \eta=Gamma_eq
 !
 use pars,           ONLY:SP,cONE
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:RT_levels,RT_Deph_Matrix,RT_deph_range,RTibz,&
&                         l_RT_EXP,RT_is_statically_dephased,RT_bands,NE_time
 !
 implicit none
 !
 integer,      intent(in)     :: ik
 real(SP),     intent(in)     :: rotation_sign
 complex(SP),  intent(in)     :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Work space
 !
 integer     :: i_n,i_np
 real(SP)    :: deltaE,sumGAMMA
 complex(SP) :: phase
 !
 phase  =cONE
 !
 do i_n=RT_bands(1),RT_bands(2)
   do i_np=i_n,RT_bands(2)
     !
     sumGAMMA=0._SP
     !
     if (RT_is_statically_dephased) sumGAMMA=RT_Deph_Matrix(i_n,i_np,RTibz%k_map(ik))
     !
     if (NE_time<RT_deph_range(1).and.RT_deph_range(1)>0._SP) sumGAMMA=0._SP
     if (NE_time>RT_deph_range(2).and.RT_deph_range(2)>0._SP) sumGAMMA=0._SP
     !
     deltaE=RT_levels%E(i_n,ik,1)-RT_levels%E(i_np,ik,1)
     !
     dG_in(i_n,i_np)=dG_in(i_n,i_np)*exp(rotation_sign*(deltaE-sumGAMMA)
     !
!     if (dT/=dTp) then
!       !
!       alpha=-deltaE*dT
!       phase=cmplx(cos(alpha),sin(alpha))
!       dec_exp=exp(-sumGAMMA*dT)
!       !
!     endif
!     !
!                   dG_out(i_n,i_np)=dG_out(i_n,i_np)+dG_in(i_n,i_np)*      phase *dec_exp
!     if(i_np/=i_n) dG_out(i_np,i_n)=dG_out(i_np,i_n)+dG_in(i_np,i_n)*conjg(phase)*dec_exp
!     !
   enddo
   !
 enddo
 !
end subroutine RT_apply_RWA

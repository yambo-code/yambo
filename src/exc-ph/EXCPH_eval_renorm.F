!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP CA 
!
! headers
!
#include<y_memory.h>
!
!
! @param[in]       iq_bz               iq_bz index 
! @param[in]       q                   bz sampling
!
! @param[out]      R                   renormalization factor
! @param[out]      R_PH_abs            renormalization factor for phonon-absorption
! @param[out]      R_PL                luminescence renormalization factor
! @param[out]      R_PL_PH_abs         luminescence renormalization factor for phonon-absorption
!
subroutine EXCPH_eval_renorm(iq_bz,q,R,R_PH_abs,R_PL,R_PL_PH_abs)
 !
 use pars,           ONLY:SP,cZERO,rZERO
 use R_lattice,      ONLY:bz_samp
 use functions,      ONLY:bose_f
 use ELPH,           ONLY:elph_branches,PH_freqs_sq,FAN_deltaE_treshold,Q_io_map
 use EXCPH,          ONLY:BS_Sat_WEIGHT,BS_Sat_WEIGHT_PH_abs,EXCPH_sum,EXCPH_states
 !
 use y_memory_alloc
 !
 implicit none
 !
 integer,       intent(in)    :: iq_bz
 type(bz_samp), intent(in)    :: q
 real(SP),      intent(inout) :: R(EXCPH_states(2)),   R_PH_abs(EXCPH_states(2))
 real(SP),      intent(inout) :: R_PL(EXCPH_states(2)),R_PL_PH_abs(EXCPH_states(2))
 !
 ! Work space
 !
 integer  :: i_alpha,i_beta,il,iq_db
 real(SP) :: ph_E,N_bose
 !
 ! Add the contribution of the iq statellites to the renormalization factors
 !
 do il=elph_branches(1),elph_branches(2)
   !
   iq_db=Q_io_map(iq_bz)
   ph_E=sqrt(abs(PH_freqs_sq(iq_db,il)))
   if(ph_E<FAN_deltaE_treshold) cycle
   !
   N_bose=bose_f(ph_E)
   !
   do i_alpha=EXCPH_states(1),EXCPH_states(2)
     do i_beta=EXCPH_sum(1),EXCPH_sum(2)
       !
       ! Phonon emission in absorption [photon absorption / phonon emission]
       !
       R(i_alpha)          = R(i_alpha) + BS_Sat_WEIGHT(il,i_beta,i_alpha)*(1._SP+N_bose)/real(q%nbz,SP)
       !
       ! Phonon absorption in luminescence [photon emission / phonon absorption]
       !
       R_PL_PH_abs(i_alpha) = R_PL_PH_abs(i_alpha) + BS_Sat_WEIGHT(il,i_beta,i_alpha)*(1._SP+N_bose)/real(q%nbz,SP)
       !
       ! Phonon absorption in absorption [photon absorption / phonon absorption]
       !
       R_PH_abs(i_alpha)    = R_PH_abs(i_alpha) + BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)*N_bose/real(q%nbz,SP)
       !
       ! Phonon emission in luminescence [photon emission / phonon emission]
       !
       R_PL(i_alpha)        = R_PL(i_alpha) + BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)*N_bose/real(q%nbz,SP)
       !
     enddo
   enddo
   !
 enddo
 !
end subroutine

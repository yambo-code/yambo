!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP
!
! headers
!
#include<y_memory.h>
!
!
subroutine EXCPH_eval_sat(iq_bz,q)
 !
 use pars,           ONLY:SP,cZERO,rZERO
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:nsym
 use ELPH,           ONLY:elph_branches,PH_freqs_sq,FAN_deltaE_treshold,Q_io_map
 use EXCPH,          ONLY:BS_Sat_E,BS_Sat_E_PH_abs,BS_Sat_WEIGHT,BS_Sat_WEIGHT_PH_abs, &
&                         BS_all_E,BS_E_in,EXCPH_Gkkp_sq,EXCPH_sum,EXCPH_states,DB_Q_map, &
&                         l_no_matrix_elements,EXCPH_deltaE_treshold
 !
 use y_memory_alloc
 !
 implicit none
 !
 integer,       intent(in) :: iq_bz
 type(bz_samp), intent(in) :: q
 !
 ! Work space
 !
 integer  :: i_alpha,i_beta,il,iq_db,iq_ibz
 real(SP) :: ph_E,E_alpha,E_beta,EXCPH_gaux
 !
 ! Calculate satellites weights and renormalization factors without double-grid
 !
 iq_ibz=q%sstar(iq_bz,1)
 !
 BS_Sat_WEIGHT       =rZERO
 BS_Sat_WEIGHT_PH_abs=rZERO
 !
 do il=elph_branches(1),elph_branches(2)
   !
   iq_db=Q_io_map(iq_bz)
   ph_E=sqrt(abs(PH_freqs_sq(iq_db,il)))
   if(ph_E<FAN_deltaE_treshold) cycle
   !
   do i_alpha=EXCPH_states(1),EXCPH_states(2)
     do i_beta=EXCPH_sum(1),EXCPH_sum(2)
       !
       E_alpha=BS_E_in(i_alpha)
       E_beta =BS_all_E(i_beta,iq_ibz)
       !
       BS_Sat_E       (il,i_beta,i_alpha)=E_beta-E_alpha+ph_E
       BS_Sat_E_PH_abs(il,i_beta,i_alpha)=E_beta-E_alpha-ph_E
       !
       EXCPH_gaux=EXCPH_Gkkp_sq(il,i_beta,i_alpha)
       if(l_no_matrix_elements) EXCPH_gaux=1._SP
       !
       if(abs(BS_Sat_E(il,i_beta,i_alpha))>EXCPH_deltaE_treshold) &
&         BS_Sat_WEIGHT(il,i_beta,i_alpha)       =EXCPH_gaux/abs(BS_Sat_E(il,i_beta,i_alpha))**2
       !
       if(abs(BS_Sat_E_PH_abs(il,i_beta,i_alpha))>EXCPH_deltaE_treshold) &
&         BS_Sat_WEIGHT_PH_abs(il,i_beta,i_alpha)=EXCPH_gaux/abs(BS_Sat_E_PH_abs(il,i_beta,i_alpha))**2
       !
     enddo
   enddo
   !
 enddo
 !
end subroutine

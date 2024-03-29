!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine RT_mean_lifetimes_and_temperatures( LIFE, LIFE_ref )
 ! 
 use pars,           ONLY:SP,rZERO
 use RT_operations,  ONLY:ABS_RT_life_one_KIND,EMIT_RT_life_one_KIND
 use RT_lifetimes,   ONLY:RT_lifetime,l_RT_lifetimes_Tfit
 use real_time,      ONLY:RTibz,RT_nk,RT_bands
 use electrons,      ONLY:spin_occ,n_full_bands
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX
 use parallel_int,   ONLY:PP_redux_wait
 use units,          ONLY:HA2KEL
 !
 implicit none
 !
 type(RT_lifetime), intent(inout) :: LIFE, LIFE_ref
 !
 ! Work space
 !
 integer            :: ib,ik
 real(SP)           :: n_states(1),pre_factor,ABS_gamma,TOT_gamma
 real(SP)           :: occupations(RT_bands(1):RT_bands(2),RT_nk),Efit(2)
 logical            :: l_RT_lifetimes_Tfit_LOC
 !
 if (.not.LIFE%active) return
 !
 l_RT_lifetimes_Tfit_LOC=l_RT_lifetimes_Tfit
 if (l_RT_lifetimes_Tfit) then
   if (trim(LIFE%kind)=="el-photon".or.trim(LIFE%kind)=="el-ph_abs") l_RT_lifetimes_Tfit_LOC=.FALSE.
   if (l_RT_lifetimes_Tfit_LOC) then
     occupations=rZERO
     occupations(RT_bands(1):n_full_bands(1),:)=spin_occ
   endif
 endif
 !
 n_states(1)  =rZERO
 LIFE%mean    =rZERO
 occupations  =rZERO
 !
 do ik=1,RT_nk
   !
   if( .not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
   !
   pre_factor=RTibz%weights(ik)
   !
   do ib=RT_bands(1),RT_bands(2)
     !
     n_states(1)=n_states(1)+pre_factor
     !
     if (l_RT_lifetimes_Tfit_LOC) then
       ABS_gamma = ABS_RT_life_one_KIND(LIFE,LIFE_ref,ib,ik)
       TOT_gamma = ABS_gamma+EMIT_RT_life_one_KIND(LIFE,LIFE_ref,ib,ik)
       if(TOT_gamma>rZERO) then
         occupations(ib,ik)=ABS_gamma/TOT_gamma*spin_occ
       endif
     endif
     !
     LIFE%mean(1)=LIFE%mean(1)+(LIFE%abs(ib,ik)+LIFE_ref%abs(ib,ik))*pre_factor
     LIFE%mean(2)=LIFE%mean(2)+(LIFE%emit(ib,ik)+LIFE_ref%emit(ib,ik))*pre_factor
     !
   enddo
   !
 enddo
 !
 call PP_redux_wait(LIFE%mean,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(n_states,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 LIFE%mean=LIFE%mean/n_states(1)
 !
 if (.not.l_RT_lifetimes_Tfit_LOC) return
 !
 LIFE%Tfit=rZERO
 Efit=rZERO
 !
 call RT_occupations_FIT(LIFE%Tfit,Efit,occupations)
 if (LIFE%Tfit(1)>50000/HA2KEL.or.LIFE%Tfit(1)<0._SP) LIFE%Tfit(1)=0._SP
 if (LIFE%Tfit(2)>50000/HA2KEL.or.LIFE%Tfit(2)<0._SP) LIFE%Tfit(2)=0._SP
 !
end subroutine RT_mean_lifetimes_and_temperatures

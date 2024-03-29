!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine RT_entropy(en,k,dG_lesser,Entr,TEntr,EnEntr,E_IP)
 !
 use pars,           ONLY:SP,cZERO,rZERO
 use R_lattice,      ONLY:bz_samp
 use units,          ONLY:HA2KEL
 use electrons,      ONLY:levels,spin_occ
 use RT_occupations, ONLY:RT_el_occ,RT_ho_occ
 use real_time,      ONLY:RT_bands,eval_entropy,RT_levels,RTibz
 use parallel_m,     ONLY:PAR_G_k_range
 !
 implicit none 
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: dG_lesser(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 real(SP),      intent(in) :: E_IP
 real(SP),     intent(out) :: TEntr(3)
 real(SP),   intent(inout) :: Entr(3),EnEntr(3)
 !
 ! Workspace
 !
 integer     :: ib1,ib2,ik,ik_RT,i1
 real(SP)    :: Energies_e(5),Energies_h(5),tmp_E(2),Entr_prev(3),EnEntr_prev(3),   &
&               S_e,S_h,VALUE_tmp(3),one_over_T
 complex(SP) :: dG_lesser_h(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2)), &
&               dG_lesser_e(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 !
 TEntr=rZERO
 Entr=rZERO
 EnEntr=rZERO
 !
 if(.not.eval_entropy) return
 !
 ! Store values from last time they where computed
 !
 Entr_prev  =Entr
 EnEntr_prev=EnEntr
 !
 ! Define G_lesser in the holes and electrons channels alone
 !
 dG_lesser_h=cZERO
 dG_lesser_e=cZERO
 !
 do ik_RT=PAR_G_k_range(1),PAR_G_k_range(2)
   do ib1=RT_bands(1),RT_bands(2)
     do ib2=RT_bands(1),RT_bands(2)
       if ( RT_levels%E(ib1,ik_RT,1)<=0._SP .and. RT_levels%E(ib2,ik_RT,1)<=0._SP ) then
         dG_lesser_h(ib1,ib2,ik_RT)=dG_lesser(ib1,ib2,ik_RT)
       else if ( RT_levels%E(ib1,ik_RT,1)> 0._SP .and. RT_levels%E(ib2,ik_RT,1)> 0._SP ) then
         dG_lesser_e(ib1,ib2,ik_RT)=dG_lesser(ib1,ib2,ik_RT)
       endif
     enddo
   enddo
 enddo
 !
 !
 ! Compute IP energy
 !
 EnEntr(1)=E_IP
 call RT_energy_components(en,k,dG_lesser_h,Energies_e,EnEntr(2),'el ')
 call RT_energy_components(en,k,dG_lesser_e,Energies_h,EnEntr(3),'hol')
 !
 ! Compute IP Entropy
 !
 Entr= 0._SP
 !
 do ik=1,en%nk
   do ib1=RT_bands(1),RT_bands(2)
     VALUE_tmp=0._SP
     do ik_RT=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
       S_h=0._SP
       S_e=0._SP
       if(RT_ho_occ%N(ib1,ik_RT)>0._SP) S_h=-RT_ho_occ%N(ib1,ik_RT)*log(RT_ho_occ%N(ib1,ik_RT)/spin_occ)
       if(RT_el_occ%N(ib1,ik_RT)>0._SP) S_e=-RT_el_occ%N(ib1,ik_RT)*log(RT_el_occ%N(ib1,ik_RT)/spin_occ)
       VALUE_tmp(1)=VALUE_tmp(1)+S_e+S_h
       if ( RT_levels%E(ib1,ik_RT,1)<=0._SP ) VALUE_tmp(2)=VALUE_tmp(2)+S_h
       if ( RT_levels%E(ib1,ik_RT,1)> 0._SP ) VALUE_tmp(3)=VALUE_tmp(3)+S_e
       VALUE_tmp=VALUE_tmp*RTibz%weights(ik_RT)
     enddo
     Entr=Entr+VALUE_tmp
   enddo
 enddo
 !
 ! Temperature derived from Entropy to Energy variation ratio
 !
 do i1=1,3
   if (.not.abs(EnEntr(i1)-EnEntr_prev(i1))>0._SP) cycle
   one_over_T=(Entr(i1)-Entr_prev(i1))/(EnEntr(i1)-EnEntr_prev(i1))
   if (abs(one_over_T)<1._SP/(50000._SP*HA2KEL)) cycle
   TEntr(i1)=1._SP/one_over_T
 enddo
 !
end subroutine RT_entropy

!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine RT_entropy(en,k,dG_lesser,Entr,TEntr,EnEntr,E_IP)
 !
 use pars,           ONLY:SP,cZERO,rZERO
 use R_lattice,      ONLY:bz_samp
 use units,          ONLY:HA2KEL
 use electrons,      ONLY:levels,spin_occ
 use real_time,      ONLY:RT_nk,RT_bands,RT_Nk_ibz,eval_entropy,RT_E_and_k, &
&                         RT_E_occupations,RT_H_occupations,RT_ibz_coo
 !
 implicit none 
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: dG_lesser(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 real(SP),      intent(in) :: E_IP
 real(SP),     intent(out) :: TEntr(3)
 real(SP),   intent(inout) :: Entr(3),EnEntr(3)
 !
 ! Workspace
 !
 integer     :: ib1,ib2,ik,ik_RT,i1
 real(SP)    :: Energies_e(5),Energies_h(5),tmp_E(2),Entr_prev(3),EnEntr_prev(3),   &
&               S_e,S_h,VALUE_tmp(3),one_over_T
 complex(SP) :: dG_lesser_h(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk), &
&               dG_lesser_e(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
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

 do ik_RT=1,RT_nk
   do ib1=RT_bands(1),RT_bands(2)
     do ib2=RT_bands(1),RT_bands(2)
       if ( RT_E_and_k%E(ib1,ik_RT,1)<=0._SP .and. RT_E_and_k%E(ib2,ik_RT,1)<=0._SP ) then
         dG_lesser_h(ib1,ib2,ik_RT)=dG_lesser(ib1,ib2,ik_RT)
       else if ( RT_E_and_k%E(ib1,ik_RT,1)> 0._SP .and. RT_E_and_k%E(ib2,ik_RT,1)> 0._SP ) then
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
     do ik_RT=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       S_h=0._SP
       S_e=0._SP
       if(RT_H_occupations(ib1,ik_RT)>0._SP) S_h=-RT_H_occupations(ib1,ik_RT)*log(RT_H_occupations(ib1,ik_RT)/spin_occ)
       if(RT_E_occupations(ib1,ik_RT)>0._SP) S_e=-RT_E_occupations(ib1,ik_RT)*log(RT_E_occupations(ib1,ik_RT)/spin_occ)
       VALUE_tmp(1)=VALUE_tmp(1)+S_e+S_h
       if ( RT_E_and_k%E(ib1,ik_RT,1)<=0._SP ) VALUE_tmp(2)=VALUE_tmp(2)+S_h
       if ( RT_E_and_k%E(ib1,ik_RT,1)> 0._SP ) VALUE_tmp(3)=VALUE_tmp(3)+S_e
     enddo
     Entr=Entr+VALUE_tmp*k%weights(ik)/real(RT_Nk_ibz(ik),SP)
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

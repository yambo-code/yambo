!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_mean_lifetimes_and_temperatures( LIFE, LIFE_ref )
 ! 
 use pars,           ONLY:SP,rZERO
 use RT_operations,  ONLY:ABS_RT_life_one_KIND,EMIT_RT_life_one_KIND
 use RT_lifetimes,   ONLY:RT_lifetime,l_RT_lifetimes_Tfit
 use real_time,      ONLY:RTibz,RT_nk,RT_bands
 use electrons,      ONLY:spin_occ,n_full_bands
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX
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
     occupations(RT_bands(1):n_full_bands,:)=spin_occ
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

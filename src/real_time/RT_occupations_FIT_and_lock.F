! 
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team
!              http://www.yambo-code.org
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
subroutine RT_occupations_FIT_and_lock(E,k,T_fit,Ef_fit,Occupations)
 !
 ! Check if both the electrons and the holes are below a given temperature
 ! In case locks the occupations of states which are not expected to change
 ! any more
 !
 use pars,           ONLY:SP,cZero,cI
 use QP_m,           ONLY:QP_table,QP_n_states
 use electrons,      ONLY:levels,spin_occ,spin
 use R_lattice,      ONLY:bz_samp
 use drivers,        ONLY:l_elel_scatt
 use vec_operate,    ONLY:sort
 use functions,      ONLY:Fermi_fnc_fit
 use SC,             ONLY:SC_bands,RT_nk,RT_nstates,RT_ibz_coo,RT_nk,&
&                         RT_occupations,RT_occupations_locked
 use real_time,      ONLY:RIM_ibz_coo,l_RT_uses_E_RIM
 !
 implicit none
 !
 type(levels),  intent(in)   :: E
 type(bz_samp), intent(in)   :: k
 real(SP),      intent(inout):: T_fit(2),Ef_fit(2)
 real(SP),      intent(in)   :: Occupations(SC_bands(1):SC_bands(2),RT_nk)
 !
 integer,  parameter :: fit_dim=1000
 real(SP), parameter :: tresh=1.E-5
 !
 integer  :: i_RT,i_qp,i_b,i_k,i_k_rim,i_k_Erim,i_k_RT,i_spin,i1,&
&            nfit,E_sort_index(RT_nstates)
 real(SP) :: err_fit(4),E_first_hole,E_last_electron, &
&            RT_occ_fit(fit_dim,2),RT_E_fit(fit_dim,2), &
&            RT_occ(RT_nstates),RT_occ_tmp(RT_nstates),RT_E(RT_nstates)
 !
 i_RT=0
 do i_qp=1,QP_n_states
   !
   ! n   =QP_table(i_qp,1)
   ! k   =QP_table(i_qp,3)
   ! sp  =QP_table(i_qp,4)
   !
   if (QP_table(i_qp,2)/=QP_table(i_qp,1)) cycle
   !
   i_b   =QP_table(i_qp,1)
   i_k   =QP_table(i_qp,3)
   i_spin=spin(QP_table(i_qp,:))
   !
   if (i_b<SC_bands(1)) cycle
   if (i_b>SC_bands(2)) cycle
   !
   do i_k_rim=0,RIM_ibz_coo(i_k,2)-RIM_ibz_coo(i_k,1)
     i_k_RT=RT_ibz_coo(i_k,1)+i_k_rim
     i_k_Erim =RIM_ibz_coo(i_k,1)+i_k_rim
     if (i_k_RT>RT_nk) cycle
     i_RT=i_RT+1
     !
     ! Prepare eps,f(eps) for the report
     RT_occ(i_RT)=Occupations(i_b ,i_k_RT)
     if(     l_RT_uses_E_RIM) RT_E(i_RT)=E%E_RIM(i_b,i_k_Erim,1)
     if(.not.l_RT_uses_E_RIM) RT_E(i_RT)=E%E(i_b,i_k_Erim,1)
     !
   enddo
   !
 enddo
 !
 call sort(RT_E,indx=E_sort_index)
 RT_occ_tmp(:)=RT_occ(E_sort_index(:))
 RT_occ=RT_occ_tmp
 !
 nfit=Fermi_fnc_fit(RT_E,RT_occ,RT_nstates,T_fit,Ef_fit,err_fit,&
&       RT_occ_fit_out=RT_occ_fit,RT_E_fit_out=RT_E_fit,l_T_max_in=.false.)
 !
 if(err_fit(1)>0.05) then
   T_fit(1) =-T_fit(1)
   Ef_fit(1)=0._SP
 endif
 if(err_fit(2)>0.05) then
   T_fit(2) =-T_fit(2)
   Ef_fit(2)=0._SP
 endif
 !
end subroutine RT_occupations_FIT_and_lock

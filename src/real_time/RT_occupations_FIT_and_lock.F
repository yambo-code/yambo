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
subroutine RT_occupations_FIT_and_lock(E,k,T_fit,G_lesser)
 !
 ! Check if both the electrons and the holes are below a given temperature
 ! In case locks the occupations of states which are not expected to change
 ! any more
 !
 use pars,           ONLY:SP
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
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k
 real(SP),      intent(out):: T_fit(2)
 complex(SP),   intent(in) :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 !
 integer,  parameter :: fit_dim=1000
 real(SP), parameter :: tresh=1.E-5
 !
 integer  :: i_RT,i_qp,i_b,i_k,i_k_rim,i_k_Erim,i_k_RT,i_spin,i1,&
&            nfit,E_sort_index(RT_nstates)
 real(SP) :: Ef_fit(2),err_fit(4),E_first_hole,E_last_electron, &
&            RT_occ_fit(fit_dim,2),RT_E_fit(fit_dim,2), &
&            RT_occ(RT_nstates),RT_occ_tmp(RT_nstates),RT_E(RT_nstates)
 real(SP) :: fit_tresh=1.E-2
 !
 !
 Ef_fit=0._SP
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
     !
     RT_occ(i_RT)=aimag(G_lesser(i_b ,i_b ,i_k_RT))
     if(     l_RT_uses_E_RIM) RT_E(i_RT)=E%E_RIM(i_b,i_k_Erim,1)
     if(.not.l_RT_uses_E_RIM) RT_E(i_RT)=E%E(i_b,i_k_Erim,1)
     !
     ! DEBUG <
     ! write(*,*) i_RT,i_k_RT,i_k,i_b,RT_E(i_RT),RT_occ(i_RT)
     ! DEBUG >
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
&                   RT_occ_fit_out=RT_occ_fit,RT_E_fit_out=RT_E_fit,l_T_max_in=l_elel_scatt)
 !
 return
 if(nfit==0) return
 if(l_elel_scatt.and.nfit<3) return
 !
 if( nfit==3 .and. any(err_fit(3:4)>fit_tresh ) ) then
   if ( l_elel_scatt ) return
   if ( all(err_fit(3:4)>fit_tresh ) ) return
   if ( err_fit(3)<fit_tresh ) nfit=1
   if ( err_fit(4)<fit_tresh ) nfit=2
 endif
 if(nfit==2 .and. err_fit(4)>fit_tresh  ) return
 if(nfit==1 .and. err_fit(3)>fit_tresh  ) return
 !
 !DEBUG>
 !if (nfit==3) then
 !  do i1=1,i_RT
 !    write (100,*) RT_E(i1)*HA2EV,RT_occ(i1)
 !  enddo
 !  do i1=1,fit_dim
 !    write (200,*) RT_E_fit(i1,1)*HA2EV,RT_occ_fit(i1,1)
 !    write (300,*) RT_E_fit(i1,2)*HA2EV,RT_occ_fit(i1,2)
 !  enddo
 !endif
 !return
 !DEBUG<
 ! 
 if(nfit==3.or.nfit==1) then
   do i1=1,fit_dim
     if(RT_occ_fit(i1,1)<(spin_occ-tresh)) exit
   enddo
   if(i1==fit_dim+1) i1=fit_dim
   E_first_hole=RT_E_fit(i1,1)
 endif
 !
 if(nfit==3.or.nfit==2) then
   do i1=fit_dim,1,-1
     if(RT_occ_fit(i1,2)>tresh) exit
   enddo
   if(i1==0) i1=1
   E_last_electron=RT_E_fit(i1,2)
 endif
 !
 RT_occupations_locked=.false.
 !
 i_RT=0
 do i_qp=1,QP_n_states
   !
   if (QP_table(i_qp,2)/=SC_bands(1)) cycle
   !
   i_b   =QP_table(i_qp,1)
   i_k   =QP_table(i_qp,3)
   i_spin=spin(QP_table(i_qp,:))
   !
   if (i_b<SC_bands(1)) cycle
   if (i_b>SC_bands(2)) cycle
   !
   do i_k_rim=1,RIM_ibz_coo(i_k,2)-RIM_ibz_coo(i_k,1)+1
     i_k_RT=RT_ibz_coo(i_k,1)+i_k_rim-1
     i_k_Erim =RIM_ibz_coo(i_k,1)+i_k_rim-1
     if (i_k_RT>RT_nk) cycle
     i_RT=i_RT+1
     !
     ! Prepare eps,f(eps) for the report
     !
     if(     l_RT_uses_E_RIM) RT_E(i_RT)=E%E_RIM(i_b,i_k_Erim,1)
     if(.not.l_RT_uses_E_RIM) RT_E(i_RT)=E%E(i_b,i_k_Erim,1)
     !
     if(RT_E(i_RT)<E_first_hole   .and. (nfit==3.or.nfit==1) ) RT_occupations_locked(i_b,i_k_RT)=.true.
     if(RT_E(i_RT)>E_last_electron.and. (nfit==3.or.nfit==2) ) RT_occupations_locked(i_b,i_k_RT)=.true.
     !
   enddo
   !
 enddo
 !
end subroutine RT_occupations_FIT_and_lock

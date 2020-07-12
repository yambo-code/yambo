!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine RT_Hamiltonian_diagonalize(E,k,BGR,A_input)
 !
 ! This routine defines and diagonalize the Coherent part of the
 ! evolution operator.
 !
 ! This is by Ho_plus_Sigma which include a H/xc part plus RT_Vnl_xc
 !
 ! In this routine I do a double job. I diagonalize
 ! 
 !  1. Ho_plus_Sigma
 !  2. Ho_plus_Sigma-RT_Vnl_xc+Z*RT_Vnl_xc
 !
 ! The point 2 is done only if l_RT_update_Energies=.TRUE. and if Z_bgr/=1.
 !
 use pars,           ONLY:SP,rZERO,cONE,cZERO
 use R_lattice,      ONLY:nkibz
 use timing_m,       ONLY:timing
 use electrons,      ONLY:levels
 use hamiltonian,    ONLY:H_rotation,E_reference
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX
 !use interfaces,     ONLY:LINEAR_ALGEBRA_driver,OCCUPATIONS_Fermi
 use real_time,      ONLY:RT_E_and_k,Ho_plus_Sigma,RT_ibz_coo,RT_ibz_dupl,RT_bands,&
&                         l_RT_uses_E_FineGd,l_RT_update_Energies,l_RT_rotate_DM,&
&                         l_RT_eval_lifetimes,RT_nbands,l_NE_with_fields
 use R_lattice,      ONLY:bz_samp
 use RT_control,     ONLY:RT_do_it
 use fields,         ONLY:gauge_field
 use wrapper_omp,    ONLY:V_by_V_plus_V_omp
 !
 implicit none
 !
 type(levels)                 :: E
 type(bz_samp), intent(in)    :: k
 real(SP),      intent(out)   :: BGR(4)
 type(gauge_field), intent(in):: A_input
 !
 ! Work Space
 !
 complex(SP)  :: H(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2)),&
&                H_field(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 real(SP)     :: Ed(RT_bands(1):RT_bands(2)),dE
 integer      :: ib,ik,ik_RT,ik_E_FineGd
 logical      :: l_do_E,l_do_R
 !
 BGR=rZERO
 !
 l_do_E=l_RT_update_Energies.and.(l_RT_eval_lifetimes.or.(RT_do_it('OUT').or.RT_do_it('OBS')))
 l_do_R=l_RT_rotate_DM.and.(RT_do_it('OUT').or.RT_do_it('OBS'))
 !
 if (.not.l_do_E .and. .not. l_do_R) return
 !
 call timing('RT H diago',OPR='start')
 !
                         E%E       (RT_bands(1):RT_bands(2),:,:)=rZERO
 if (l_RT_uses_E_FineGd) E%E_FineGd(RT_bands(1):RT_bands(2),:,:)=rZERO
 !
 RT_E_and_K%E(RT_bands(1):RT_bands(2),:,:)=rZERO
 !
 do ik=1,nkibz
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik) ) cycle
   !
   ik_RT=RT_ibz_coo(ik,1)
   !
   H(:,:)=Ho_plus_Sigma(:,:,ik_RT,1)
   !
   if (l_NE_with_fields) then
     !
     H_field=cZERO
     call RT_apply_field(ik,1,H_field,A_input)
     call V_by_V_plus_V_omp(RT_nbands,cONE,-H_field,H)
     !
   endif
   !
   call SERIAL_HERMITIAN_diagonalization(RT_nbands,H,Ed)
   !
   ! Rotation
   !
   if (l_do_R) H_rotation(:,:,ik,1)=H(:,:)
   !
   if (.not.l_do_E) cycle
   !
   do ib=RT_bands(2),1,-1
     !
     if (ib>=RT_bands(1)) dE=Ed(ib)-E_reference%E(ib,ik,1)
     if (ib< RT_bands(1)) dE=Ed(RT_bands(1))-E_reference%E(RT_bands(1),ik,1)
     !
     do ik_RT=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       if(      l_RT_uses_E_FineGd .and.ik_RT/=RT_ibz_coo(ik,1)) then
         ik_E_FineGd=ik_RT-ik+sum(RT_ibz_dupl(:ik))
         E%E_FineGd(ib,ik_E_FineGd,1) = E_reference%E_FineGd(ib,ik_E_FineGd,1)+dE
         if (ib>=RT_bands(1)) RT_E_and_k%E(ib,ik_RT,1) =E%E_FineGd(ib,ik_E_FineGd,1)
       else
         E%E(ib,ik,1)             =E_reference%E(ib,ik,1)+dE
         if (ib>=RT_bands(1)) RT_E_and_k%E(ib,ik_RT,1) =E%E(ib,ik,1)
       endif
       !
     enddo
   enddo
   !    
 enddo
 !
                         call PP_redux_wait(E%E,       COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 if (l_RT_uses_E_FineGd) call PP_redux_wait(E%E_FineGd,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 call PP_redux_wait(RT_E_and_K%E(RT_bands(1):RT_bands(2),:,:),COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 ! Evaluation of the contribution to the minimal gap
 !
 BGR=0._SP
 !call OCCUPATIONS_Fermi(E,k,mode="OCCUPATIONS",n_bands=RT_bands(2))
 !
 !BGR(1)=E%E_dir_gap(1)
 !BGR(2)=E%E_ind_gap(1)
 !BGR(3)=BGR(1)-E_reference%E_dir_gap(1)
 !BGR(4)=BGR(2)-E_reference%E_ind_gap(1)
 !
 call timing('RT H diago',OPR='stop')
 !
end subroutine RT_Hamiltonian_diagonalize

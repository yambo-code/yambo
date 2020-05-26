!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine RT_average_operator(N_V,O_VALUE,V_VALUE,O_REAL,O_QP,O_MATRIX,&
&                              V_MATRIX,PV_MATRIX,G,k,en,TRANS,Double_Grid,nK_mem)
 !
 use pars,           ONLY:SP,cI,cZERO
 use real_time,      ONLY:DbGd_ibz_coo,RT_Nk_ibz,RT_ibz_coo,l_RT_uses_E_DbGd,&
&                         RT_bands,RT_nbands
 use R_lattice,      ONLY:bz_samp 
 use QP_m,           ONLY:QP_n_states
 use electrons,      ONLY:n_sp_pol,n_spin,levels
 use FFT_m,          ONLY:fft_size
 use H_interfaces,   ONLY:V_real_space_to_H
 use wave_func,      ONLY:WF
 use wrapper_omp,    ONLY:Vstar_dot_V_omp
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_Xk_ibz_index,&
&                         PAR_COM_Xk_ibz_INDEX,PAR_G_k_range
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(bz_samp), intent(in) :: k
 type(levels),  intent(in) :: en
 complex(SP),   intent(in) :: G(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 integer,       intent(in) :: nK_mem,N_V
 !
 ! Optionals
 !
 complex(SP),     optional, intent(out):: O_VALUE,V_VALUE(N_V)
 complex(SP),     optional, intent(in) :: PV_MATRIX(N_V,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
 complex(SP),     optional, intent(in) ::  V_MATRIX(N_V,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
 complex(SP),     optional, intent(in) ::  O_MATRIX(    RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
 complex(SP),     optional, intent(in) :: O_QP(QP_n_states)
 real(SP),        optional, intent(in) :: O_REAL(fft_size,n_spin)
 logical,         optional, intent(in) :: TRANS
 character(6),    optional, intent(in) :: Double_Grid
 !
 ! Work Space
 !
 integer     :: ib,ibp,ik,RT_bands2,ik_ibz,i_sp_pol,iE_rim,i_cmp
 real(SP)    :: deltaE,deltaE_DbGd
 complex(SP) :: Hk(N_V,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2)),VALUE_tmp(N_V), &
&               Hk_tmp(RT_bands(2),RT_bands(2)),                                    &
&               Hk_vec_tmp(N_V,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 logical     :: l_rim_energy,l_rim_dipole
 !
 call timing('RT average Op',OPR='start')
 !
 l_rim_energy=.false.
 l_rim_dipole=.false.
 if(present(Double_Grid).and.l_RT_uses_E_DbGd) then
   if(trim(Double_Grid)=='ENERGY') l_rim_energy=.true.
   if(trim(Double_Grid)=='DIPOLE') l_rim_dipole=.true.
 endif
 !
 if (N_V==0) call error('RT_average_operator: not a valid call')
 !
 RT_bands2=RT_nbands**2
 if (N_V==1) O_VALUE=cZERO
 if (N_V> 1) V_VALUE=cZERO
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik_ibz=1,k%nibz
     ! 
     if (.not.PAR_IND_Xk_ibz%element_1D(ik_ibz)) cycle   
     !
     Hk       =cZERO
     VALUE_tmp=cZERO
     !
     call make_Hk_matrix(ik_ibz,i_sp_pol)
     !
     do i_cmp=1,N_V
       !
       do ik=RT_ibz_coo(ik_ibz,1),RT_ibz_coo(ik_ibz,2)
         !
         if(l_rim_energy) then
           iE_rim=ik-RT_ibz_coo(ik_ibz,1)+DbGd_ibz_coo(ik_ibz,1)
           do ib=RT_bands(1),RT_bands(2)
             Hk(1,ib,ib)=Hk_tmp(ib,ib)-en%E(ib,ik_ibz,i_sp_pol)+en%E_DbGd(ib,iE_rim,i_sp_pol)
             do ibp=ib+1,RT_bands(2)
               Hk(1,ib,ibp)=Hk_tmp(ib,ibp)
               Hk(1,ibp,ib)=Hk_tmp(ibp,ib)
             enddo
           enddo
         else if(l_rim_dipole) then
           iE_rim=ik-RT_ibz_coo(ik_ibz,1)+DbGd_ibz_coo(ik_ibz,1)
           do ib=RT_bands(1),RT_bands(2)
             Hk(i_cmp,ib,ib)=Hk_vec_tmp(i_cmp,ib,ib)
             do ibp=ib+1,RT_bands(2)
               deltaE     =en%E(ib,ik_ibz,i_sp_pol)     -en%E(ibp,ik_ibz,i_sp_pol)
               deltaE_DbGd=en%E_DbGd(ib,iE_rim,i_sp_pol)-en%E_DbGd(ibp,iE_rim,i_sp_pol)
               if(abs(deltaE)>1.E-5_SP .and. abs(deltaE_DbGd)>1.E-5_SP) then
                 Hk(i_cmp,ib,ibp)=Hk_vec_tmp(i_cmp,ib,ibp)*deltaE/deltaE_DbGd
                 Hk(i_cmp,ibp,ib)=Hk_vec_tmp(i_cmp,ibp,ib)*deltaE/deltaE_DbGd
               else
                 Hk(i_cmp,ibp,ib)=Hk_vec_tmp(i_cmp,ibp,ib)
               endif
             enddo
           enddo
         else
           if(N_V==1) Hk(1,:,:)    =Hk_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
           if(N_V> 1) Hk(i_cmp,:,:)=Hk_vec_tmp(i_cmp,:,:)
         endif 
         !
         VALUE_tmp(i_cmp)=VALUE_tmp(i_cmp)-cI*Vstar_dot_V_omp(RT_bands2,Hk(i_cmp,:,:),G(:,:,ik))
         !
       enddo
       !
       if (N_V==1) O_VALUE       =O_VALUE       +VALUE_tmp(i_cmp)*k%weights(ik_ibz)/real(RT_Nk_ibz(ik_ibz))
       if (N_V> 1) V_VALUE(i_cmp)=V_VALUE(i_cmp)+VALUE_tmp(i_cmp)*k%weights(ik_ibz)/real(RT_Nk_ibz(ik_ibz))
       !
     enddo
     !
   enddo
   !
 enddo
 !
 ! All 2 All. 
 !============
 ! AM March 11 2015. 
 !
 ! The parallel distribution is tricky as the k%nibz/=nK_mem case is the only explicit.
 ! All other parallelizations are inside V_qp_basis_to_H and V_real_space_to_H.
 !
 ! Note that the all2all here is possible because the parallel structure is
 !
 !   "k.b" -> QP (k,n,n') and PAR_IND_WF_k+PAR_IND_B_mat_ordered
 !   "q"   -> q 
 !   "qp"  -> Bp_mat (m,m')
 !
 ! and inside the above routines there is a HEAD_QP_cpu that does not duplicate the calls. 
 !
 ! NOTE, however, that the all2all procedure are not at all general and must be changed if the
 ! input array is distributed in another way.
 !
 call timing('RT avg redux',OPR='start')
 !
 if (present(O_QP).or.present(O_REAL)) then
   if (N_V==1) call PP_redux_wait(O_VALUE)
   if (N_V> 1) call PP_redux_wait(V_VALUE)
 else
   if (N_V==1) call PP_redux_wait(O_VALUE,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   if (N_V> 1) call PP_redux_wait(V_VALUE,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 endif
 !
 call timing('RT avg redux',OPR='stop')
 call timing('RT average Op',OPR='stop')
 !
 contains 
   !
   subroutine make_Hk_matrix(ik,i_sp_pol)
     !
     use D_lattice,      ONLY:dl_sop,sop_inv
     use matrix_operate, ONLY:m3det
     !
     implicit none
     !
     integer, intent(in) :: ik,i_sp_pol
     !
     integer     :: ik_mem,i_star,is
     real(SP)    :: R_s(N_V,N_V)
     complex(SP) :: V_N(N_V)
     logical     :: l_transpose
     !
     ik_mem=ik
     if (k%nibz/=nK_mem) ik_mem=PAR_Xk_ibz_index(ik)
     !
     l_transpose=.false.
     if(present(TRANS)) l_transpose=TRANS
     !
     ! A: Average of a scalar
     !========================
     if (N_V==1) then
       !
       Hk_tmp=cZERO
       !
       if (present(O_MATRIX)) then
         do ib=RT_bands(1),RT_bands(2)
           do ibp=RT_bands(1),RT_bands(2)
             !
             if (     l_transpose) Hk_tmp(ibp,ib)=O_MATRIX(ib,ibp,ik_mem,i_sp_pol) 
             if (.not.l_transpose) Hk_tmp(ib,ibp)=O_MATRIX(ib,ibp,ik_mem,i_sp_pol) 
             !
           enddo
         enddo
       endif
       !
       if (present(O_QP))     call V_qp_basis_to_H(ik,i_sp_pol,O_QP,Hk_tmp)
       !
       ! Warning: for nspinor=2 the Vxc countribution could be wrong
       !
       if (present(O_REAL))   call V_real_space_to_H(ik,i_sp_pol,Hk_tmp,WF,'def',V=O_REAL)
       !
     endif
     !
     if (N_V>1) then
       !
       Hk_vec_tmp=cZERO
       !
       do ib=RT_bands(1),RT_bands(2)
         do ibp=RT_bands(1),RT_bands(2)
             !
             V_N=cZERO
             !
             do i_star=1,k%nstar(ik)
                !
                is = sop_inv(k%star(ik,i_star))
                !
                if (N_V == 3) then
                  !
                  ! B: Average of a Chartesian vector
                  !===================================
                  !
                  ! This procedure is correct for polar-vectors
                  !
                  if (present( V_MATRIX)) then
                    R_s= dl_sop(:,:,is)
                    V_N(:)=V_N(:)+matmul(R_s, V_MATRIX(:,ib,ibp,ik_mem,i_sp_pol))
                  endif
                  !
                  ! This procedure is correct for axial-vectors
                  !
                  if (present(PV_MATRIX)) then
                    R_s=dl_sop(:,:,is)*m3det(dl_sop(:,:,is))
                    V_N(:)=V_N(:)+matmul(R_s,PV_MATRIX(:,ib,ibp,ik_mem,i_sp_pol))
                  endif
                  !
                else 
                  !
                  ! C: Average of a Multi-dim operator
                  !====================================
                  !
                  ! Here I assume that V_MATRIX(:,:,:,R_is K_ik,:) = V_MATRIX(:,:,:,K_ik,:)
                  !
                  if (present( V_MATRIX)) then
                    V_N(:)=V_N(:)+V_MATRIX(:,ib,ibp,ik_mem,i_sp_pol)
                  endif
                  if (present(PV_MATRIX)) then
                    V_N(:)=V_N(:)+PV_MATRIX(:,ib,ibp,ik_mem,i_sp_pol)
                  endif
                endif
             enddo
             !
             if (     l_transpose) Hk_vec_tmp(:,ibp,ib )=V_N(:)/real(k%nstar(ik),SP)
             if (.not.l_transpose) Hk_vec_tmp(:,ib ,ibp)=V_N(:)/real(k%nstar(ik),SP)
             !
         enddo
       enddo
       !
     endif
     !
   end subroutine make_Hk_matrix
   !
end subroutine RT_average_operator

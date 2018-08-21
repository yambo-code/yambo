!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_average_operator(O_VALUE,V_VALUE,O_REAL,O_QP,O_MATRIX,&
&                              V_MATRIX,PV_MATRIX,G,k,en,TRANS,Double_Grid,nK_mem)
 !
 use pars,           ONLY:SP,cI,cZERO
 use real_time,      ONLY:DbGd_ibz_coo,RT_Nk_ibz,RT_nk,RT_ibz_coo,l_RT_uses_E_DbGd,&
&                         RT_bands,RT_nbands
 use com,            ONLY:error
 use R_lattice,      ONLY:bz_samp 
 use QP_m,           ONLY:QP_n_states
 use electrons,      ONLY:n_sp_pol,n_spin,levels
 use FFT_m,          ONLY:fft_size
 use X_m,            ONLY:Dipole_Energy_treshold
 use wave_func,      ONLY:WF
 use wrapper_omp,    ONLY:Vstar_dot_V_omp
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_Xk_ibz_index,PAR_COM_Xk_ibz_INDEX
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(bz_samp), intent(in) :: k
 type(levels),  intent(in) :: en
 complex(SP),   intent(in) :: G(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 integer,       intent(in) :: nK_mem
 !
 ! Optionals
 !
 complex(SP),     optional, intent(out):: O_VALUE,V_VALUE(3)
 complex(SP),     optional, intent(in) :: PV_MATRIX(3,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
 complex(SP),     optional, intent(in) ::  V_MATRIX(3,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
 complex(SP),     optional, intent(in) ::  O_MATRIX(  RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
 complex(SP),     optional, intent(in) :: O_QP(QP_n_states)
 real(SP),        optional, intent(in) :: O_REAL(fft_size,n_spin)
 logical,         optional, intent(in) :: TRANS
 character(6),    optional, intent(in) :: Double_Grid
 !
 ! Work Space
 !
 integer     :: ib,ibp,ik,RT_bands2,ik_ibz,i_sp_pol,iE_rim,OUTPUT_dim,i_dir
 real(SP)    :: deltaE,deltaE_DbGd
 complex(SP) :: Hk(3,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2)),VALUE_tmp(3), &
&               Hk_tmp(RT_bands(2),RT_bands(2)),                                    &
&               Hk_vec_tmp(3,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 logical     :: l_rim_energy,l_rim_dipole
 !
#if defined _TIMING
 call timing('RT average Op',OPR='start')
#endif
 !
 l_rim_energy=.false.
 l_rim_dipole=.false.
 if(present(Double_Grid).and.l_RT_uses_E_DbGd) then
   if(trim(Double_Grid)=='ENERGY') l_rim_energy=.true.
   if(trim(Double_Grid)=='DIPOLE') l_rim_dipole=.true.
 endif
 !
 OUTPUT_dim=0
 if(present(O_MATRIX).or.present(O_real).or.present(O_QP)) OUTPUT_dim=1
 !
 if(present(V_MATRIX).or.present(PV_MATRIX))    OUTPUT_dim=3
 !
 if (OUTPUT_dim==0) call error('RT_average_operator: not a valid call')
 !
 RT_bands2=RT_nbands**2
 if (OUTPUT_dim==1) O_VALUE=cZERO
 if (OUTPUT_dim==3) V_VALUE=cZERO
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
   do i_dir=1,OUTPUT_dim
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
           Hk(i_dir,ib,ib)=Hk_vec_tmp(i_dir,ib,ib)
           do ibp=ib+1,RT_bands(2)
             deltaE     =en%E(ib,ik_ibz,i_sp_pol)     -en%E(ibp,ik_ibz,i_sp_pol)
             deltaE_DbGd=en%E_DbGd(ib,iE_rim,i_sp_pol)-en%E_DbGd(ibp,iE_rim,i_sp_pol)
             if(abs(deltaE)>Dipole_Energy_treshold .and. abs(deltaE_DbGd)>Dipole_Energy_treshold) then
               Hk(i_dir,ib,ibp)=Hk_vec_tmp(i_dir,ib,ibp)*deltaE/deltaE_DbGd
               Hk(i_dir,ibp,ib)=Hk_vec_tmp(i_dir,ibp,ib)*deltaE/deltaE_DbGd
             else
               Hk(i_dir,ibp,ib)=Hk_vec_tmp(i_dir,ibp,ib)
             endif
           enddo
         enddo
       else
         if(OUTPUT_dim==1) Hk(1,:,:)    =Hk_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
         if(OUTPUT_dim==3) Hk(i_dir,:,:)=Hk_vec_tmp(i_dir,:,:)
       endif 
       !
       VALUE_tmp(i_dir)=VALUE_tmp(i_dir)-cI*Vstar_dot_V_omp(RT_bands2,Hk(i_dir,:,:),G(:,:,ik))
       !
     enddo
     !
     if(OUTPUT_dim==1) O_VALUE       =O_VALUE       +VALUE_tmp(i_dir)*k%weights(ik_ibz)/real(RT_Nk_ibz(ik_ibz))
     if(OUTPUT_dim==3) V_VALUE(i_dir)=V_VALUE(i_dir)+VALUE_tmp(i_dir)*k%weights(ik_ibz)/real(RT_Nk_ibz(ik_ibz))
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
 if (present(O_QP).or.present(O_REAL)) then
   if (OUTPUT_dim==1) call PP_redux_wait(O_VALUE)
   if (OUTPUT_dim==3) call PP_redux_wait(V_VALUE)
 else
   if (OUTPUT_dim==1) call PP_redux_wait(O_VALUE,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   if (OUTPUT_dim==3) call PP_redux_wait(V_VALUE,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 endif
 !
#if defined _TIMING
 call timing('RT average Op',OPR='stop')
#endif
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
     real(SP)    :: R_s(3,3)
     complex(SP) :: V3(3)
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
     if (OUTPUT_dim==1) then
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
       if (present(O_REAL))   call V_real_space_to_H(ik,i_sp_pol,O_REAL,Hk_tmp,WF,'def')
       !
     endif
     !
     ! B: Average of a vector
     !========================
     if (OUTPUT_dim==3) then
       !
       Hk_vec_tmp=cZERO
       !
       do ib=RT_bands(1),RT_bands(2)
         do ibp=RT_bands(1),RT_bands(2)
             !
             V3=cZERO
             !
             do i_star=1,k%nstar(ik)
                !
                is = sop_inv(k%star(ik,i_star))
                !
                ! This procedure is correct for polar-vectors
                !
                if (present( V_MATRIX)) then
                  R_s= dl_sop(:,:,is)
                  V3(:)=V3(:)+matmul(R_s, V_MATRIX(:,ib,ibp,ik_mem,i_sp_pol))
                endif
                !
                ! This procedure is correct for axial-vectors
                !
                if (present(PV_MATRIX)) then
                  R_s=dl_sop(:,:,is)*m3det(dl_sop(:,:,is))
                  V3(:)=V3(:)+matmul(R_s,PV_MATRIX(:,ib,ibp,ik_mem,i_sp_pol))
                endif
                !
             enddo
             !
             if (     l_transpose) Hk_vec_tmp(:,ibp,ib )=V3(:)/real(k%nstar(ik),SP)
             if (.not.l_transpose) Hk_vec_tmp(:,ib ,ibp)=V3(:)/real(k%nstar(ik),SP)
             !
         enddo
       enddo
       !
     endif
     !
   end subroutine make_Hk_matrix
   !
end subroutine RT_average_operator

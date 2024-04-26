!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
!> @brief Calculate the average value of an operator
!
! <A> = 1/N_k \sum_{v,k} < u_{v,k} (t) | A | u_{v,k} (t) >
!
! Symmetry operatoration are applied if necessary
!
subroutine NL_average_operator(O_VALUE,V_VALUE,O_REAL,O_MATRIX,&
&                              V_MATRIX,PV_MATRIX,V_bands,dG,k,en,TRANS,nK_mem)
 !
 use pars,           ONLY:SP,cZERO,cONE,cI
 use nl_optics,      ONLY:NL_bands,NL_nbands
 use R_lattice,      ONLY:bz_samp,nXkibz 
 use QP_m,           ONLY:QP_nk
 use electrons,      ONLY:n_sp_pol,n_spin,levels,spin_occ
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:WF
 use wrapper_omp,    ONLY:Vstar_dot_V_omp,M_by_V_omp
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_Xk_ibz_index,PAR_COM_Xk_ibz_INDEX
 use parallel_int,   ONLY:PP_redux_wait
 use H_interfaces,   ONLY:V_real_space_to_H
 !
 implicit none
 !
 type(bz_samp), intent(in) :: k
 type(levels),  intent(in) :: en
 integer,       intent(in) :: nK_mem
 !
 ! Optionals
 !
 complex(SP),     optional, intent(out):: O_VALUE,V_VALUE(3)
 complex(SP),     optional, intent(in) :: PV_MATRIX(3,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nK_mem,n_sp_pol)
 complex(SP),     optional, intent(in) ::  V_MATRIX(3,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nK_mem,n_sp_pol)
 complex(SP),     optional, intent(in) ::  O_MATRIX(  NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nK_mem,n_sp_pol)
 real(SP),        optional, intent(in) :: O_REAL(fft_size,n_spin)
 complex(SP),     optional, intent(in) :: V_bands(NL_bands(2),maxval(en%nbf),QP_nk, n_sp_pol)
 complex(SP),     optional, intent(in) :: dG(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),nXkibz)
 logical,         optional, intent(in) :: TRANS
 !
 ! Work Space
 !
 integer     :: ib,ibp,ik,NL_bands2,ik_ibz,i_sp_pol,OUTPUT_dim,i_dir,iv
 complex(SP) :: Hk(3,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2)),VALUE_tmp(3), &
&               Hk_tmp(NL_bands(2),NL_bands(2)),                                    &
&               Hk_vec_tmp(3,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
 complex(SP)  :: V_tmp(NL_nbands)
 character(1) :: op
 !
 OUTPUT_dim=0
 if(present(O_MATRIX).or.present(O_real)) OUTPUT_dim=1
 !
 if(present(V_MATRIX).or.present(PV_MATRIX))    OUTPUT_dim=3
 !
 if (OUTPUT_dim==0) call error('RT_average_operator: not a valid call')
 !
 NL_bands2=NL_nbands**2
 if (OUTPUT_dim==1) O_VALUE=cZERO
 if (OUTPUT_dim==3) V_VALUE=cZERO
 !
 op='N'
 if(present(TRANS)) op='T'
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
     if(OUTPUT_dim==1) Hk(1,:,:)    =Hk_tmp(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
     if(OUTPUT_dim==3) Hk(i_dir,:,:)=Hk_vec_tmp(i_dir,:,:)
     !
     if(present(V_bands)) then
       !
       do iv=1,en%nbf(i_sp_pol)
         call M_by_V_omp(op,NL_nbands,NL_nbands,cONE,Hk(i_dir,:,:),NL_nbands,V_bands(NL_bands(1):,iv,ik_ibz,i_sp_pol) &
&               ,1,cZERO,V_tmp,1)
         VALUE_tmp(i_dir)=VALUE_tmp(i_dir)+spin_occ*Vstar_dot_V_omp(NL_nbands,V_bands(NL_bands(1):,iv,ik_ibz,i_sp_pol),V_tmp) 
       enddo
     else if(present(dG)) then
       	 if(op=='T') then
           VALUE_tmp(i_dir)=-cI*Vstar_dot_V_omp(NL_bands2,transpose(Hk(i_dir,:,:)),dG(:,:,ik_ibz))
	 else
           VALUE_tmp(i_dir)=-cI*Vstar_dot_V_omp(NL_bands2,Hk(i_dir,:,:),dG(:,:,ik_ibz))
	 endif
     endif
     !
     if(OUTPUT_dim==1) O_VALUE       =O_VALUE       +VALUE_tmp(i_dir)*k%weights(ik_ibz)
     if(OUTPUT_dim==3) V_VALUE(i_dir)=V_VALUE(i_dir)+VALUE_tmp(i_dir)*k%weights(ik_ibz)
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
 if (present(O_REAL)) then
   if (OUTPUT_dim==1) call PP_redux_wait(O_VALUE)
   if (OUTPUT_dim==3) call PP_redux_wait(V_VALUE)
 else
   if (OUTPUT_dim==1) call PP_redux_wait(O_VALUE,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   if (OUTPUT_dim==3) call PP_redux_wait(V_VALUE,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 endif
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
     real(SP)    :: R_s(OUTPUT_dim,OUTPUT_dim)
     complex(SP) :: V_N(OUTPUT_dim)
     !
     ik_mem=ik
     if (k%nibz/=nK_mem) ik_mem=PAR_Xk_ibz_index(ik)
     !
     ! A: Average of a scalar
     !========================
     if (OUTPUT_dim==1) then
       !
       Hk_tmp=cZERO
       !
       if (present(O_MATRIX)) then
         do ib=NL_bands(1),NL_bands(2)
           do ibp=NL_bands(1),NL_bands(2)
             !
             Hk_tmp(ib,ibp)=O_MATRIX(ib,ibp,ik_mem,i_sp_pol) 
             !
           enddo
         enddo
       endif
       !
       ! Warning: for nspinor=2 the Vxc countribution could be wrong
       !
       if (present(O_REAL))   call V_real_space_to_H(ik,i_sp_pol,Hk_tmp,WF,'def',V=O_REAL)
       !
     endif
     !
     if (OUTPUT_dim>1) then
       !
       Hk_vec_tmp=cZERO
       !
       do ib=NL_bands(1),NL_bands(2)
         do ibp=NL_bands(1),NL_bands(2)
             !
             V_N=cZERO
             !
             do i_star=1,k%nstar(ik)
                !
                is = sop_inv(k%star(ik,i_star))
                !
                if (OUTPUT_dim == 3) then
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
             Hk_vec_tmp(:,ib ,ibp)=V_N(:)/real(k%nstar(ik),SP)
             !
         enddo
       enddo
       !
     endif
     !
   end subroutine make_Hk_matrix
   !
end subroutine NL_average_operator

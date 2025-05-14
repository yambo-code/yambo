!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Originally by AM, MG, DS
!
! Modified by NM for taking care of new parallel distribution.
!
!
subroutine K_diago_perturbative(pert_dim, what, i_BS_mat, &
&           BS_corrections, BS_VR, neigs_this_cpu, neig_shift, BS_VL, BS_Overlap) 
 !
 ! Diagonal Perturbative corrections. We assume that KS corrections are diagonal.
 !
 ! Hermitian = \sum_{kcv} |A^S_kcv|^2*BS_corrections_{kcv}
 ! 
 ! Non-hermitian : (overlap^-1)_SS' * \sum_{kcv} conj(A_L^S'_kcv) * (A_R^S_kcv) * BS_corrections_{kcv}
 !
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:V_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,PP_indexes_reset,ncpu,myid
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use BS_solvers,     ONLY:BSS_eh_E_SOC_corr,BSS_eh_W,BSS_eh_E,BSS_n_eig
 use BS,             ONLY:BS_K_coupling,BS_K_dim,BS_H_dim
 !
 implicit none
 !
 integer,     intent(in)                   :: pert_dim
 character(3),intent(in)                   :: what
 complex(SP),allocatable                   :: BS_corrections(:,:) !(pert_dim,BSS_n_eig)
 integer,intent(in)                        :: i_BS_mat, neigs_this_cpu, neig_shift
 complex(SP), target,allocatable           :: BS_VR(:,:)
 complex(SP), target, allocatable          :: BS_VL(:,:)
 complex(SP), pointer                      :: BS_Overlap(:,:)

 ! Workspace
 type(PP_indexes)                          :: px
 logical                                   :: l_duplicate_mem_par
 integer                                   :: i1,i2,i_res,res_shift,SL_H_dim,n_steps,step_shift
 complex(SP),pointer                       :: tmp_v_left(:,:) => null()
 complex(SP),allocatable                   :: tmp_BS_RL(:)
 complex(SP)                               :: aux1
 !
 BS_corrections=cZERO
 !
 if(BS_K_coupling) then 
    SL_H_dim=BS_H_dim
    tmp_v_left => BS_VL
 else 
    SL_H_dim=BS_K_dim(i_BS_mat)
    tmp_v_left => BS_VR
 endif
 !
 l_duplicate_mem_par=(ncpu>1.and.neigs_this_cpu==BSS_n_eig)
 !
 n_steps=neigs_this_cpu
 res_shift=0
 step_shift=neig_shift
 if (l_duplicate_mem_par) then
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/BSS_n_eig/),CONSECUTIVE=.TRUE.)
   n_steps=px%n_of_elements(myid+1)
   do i1=1,BSS_n_eig
     if (.not.px%element_1D(i1)) cycle
     res_shift=i1-1
     step_shift=i1-1
     exit
   enddo
 endif
 !
 allocate(tmp_BS_RL(SL_H_dim))
 tmp_BS_RL=cZERO
 !
 call live_timing('Perturb.    '//what//' ',n_steps)
 !
 do i1=1, n_steps
   !
   i_res    = i1 + res_shift
   !
   tmp_BS_RL = conjg(tmp_v_left(:,i_res))*BS_VR(:,i_res)
   !
   if(what=='SOC') then
      do i2=1,pert_dim
         BS_corrections(i2,i1+step_shift)=V_dot_V_omp(SL_H_dim, &
&                                           cmplx(BSS_eh_E_SOC_corr(i2,:),0._SP,SP), tmp_BS_RL)
      enddo
   endif
   !
   if(what=='WID') then
      BS_corrections(1,i1+step_shift)=V_dot_V_omp(SL_H_dim,cmplx(0._SP,BSS_eh_W(:),SP),tmp_BS_RL)
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call PP_redux_wait(BS_corrections) 
 !
 call live_timing()
 !
 ! In case of overlap, should I multiply with inverse of overlap
 !
 if (BS_K_coupling .and. associated(BS_Overlap)) then 
    !
    ! NM : If you use Ydiago solver, you should never see this because, Ydiago outputs 
    ! left and right eigenvectors with overlap = null() i.e identity.
    call error(" BS pertubative residuals not implemented in case of Overlap matrix")
 endif
 !
 deallocate(tmp_BS_RL)
 !
 if (l_duplicate_mem_par) call PP_indexes_reset(px)
 !
end subroutine K_diago_perturbative

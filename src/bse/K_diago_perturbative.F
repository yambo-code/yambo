!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine K_diago_perturbative(BS_mat_eigen,BS_corrections,pert_dim,what) 
 !
 ! Perturbative SOC
 !
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:V_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use BS_solvers,     ONLY:BSS_eh_E_SOC_corr,BSS_eh_W,BSS_eh_E,BSS_n_eig
 use BS,             ONLY:BS_H_dim
 !
 implicit none
 !
 character(3),intent(in) :: what
 integer,     intent(in) :: pert_dim
 complex(SP), intent(in) :: BS_mat_eigen(BS_H_dim,BSS_n_eig)
 complex(SP),intent(out) :: BS_corrections(pert_dim,BSS_n_eig)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i1,i2
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BSS_n_eig/))
 !
 BS_corrections=cZERO
 call live_timing('Perturb.    '//what//' ',px%n_of_elements(myid+1))
 !
 do i1=1,BSS_n_eig
   !
   if (.not.px%element_1D(i1)) cycle
   !
   if(what=='SOC') then
     do i2=1,pert_dim
       BS_corrections(i2,i1)=V_dot_V_omp( BS_H_dim,cmplx(BSS_eh_E_SOC_corr(i2,:),0._SP,SP),BS_mat_eigen(:,i1))
     enddo
   endif
   !
   if(what=='WID') then
     BS_corrections(1,i1)=V_dot_V_omp( BS_H_dim,cmplx(0._SP,BSS_eh_W(:),SP),BS_mat_eigen(:,i1))
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 call PP_redux_wait(BS_corrections) 
 !
 call PP_indexes_reset(px)
 !
end subroutine K_diago_perturbative

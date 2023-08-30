!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function EPS_via_perturbative_inversion(n,m,n_loop,D,C,f,Lo)
 !
 ! Here I  wante to calculate
 !
 ! f = C<D|1./(Lo^{-1}-K)|D> = C <D|R|D>
 !
 ! with
 !
 ! R = 1./(Lo^{-1}-K) = \sum_n (1-Lo K)^{-1} = \sum_n  ( Lo K )^n Lo 
 !
 ! Thus:
 !
 ! R = \sum_n ( Lo K )^n Lo = \sum_n R_n
 !
 ! R_n satisfies a recursive rule
 !
 ! R_n = ( Lo K ) R_nm1
 !
 ! As R_n appears always multplied by |D> I define
 !
 ! R_x_D_n = R_n |D>
 !
 use BS_solvers,     ONLY:K_INV_EPS,BS_mat,K_slk
 use pars,           ONLY:SP,cZERO,cONE
 use wrapper_omp,    ONLY:Vstar_dot_V_omp,M_by_V_omp,V_dot_V_omp
 !
 implicit none
 !
 integer,    intent(in)  ::n,m,n_loop
 complex(SP),intent(in)  ::C,D(n),Lo(n,m)
 complex(SP),intent(out) ::f
 !
 ! Work Space
 !
 integer     ::BSS_inv_iter_max=51
 integer     ::it
 complex(SP) ::R_x_D_nm1(n,m),R_x_D_n(n,m),delta_f,fo
 !
 EPS_via_perturbative_inversion=0
 !
 ! IP part
 !
 R_x_D_nm1(:,1)=Lo(:,1)*D(:)
 fo=C*Vstar_dot_V_omp(n,D,R_x_D_nm1(:,1))
 if(n_loop==2) then
   R_x_D_nm1(:,2)=Lo(:,2)*conjg(D(:))
   fo=fo+C*V_dot_V_omp(n,D,R_x_D_nm1(:,2))
 endif
 !
 ! Iterative Cycle 
 !
 it=1
 !
 f=fo
 delta_f=cZERO
 !
 if (K_INV_EPS%treshold<0) fo=cONE
 !
 do while (it<=BSS_inv_iter_max) 
   !
   if (.not.K_slk%kind=="SLK") then
     call M_by_V_omp('n',n,BS_mat,R_x_D_nm1(:,1),R_x_D_n(:,1))
     if (n_loop==2) call M_by_V_omp('n',n,conjg(BS_mat),R_x_D_nm1(:,2),R_x_D_n(:,2))
   else
#if defined _SCALAPACK
     call PARALLEL_M_by_V('n',n,K_slk,R_x_D_nm1(:,1),R_x_D_n(:,1))
     if (n_loop==2) call PARALLEL_M_by_V('C',n,K_slk,R_x_D_nm1(:,2),R_x_D_n(:,2))
#endif
   endif
   !
   R_x_D_n(:,1)=Lo(:,1)*R_x_D_n(:,1)
   delta_f=C*Vstar_dot_V_omp(n,D,R_x_D_n(1:n,1))
   if (n_loop==2) then 
     R_x_D_n(:,2)=Lo(:,2)*R_x_D_n(:,2)
     delta_f=delta_f+C*V_dot_V_omp(n,D,R_x_D_n(:,2))
   endif
   !
   f=f+delta_f
   !
   if (abs(delta_f)/abs(fo)<abs(K_INV_EPS%treshold)) return
   !
   if (abs(delta_f)>K_INV_EPS%max_treshold) exit
   !
   R_x_D_nm1=R_x_D_n
   !
   it=it+1
   !
 enddo
 !
 EPS_via_perturbative_inversion=1
 f=cZERO
 !
end function

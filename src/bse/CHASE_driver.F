!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS CR AF
!
! Hermitian / pseudo-Hermitian ChASE on an already-distributed SLK matrix.
! Eigenvectors remain row-distributed (no V gather). Caller should set
! neig_shift=-1 so residuals Allreduce partial dots.
!
subroutine CHASE_driver(N,neig,M_slk,V_right,V_left,E_cmpl,E_real)
 !
 use pars,          ONLY:SP,cZERO
 use matrix,        ONLY:PAR_matrix
 use linear_algebra,ONLY:USE_LK,USE_SLK
 use parallel_m,    ONLY:ncpu
 use parallel_int,  ONLY:PP_redux_wait
#if defined _SCALAPACK
 use SLK_m,         ONLY:SLK_ORTHO
#endif
 !
 implicit none
 !
 integer, intent(in)           :: N
 integer, intent(in)           :: neig
 type(PAR_matrix), intent(inout) :: M_slk
 complex(SP),      intent(out) :: V_right(N,neig)
 complex(SP),      intent(out), optional :: V_left(N,neig)
 real(SP),         intent(out), optional :: E_real(neig)
 complex(SP),      intent(out), optional :: E_cmpl(neig)
 !
 integer   :: lib_in_use, nh
 real(SP), allocatable :: E_raux(:)
 !
 lib_in_use=USE_LK
#if defined _SCALAPACK
 if(ncpu>1) lib_in_use=USE_SLK
#endif
 !
 if (lib_in_use==USE_LK) then
   !
   if (present(E_cmpl)) then
     allocate(E_raux(neig))
     call SERIAL_PSEUDOHERM_chase(N,M_slk%blc(:,:,M_slk%I),E_raux,V_left,V_right,neig)
     E_cmpl(1:neig)=cmplx(E_raux(1:neig),kind=SP)
     deallocate(E_raux)
   else
     call SERIAL_HERMITIAN_chase(N,M_slk%blc(:,:,M_slk%I),E_real,neig)
     V_right(:,1:neig)=M_slk%blc(:,1:neig,M_slk%I)
   endif
   !
 else
   !
#if defined _SCALAPACK
   if (present(E_cmpl)) then
     !
     allocate(E_raux(neig))
     call PARALLEL_PSEUDOHERM_chase( M_slk, V_right, N, E_raux, neig)
     if (ncpu>SLK_ORTHO%n_CPU) then
       call PP_redux_wait(E_raux)
       E_raux=E_raux/real(SLK_ORTHO%n_CPU,SP)
     endif
     E_cmpl(1:neig)=cmplx(E_raux(1:neig),kind=SP)
     deallocate(E_raux)
     !
     ! L = S R with S=diag(I,-I) on whatever rows are present locally
     if (present(V_left)) then
       nh=N/2
       V_left=cZERO
       V_left(1:nh,1:neig)=V_right(1:nh,1:neig)
       V_left(nh+1:N,1:neig)=-V_right(nh+1:N,1:neig)
     endif
     !
   else
     !
     call PARALLEL_HERMITIAN_chase( M_slk, V_right, N, E_real, neig)
     if (ncpu>SLK_ORTHO%n_CPU) then
       call PP_redux_wait(E_real)
       E_real=E_real/real(SLK_ORTHO%n_CPU,SP)
     endif
     !
   endif
#endif
   !
 endif
 !
end subroutine CHASE_driver

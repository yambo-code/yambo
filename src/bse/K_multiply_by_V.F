!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS IMA
!
subroutine K_multiply_by_V(Vi,Vo,iter_par)
 !
 ! Given K distributed on the processors, and Vi distributed on the processors, multiply K by Vi.
 !
 ! See mod_BS for details on the K structure
 !
 use pars,           ONLY:cZERO,cI,SP
 use parallel_m,     ONLY:PAR_IND_T_Haydock,PAR_COM_T_Haydock
 use BS,             ONLY:BS_nT_grps,BS_blk,n_BS_blks
 use BS_solvers,     ONLY:Haydock_vector,BSS_uses_FKE,BSS_slepc_double_grp
 use parallel_int,   ONLY:PP_redux_wait
 !
 implicit none
 !
 integer                ::iter_par
 type(Haydock_vector)   ::Vi(BS_nT_grps),Vo(BS_nT_grps)
 !
 ! Work Space
 !
 complex(SP)     :: alpha
 integer         :: i_B,i_Tk,i_Tp,i_g,ColsFG_Tk,ColsFG_Tp
 ! 
 if(.not.BSS_Slepc_double_grp) alpha=-real(iter_par,SP)*cI
 if(     BSS_Slepc_double_grp) alpha=-real(iter_par,SP)
 ! 
 forall(i_g=1:BS_nT_grps,PAR_IND_T_Haydock%element_1D(i_g)) Vo(i_g)%fragment = cZERO
 !
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   ColsFG_Tk = size(Vi(i_Tk)%fragment,2)
   ColsFG_Tp = size(Vi(i_Tp)%fragment,2)
   !
   select case(BS_blk(i_B)%mode)
   case("R")
     !
     ! Resonant part 
     !
     if (     BSS_uses_FKE) call K_multiply_by_V_R_FKE(Vi(i_Tp),Vo(i_Tk),i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp)
     if (.not.BSS_uses_FKE) call K_multiply_by_V_R_DKE(Vi(i_Tp),Vo(i_Tk),i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp)
     !
   case("C")
     !
     ! Coupling part
     !
     call K_multiply_by_V_C(Vi(i_Tp),Vo(i_Tk),alpha,i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp)
     !
   case("A")
     !
     ! Anti-Resonant part
     !
     if (     BSS_uses_FKE) call K_multiply_by_V_A_FKE(Vi(i_Tp),Vo(i_Tk),i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp)
     if (.not.BSS_uses_FKE) call K_multiply_by_V_A_DKE(Vi(i_Tp),Vo(i_Tk),i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp)
     !
   end select
   !
 enddo
 !
#if defined _MPI
 do i_g=1,BS_nT_grps
   if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
   call PP_redux_wait(Vo(i_g)%fragment,COMM=PAR_COM_T_Haydock(i_g)%COMM)
 enddo
#endif
 !
end subroutine K_multiply_by_V
!
!
subroutine K_multiply_by_V_R_DKE(Vi,Vo,i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp)
 !
 use pars,           ONLY:cONE
 use BS_solvers,     ONLY:Haydock_vector
 use BS,             ONLY:BS_blk,BS_T_grp
 use wrapper,        ONLY:M_by_V
 !
 implicit none
 !
 type(Haydock_vector)   :: Vi,Vo
 integer, intent(in)    :: i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp
 !
 ! Work Space
 !
 integer  :: i_ColFG
 !
 do i_ColFG=1,ColsFG_Tk
   if ( i_ColFG .gt. ColsFG_Tp ) cycle
   call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
   &                        BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
   &                        Vi%fragment(:,i_ColFG),1,cONE,&
   &                        Vo%fragment(:,i_ColFG),1)
 enddo
 !
 ! Resonant part is hermitian
 !
 if (i_Tk.ne.i_Tp) then
   do i_ColFG=1,ColsFG_Tp
     if ( i_ColFG .gt. ColsFG_Tk ) cycle
     call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
     &                        BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
     &                        Vi%fragment(:,i_ColFG),1,cONE,&
     &                        Vo%fragment(:,i_ColFG),1)
   enddo
 end if
 !
 ! Add the energy diffs on the diagonal
 !
 if (i_Tk.eq.i_Tp) then
   do i_ColFG=1,ColsFG_Tk
     Vo%fragment(:,i_ColFG) = Vo%fragment(:,i_ColFG) + BS_T_grp(i_Tk)%E(:,i_ColFG)*Vi%fragment(:,i_ColFG)
   enddo
 end if
 !
end subroutine K_multiply_by_V_R_DKE
!
!
subroutine K_multiply_by_V_A_DKE(Vi,Vo,i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp)
 !
 use pars,           ONLY:cONE
 use BS_solvers,     ONLY:Haydock_vector
 use BS,             ONLY:BS_blk,BS_T_grp
 use wrapper,        ONLY:M_by_V
 !
 implicit none
 !
 type(Haydock_vector)   :: Vi,Vo
 integer, intent(in)    :: i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp
 !
 ! Work Space
 !
 integer  :: i_ColFG
 !
 ! Anti-Resonant part 
 !
 do i_ColFG=1,ColsFG_Tk
   if ( i_ColFG .gt. ColsFG_Tp ) cycle
   call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
   &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
   &                       Vi%fragment(:,i_ColFG),1,cONE,&
   &                       Vo%fragment(:,i_ColFG),1)
 enddo
 !
 ! Anti-Resonant part is hermitian
 !
 if (i_Tk.ne.i_Tp) then
   do i_ColFG=1,ColsFG_Tp
     if ( i_ColFG .gt. ColsFG_Tk ) cycle
     call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
     &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
     &                       Vi%fragment(:,i_ColFG),1,cONE,&
     &                       Vo%fragment(:,i_ColFG),1)
   enddo
 end if
 !
 ! Add the energy diffs on the diagonal
 !
 if (i_Tk.eq.i_Tp) then
   do i_ColFG=1,ColsFG_Tk
     Vo%fragment(:,i_ColFG) = Vo%fragment(:,i_ColFG) + BS_T_grp(i_Tk)%E(:,i_ColFG)*Vi%fragment(:,i_ColFG)
   enddo
 end if
 !
end subroutine K_multiply_by_V_A_DKE
!
!
subroutine K_multiply_by_V_R_FKE(Vi,Vo,i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp)
 ! 
 use pars,           ONLY:SP,cONE,cZERO,cI
 use BS_solvers,     ONLY:Haydock_vector,FG_factor
 use BS,             ONLY:BS_blk,BS_T_grp
 use wrapper,        ONLY:M_by_V
 !
 implicit none
 !
 type(Haydock_vector)   ::Vi,Vo
 integer, intent(in)    ::i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp
 !
 ! Work Space
 !
 integer       :: i_ColFG
 complex(SP)   :: FG_factor_c
 complex(SP),allocatable     ::vtmp_fke(:)
 !
 FG_factor_c = cmplx(FG_factor,0._SP)
 !
 ! Resonant part 
 !
 allocate(vtmp_fke(BS_blk(i_B)%size(1)))
 vtmp_fke = cZERO
 call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),FG_factor_c,&
 &                        BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
 &                        sum(Vi%fragment,dim=2),1,cONE,&
 &                        vtmp_fke,1)
 ! 
 do i_ColFG=1,ColsFG_Tk
   Vo%fragment(:,i_ColFG) = Vo%fragment(:,i_ColFG) + vtmp_fke
 enddo
 deallocate(vtmp_fke)
 ! 
 ! Resonant part is hermitian 
 ! 
 if (i_Tk.ne.i_Tp) then
   allocate(vtmp_fke(BS_blk(i_B)%size(2)))
   vtmp_fke = cZERO
   call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),FG_factor_c,&
   &                        BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
   &                        sum(Vi%fragment,dim=2),1,cONE,&
   &                        vtmp_fke,1)
   !
   do i_ColFG=1,ColsFG_Tp
     Vo%fragment(:,i_ColFG) = Vo%fragment(:,i_ColFG) + vtmp_fke
   enddo
   deallocate(vtmp_fke)
 end if
 !
 ! Add the energy diffs on the diagonal
 !
 if (i_Tk.eq.i_Tp) then
   do i_ColFG=1,ColsFG_Tk
     Vo%fragment(:,i_ColFG) = Vo%fragment(:,i_ColFG) + BS_T_grp(i_Tk)%E(:,i_ColFG)*Vi%fragment(:,i_ColFG)
   enddo
 end if
 !
end subroutine K_multiply_by_V_R_FKE
!
!
subroutine K_multiply_by_V_A_FKE(Vi,Vo,i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp)
 ! 
 use pars,           ONLY:SP,cONE,cZERO,cI
 use BS_solvers,     ONLY:Haydock_vector,FG_factor
 use BS,             ONLY:BS_blk,BS_T_grp
 use wrapper,        ONLY:M_by_V
 !
 implicit none
 !
 type(Haydock_vector)   ::Vi,Vo
 integer, intent(in)    ::i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp
 !
 ! Work Space
 !
 integer       :: i_ColFG
 complex(SP)   :: FG_factor_c
 complex(SP),allocatable     ::vtmp_fke(:)
 !
 FG_factor_c = cmplx(FG_factor,0._SP)
 !
 ! Anti-Resonant part 
 !
 allocate(vtmp_fke(BS_blk(i_B)%size(1)))
 vtmp_fke = cZERO
 call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),FG_factor_c,&
 &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
 &                       sum(Vi%fragment,dim=2),1,cONE,&
 &                       vtmp_fke,1)
 !
 do i_ColFG=1,ColsFG_Tk
   Vo%fragment(:,i_ColFG) = Vo%fragment(:,i_ColFG) + vtmp_fke
 enddo
 deallocate(vtmp_fke)
 !
 ! Anti-Resonant part is hermitian
 !
 if (i_Tk.ne.i_Tp) then
   !
   allocate(vtmp_fke(BS_blk(i_B)%size(2)))
   vtmp_fke = cZERO
   call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),FG_factor_c,&
   &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
   &                       sum(Vi%fragment,dim=2),1,cONE,&
   &                       vtmp_fke,1)
   !
   do i_ColFG=1,ColsFG_Tp
     Vo%fragment(:,i_ColFG) = Vo%fragment(:,i_ColFG) + vtmp_fke
   enddo
   deallocate(vtmp_fke)
   !
 end if
 !
 ! Add the energy diffs on the diagonal
 !
 if (i_Tk.eq.i_Tp) then
   do i_ColFG=1,ColsFG_Tk
     Vo%fragment(:,i_ColFG) = Vo%fragment(:,i_ColFG) + BS_T_grp(i_Tk)%E(:,i_ColFG)*Vi%fragment(:,i_ColFG)
   enddo
 end if
 !
end subroutine K_multiply_by_V_A_FKE
!
!
subroutine K_multiply_by_V_C(Vi,Vo,alpha,i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp)
  !
  ! if l_BS_ares_from_res=true, then Vi is defined only in the resonant part
  ! for the anti-resonant part, Vi(A)=alpha*Vi(R)
  !
  use pars,           ONLY:SP,cONE,cZERO,cI
  use parallel_int,   ONLY:PP_redux_wait
  use parallel_m,     ONLY:PAR_IND_T_Haydock,PAR_COM_T_Haydock
  use BS_solvers,     ONLY:Haydock_vector,BSS_uses_DbGd
  use BS,             ONLY:BS_nT_grps,BS_blk,n_BS_blks,BS_T_grp,l_BS_ares_from_res
  use wrapper,        ONLY:M_by_V 
  !
  implicit none
  !
  complex(SP)            ::alpha
  type(Haydock_vector)   ::Vi,Vo
  integer, intent(in)    ::i_B,i_Tk,i_Tp,ColsFG_Tk,ColsFG_Tp
  !
  ! Work Space
  !
  integer       :: i_ColFG
  !
  ! IA (2022/01/19):
  ! Preliminary implementation of DbGd into coupling case does not work - to be tested
  !
  if (BSS_uses_DbGd) call error("[Haydock] Double Grid method not yet implemented in &
                                & coupling (pseudo-Hermitian) case of Haydock solver")
  !
  if (l_BS_ares_from_res) then
    !
    ! Coupling part
    !
    do i_ColFG=1,ColsFG_Tk
      if ( i_ColFG .gt. ColsFG_Tp ) cycle
      call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),alpha,&
      &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
      &                       conjg(Vi%fragment(:,i_ColFG)),1,cONE,&
      &                             Vo%fragment(:,i_ColFG),1)
    enddo
    !
    ! Coupling part is symmetric
    !
    if ( i_Tk.ne.i_Tp ) then
      do i_ColFG=1,ColsFG_Tp
        if ( i_ColFG .gt. ColsFG_Tk ) cycle
        call M_by_V('t',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),alpha,&
        &                   BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
        &                   conjg(Vi%fragment(:,i_ColFG)),1,cONE,&
        &                         Vo%fragment(:,i_ColFG),1)
      enddo
    end if
    !
  else
    !
    ! Coupling part
    !
    do i_ColFG=1,ColsFG_Tk
      call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
      &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
      &                       Vi%fragment(:,i_ColFG),1,cONE,&
      &                       Vo%fragment(:,i_ColFG),1)
    enddo
    !
    ! Anti-coupling Q = -conjg(C)
    ! 
    do i_ColFG=1,ColsFG_Tp
      if ( i_ColFG .gt. ColsFG_Tk ) cycle
      call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),-cONE,&
      &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
      &                       Vi%fragment(:,i_ColFG),1,cONE,&
      &                       Vo%fragment(:,i_ColFG),1)
    enddo
    !
  endif
  !
 end subroutine K_multiply_by_V_C
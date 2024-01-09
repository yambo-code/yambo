!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS
!
subroutine K_stored_in_a_big_matrix(i_BS_mat,iq,what)
 !
 ! Here I fill the kernel (coupling included) in an entire BIG matrix to be
 ! used for diagonalization and/or invertion
 !
 use pars,           ONLY:SP,cI,cZERO,cONE
 use parallel_int,   ONLY:PP_redux_wait
 use BS,             ONLY:BS_K_coupling,n_BS_blks,BS_res_ares_n_mat,&
&                         l_BS_ares_from_res,BS_blks_free
 use BS_solvers,     ONLY:BS_mat,BS_blk,BS_H_dim,BS_K_dim,&
&                         BSS_perturbative_width,run_inversion,run_Haydock,&
&                         BSS_eh_E,BSS_eh_W
 use timing_m,       ONLY:timing
 !
#include <memory.h>
 !
 integer,      intent(in) :: i_BS_mat,iq
 character(*), intent(in) :: what
 !
 ! Work Space
 !
 complex(SP) :: Mij
 integer     :: i_Tk,i_Tp,i_r,i_c,H_pos(2),H_shift(2),i_B,BS_mat_dim
 !
 if (allocated(BS_mat)) return
 !
 call timing('BSE matrix filling',OPR='start')
 !
 if (     BS_K_coupling) BS_mat_dim=BS_H_dim
 if (.not.BS_K_coupling) BS_mat_dim=BS_K_dim(i_BS_mat)
 !
 YAMBO_ALLOC(BS_mat,(BS_mat_dim,BS_mat_dim))
 BS_mat    = cZERO
 !
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   !
   if (i_BS_mat/=BS_blk(i_B)%ira_k .and. BS_res_ares_n_mat==2) cycle
   !
   H_shift=0
   if(BS_blk(i_B)%mode=="C") H_shift(2)=BS_K_dim(1)
   if(BS_blk(i_B)%mode=="A") H_shift(:)=BS_K_dim(1)
   !
   do i_r=1,BS_blk(i_B)%size(1)
     !
     H_pos(1)=BS_blk(i_B)%coordinate(1)+i_r-1
     !
     do i_c=1,BS_blk(i_B)%size(2)
       !
       H_pos(2)=BS_blk(i_B)%coordinate(2)+i_c-1
       !
       ! Then the upper triangle of each block and direct symmetrization
       !
       if (H_pos(1)+H_shift(1)>H_pos(2)+H_shift(2)) cycle
       if (l_BS_ares_from_res.and.H_pos(1)>H_pos(2)) cycle
       !
       Mij=BS_blk(i_B)%mat(i_r,i_c)
       !
       ! Add energies to the diagonal
       !
       if(H_pos(1)+H_shift(1)==H_pos(2)+H_shift(2)) then
         Mij=real(Mij)+BSS_eh_E(H_pos(1)+H_shift(1))*cONE
         if (allocated(BSS_eh_W).and..not.BSS_perturbative_width) then
           Mij=Mij+cI*BSS_eh_W(H_pos(1)+H_shift(1))
         endif
       endif
       !
       select case(BS_blk(i_B)%mode)
       case("R")
           BS_mat(H_pos(1)            ,H_pos(2)            )=       Mij
           ! The resonant block is hermitial
           BS_mat(H_pos(2)            ,H_pos(1)            )= conjg(Mij)
           if (l_BS_ares_from_res.and.BS_K_coupling) then
             ! The anti-resonant block is A=-R*
             BS_mat(H_pos(1)+BS_K_dim(1),H_pos(2)+BS_K_dim(1))=-conjg(Mij)
             ! The anti-resonant block is hermitian
             BS_mat(H_pos(2)+BS_K_dim(1),H_pos(1)+BS_K_dim(1))=      -Mij
           endif
       case("C")
           BS_mat(H_pos(1)            ,H_pos(2)+BS_K_dim(1))=       Mij
           ! Anti-coupling from coupling: the whole BSE matrix is Pseudo-HErmitian
           BS_mat(H_pos(2)+BS_K_dim(1),H_pos(1)            )=-conjg(Mij)
           if (l_BS_ares_from_res) then
             ! The coupling block and the anti-coupling block are symmetric
             BS_mat(H_pos(2)            ,H_pos(1)+BS_K_dim(1))=       Mij
             BS_mat(H_pos(1)+BS_K_dim(1),H_pos(2)            )=-conjg(Mij)
           endif
       case("A")
         ! The anti-resonant block is hermitial
         if(BS_res_ares_n_mat==1) then
           BS_mat(H_pos(1)+BS_K_dim(1),H_pos(2)+BS_K_dim(1))=       Mij
           BS_mat(H_pos(2)+BS_K_dim(1),H_pos(1)+BS_K_dim(1))= conjg(Mij)
         else
           BS_mat(H_pos(1)            ,H_pos(2)            )=       Mij
           BS_mat(H_pos(2)            ,H_pos(1)            )= conjg(Mij)
         endif
       case("Q")
           BS_mat(H_pos(1)+BS_K_dim(1),H_pos(2)            )=       Mij
           ! Coupling from anti-coupling: the whole BSE matrix is Pseudo-HErmitian
           BS_mat(H_pos(2)            ,H_pos(1)+BS_K_dim(1))=-conjg(Mij)
       end select
       !
     enddo
     !
   enddo
   !
 enddo
 !
 call timing('BSE matrix filling',OPR='stop')
 !
 ! ALL 2 ALL
 !===========
 !
 ! BS_mat lives in the entire world.
 !
 call timing('BSE matrix (REDUX)',OPR='start')
 call PP_redux_wait(BS_mat)
 call timing('BSE matrix (REDUX)',OPR='stop')
 !
 ! After the folding the %mat's are (in some cases) not needed anymore 
 !
 if (run_Haydock) return
 if (run_inversion.and.what=="DIAGO") return
 !
 call BS_blks_free(i_BS_mat)
 !
end subroutine K_stored_in_a_big_matrix

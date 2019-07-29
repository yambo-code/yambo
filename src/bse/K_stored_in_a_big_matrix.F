!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG
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
subroutine K_stored_in_a_big_matrix(iq,what)
 !
 ! Here I fill the kernel (coupling included) in an entire BIG matrix to be
 ! used for diagonalization and/or invertion
 !
 !      | (K_r)     (cI*K_c)    |  
 !  K = |                       |
 !      | (-cI*K_c^*)  (-K_r^*) |
 !
 use pars,           ONLY:SP,cI,cZERO,cONE
 use parallel_m,     ONLY:PP_redux_wait
 use BS,             ONLY:BS_K_coupling,n_BS_blks
 use BS_solvers,     ONLY:BS_mat,K_slk,BS_blk,BS_H_dim,BS_K_dim,&
&                         BSS_perturbative_width,run_inversion,run_Haydock,&
&                         BS_blks_free,BSS_eh_E,BSS_eh_W
 use timing_m,       ONLY:timing
 !
#include <memory.h>
 !
 integer      :: iq
 character(*) :: what
 !
 ! Work Space
 !
 complex(SP) :: E_plus_W
 integer     :: i_Tk,i_Tp,i_r,i_c,H_pos(2),i_B
 !
 if (allocated(BS_mat)                ) return
 if (what=="INV".and.K_slk%kind=="SLK") return
 !
 call timing('BSE matrix filling',OPR='start')
 !
 YAMBO_ALLOC(BS_mat,(BS_H_dim,BS_H_dim))
 BS_mat    = cZERO
 !
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   !
   do i_r=1,BS_blk(i_B)%size(1)
     !
     H_pos(1)=BS_blk(i_B)%coordinate(1)+i_r-1
     !
     ! First the diagonal
     !
     if (i_Tk==i_Tp) then
       if (BS_blk(i_B)%mode=="R") then
         E_plus_W=BSS_eh_E(H_pos(1))*cONE
         if (allocated(BSS_eh_W).and..not.BSS_perturbative_width) then
           E_plus_W=E_plus_W-cI*BSS_eh_W(H_pos(1))
         endif
         BS_mat(H_pos(1),H_pos(1))=E_plus_W+real(BS_blk(i_B)%mat(i_r,i_r),SP)
       else
         BS_mat(H_pos(1),H_pos(1)+BS_K_dim)=BS_blk(i_B)%mat(i_r,i_r)
       endif
       !
     endif
     !
     ! Then the upper triangle of each block
     !
     do i_c=1,BS_blk(i_B)%size(2)
       !
       H_pos(2)=BS_blk(i_B)%coordinate(2)+i_c-1
       !
       if (H_pos(1)>=H_pos(2)) cycle
       !
       if (BS_blk(i_B)%mode=="R") BS_mat(H_pos(1),H_pos(2))         =BS_blk(i_B)%mat(i_r,i_c)
       !
       if (BS_blk(i_B)%mode=="A") BS_mat(H_pos(1),H_pos(2)+BS_K_dim)=BS_blk(i_B)%mat(i_r,i_c)
       !
     enddo
     !
   enddo
   !
 enddo
 !
 ! Symmetrization: Resonant part is Hermitian
 !
 do i_r=1,BS_K_dim
   do i_c=i_r+1,BS_K_dim
     BS_mat(i_c,i_r)=conjg( BS_mat(i_r,i_c) )
   enddo
 enddo
 !
 if (BS_K_coupling) then
   !
   ! Symmetrization: Coupling part is symmetric
   !
   do i_r=1,BS_K_dim
     do i_c=i_r+1,BS_K_dim
       BS_mat(i_c,i_r+BS_K_dim)=BS_mat(i_r,i_c+BS_K_dim)
     enddo
   enddo
   !
   ! If Coupling the half lower part of K is empty
   ! Filling the anti-resonant and anti-coupling parts
   !
   forall(i_r=1:BS_K_dim,i_c=1:BS_K_dim)
     ! Anti-Coupling
     BS_mat(i_r+BS_K_dim,i_c)=-conjg(BS_mat(i_r,i_c+BS_K_dim))
     ! Anti-resonant
     BS_mat(i_r+BS_K_dim,i_c+BS_K_dim)=-conjg(BS_mat(i_r,i_c))
   end forall
   !
 endif
 !
 call timing('BSE matrix filling',OPR='stop')
 !
 ! ALL 2 ALL
 !===========
 !
 ! BS_mat lives in the entire world.
 !
 call timing('BSE matrix (REDUX)',OPR='start')
 !
 call PP_redux_wait(BS_mat)
 !
 call timing('BSE matrix (REDUX)',OPR='stop')
 !
 ! After the folding the %mat's are (in some cases) not needed anymore 
 !
 if (run_Haydock) return
 if (run_inversion.and.what=="DIAGO") return
 !
 call BS_blks_free( )
 !
end subroutine K_stored_in_a_big_matrix

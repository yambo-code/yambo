!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine K_stored_in_a_big_matrix(i_BS_mat,iq,what)
 !
 ! Here I fill the kernel (coupling included) in an entire BIG matrix to be
 ! used for diagonalization and/or invertion
 !
 !      | (K_r(q))      (cI*K_c(q))    |  
 !  K = |                              |
 !      | (-cI*K_c^*(-q))  (-K_r^*(-q) |
 !
 use pars,           ONLY:SP,cI,cZERO,cONE
 use parallel_m,     ONLY:PP_redux_wait
 use BS,             ONLY:BS_K_coupling,n_BS_blks,BS_mat_res_ares_dim,&
&                         l_BS_ares_asymm,BS_blks_free
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
 complex(SP) :: E_plus_W
 integer     :: i_Tk,i_Tp,i_r,i_c,H_pos(2),i_B
 !
 if (allocated(BS_mat)) return
 !
 call timing('BSE matrix filling',OPR='start')
 !
 if (BS_K_coupling) then
   YAMBO_ALLOC(BS_mat,(BS_H_dim,BS_H_dim))
   BS_mat    = cZERO
 else
   YAMBO_ALLOC(BS_mat,(BS_K_dim(i_BS_mat),BS_K_dim(i_BS_mat)))
   BS_mat    = cZERO
 endif
 !
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   !
   if (i_BS_mat/=BS_blk(i_B)%ira_k .and. BS_mat_res_ares_dim==2) cycle
   !
   do i_r=1,BS_blk(i_B)%size(1)
     !
     H_pos(1)=BS_blk(i_B)%coordinate(1)+i_r-1
     !
     ! First the diagonal
     !
     if (i_Tk==i_Tp) then
       select case(BS_blk(i_B)%mode)
       case("R")
         E_plus_W=BSS_eh_E(H_pos(1))*cONE
         if (allocated(BSS_eh_W).and..not.BSS_perturbative_width) then
           E_plus_W=E_plus_W-cI*BSS_eh_W(H_pos(1))
         endif
         BS_mat(H_pos(1),H_pos(1))=E_plus_W+real(BS_blk(i_B)%mat(i_r,i_r),SP)
       case("C")
         BS_mat(H_pos(1),H_pos(1)+BS_K_dim(1))=BS_blk(i_B)%mat(i_r,i_r)
       case("A")
         E_plus_W=BSS_eh_E(H_pos(1)+BS_K_dim(1)*(i_BS_mat-1))*cONE
         if (allocated(BSS_eh_W).and..not.BSS_perturbative_width) then
           E_plus_W=E_plus_W-cI*BSS_eh_W(H_pos(1)+BS_K_dim(1)*(i_BS_mat-1))
         endif
         if(BS_mat_res_ares_dim==1) then
           BS_mat(H_pos(1)+BS_K_dim(1),H_pos(1)+BS_K_dim(1))=E_plus_W+real(BS_blk(i_B)%mat(i_r,i_r),SP) 
         else
           BS_mat(H_pos(1),H_pos(1))=E_plus_W+real(BS_blk(i_B)%mat(i_r,i_r),SP) 
         endif
       case("Q")
         BS_mat(H_pos(1)+BS_K_dim(1),H_pos(1))=BS_blk(i_B)%mat(i_r,i_r)
       end select
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
       select case(BS_blk(i_B)%mode)
       case("R")
           BS_mat(H_pos(1)            ,H_pos(2)            )=BS_blk(i_B)%mat(i_r,i_c)
       case("C")
           BS_mat(H_pos(1)            ,H_pos(2)+BS_K_dim(1))=BS_blk(i_B)%mat(i_r,i_c)
       case("A")
         if(BS_mat_res_ares_dim==1) then
           BS_mat(H_pos(1)+BS_K_dim(1),H_pos(2)+BS_K_dim(1))=BS_blk(i_B)%mat(i_r,i_c)
         else
           BS_mat(H_pos(1)            ,H_pos(2)            )=BS_blk(i_B)%mat(i_r,i_c)
         endif
       case("Q")
           BS_mat(H_pos(1)+BS_K_dim(1),H_pos(2)            )=BS_blk(i_B)%mat(i_r,i_c)
       end select
       !
     enddo
     !
   enddo
   !
 enddo
 !
 ! Symmetrization: Resonant and anti-Resonant parts are Hermitian
 !                 Coupling and anti-Cuopling parts are Symmetric
 !
 do i_r=1,BS_K_dim(i_BS_mat)
   do i_c=i_r+1,BS_K_dim(i_BS_mat)
     BS_mat(i_c,i_r)=conjg( BS_mat(i_r,i_c) )
   enddo
 enddo
 !
 if (BS_K_coupling) then
   !
   do i_r=1,BS_K_dim(1)
     do i_c=i_r+1,BS_K_dim(1)
       BS_mat(i_c,i_r+BS_K_dim(1))=BS_mat(i_r,i_c+BS_K_dim(1))
     enddo
   enddo
   !
   ! If Coupling the half lower part of K is empty
   ! Filling the anti-resonant and anti-coupling parts
   !
   if(.not.l_BS_ares_asymm) then
     forall(i_r=1:BS_K_dim(1),i_c=1:BS_K_dim(1))
       ! Anti-Coupling
       BS_mat(i_r+BS_K_dim(1),i_c)=-conjg(BS_mat(i_r,i_c+BS_K_dim(1)))
       ! Anti-resonant
       BS_mat(i_r+BS_K_dim(1),i_c+BS_K_dim(1))=-conjg(BS_mat(i_r,i_c))
     end forall
   else
     ! Anti-Coupling
     do i_r=1,BS_K_dim(1)
       do i_c=i_r+1,BS_K_dim(1)
         BS_mat(i_c+BS_K_dim(1),i_r)=BS_mat(i_r+BS_K_dim(1),i_c)
       enddo
     enddo
     ! Anti-resonant
     do i_r=BS_K_dim(1)+1,BS_K_dim(1)+BS_K_dim(2)
       do i_c=i_r+1,BS_K_dim(1)+BS_K_dim(2)
         BS_mat(i_c,i_r)=conjg(BS_mat(i_r,i_c))
       enddo
     enddo
   endif
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
 call BS_blks_free(i_BS_mat)
 !
end subroutine K_stored_in_a_big_matrix

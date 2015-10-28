!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine K_stored_in_a_BIG_matrix(iq)
 !
 ! Here I fill the kernel (coupling included) in an entire BIG matrix to be
 ! used for diagonalization and/or invertion
 !
 !      | (K_r)     (K_c)    |  
 !  K = |                    |
 !      | (-K_c^*)  (-K_r^*) |
 !
 ! 
 use electrons,      ONLY:n_sp_pol,E_SOC_nbands
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                         PAR_COM_T_INDEX,PAR_COM_Xk_ibz_INDEX
 use BS,             ONLY:BS_mat,BS_K_dim,BS_nT_grps,BS_blk,n_BS_blks,BS_T_grp,BS_K_coupling,&
&                         BSS_rhoq0,BSS_eh_table,BSS_eh_E,BSS_eh_f,BSS_eh_W,BSS_eh_Z,BS_mat_alloc,&
&                         BS_H_dim,BS_mat_diagonal,BS_Blocks_symmetrize_K,BSS_eh_E_SOC_corr
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 use KERR,           ONLY:BSS_rhoq0_kerr,BS_mat_kerr_alloc
#endif
 !
 ! GPL_EXCLUDE_END
 !

 !
 implicit none
 !
 integer      :: iq
 !
 ! Work Space
 !
 integer :: i_c,i_r,H_pos(2),i_Tk,i_Tp,i_B,i_g,&
&           i_k_bz,i_v,i_cond,i_spin
 ! 
 if (allocated(BS_mat)) return
 !
 ! ALLOCATION
 !============
 !
 call BS_Blocks_symmetrize_K()
 call BS_mat_alloc( )
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 call BS_mat_kerr_alloc( )
#endif
 !
 ! GPL_EXCLUDE_END
 !
 !
 ! FILLING...
 !============
 !
 !... diagonal and additional elements
 !
 do i_g=1,BS_nT_grps
   !
   if (.not.PAR_IND_T_groups%element_1D(i_g)) cycle
   !
   do i_c=1,BS_T_grp(i_g)%size
     !
     i_k_bz=BS_T_grp(i_g)%table(i_c,1)
     i_v   =BS_T_grp(i_g)%table(i_c,2)
     i_cond=BS_T_grp(i_g)%table(i_c,3)
     i_spin=BS_T_grp(i_g)%table(i_c,4)
     !
     H_pos(1)=sum(BS_T_grp(:I_g-1)%size)+i_c
     !
     if (n_sp_pol==2) BSS_eh_table(H_pos(1),:)=(/i_k_bz,i_v,i_cond,i_spin/)
     if (n_sp_pol==1) BSS_eh_table(H_pos(1),:)=(/i_k_bz,i_v,i_cond/)
     !
     BSS_eh_E(H_pos(1))=BS_T_grp(i_g)%E(i_c)
     BSS_eh_f(H_pos(1))=BS_T_grp(i_g)%f(i_c)
     if (allocated(BS_T_grp(i_g)%Z)) BSS_eh_Z(H_pos(1))=BS_T_grp(i_g)%Z(i_c)
     if (allocated(BS_T_grp(i_g)%W)) BSS_eh_W(H_pos(1))=BS_T_grp(i_g)%W(i_c)
     !
     ! GPL_EXCLUDE_START
     !
     if (E_SOC_nbands/=0)            BSS_eh_E_SOC_corr(:,H_pos(1))=BS_T_grp(i_g)%E_SOC_corr(:,i_c)
     !
     ! GPL_EXCLUDE_END
     !
     BSS_rhoq0(H_pos(1))=BS_T_grp(i_g)%dipoles_R(i_c)
     BS_mat(H_pos(1),H_pos(1))=BS_T_grp(i_g)%E(i_c)/real( PAR_COM_T_INDEX%n_CPU )
     !
     !
     ! GPL_EXCLUDE_START
     !
#if defined _KERR
     BSS_rhoq0_kerr(H_pos(1))=BS_T_grp(i_g)%dipoles_R_kerr(i_c)
#endif
     !
     ! GPL_EXCLUDE_END
     !
     if (BS_K_coupling) then
       H_pos(1)=H_pos(1)+BS_K_dim
       BSS_rhoq0(H_pos(1))=BS_T_grp(i_g)%dipoles_A(i_c)
       !
       ! GPL_EXCLUDE_START
       !
#if defined _KERR
       BSS_rhoq0_kerr(H_pos(1))=BS_T_grp(i_g)%dipoles_A_kerr(i_c)
#endif
       !
       ! GPL_EXCLUDE_END
       !
     endif
   enddo
 enddo
 !
 !... remaining part of the matrix
 !
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   !
   do i_r=1,BS_blk(i_B)%size(1)
     !
     H_pos(1)=sum(BS_T_grp(:I_Tk-1)%size)+i_r
     !
     do i_c=1,BS_blk(i_B)%size(2)
       !
       H_pos(2)=sum(BS_T_grp(:I_Tp-1)%size)+i_c
       !
       if (BS_blk(i_B)%mode=="A") then
         !
         H_pos(2)=H_pos(2)+BS_K_dim
         !
         BS_mat(H_pos(1),H_pos(2))=BS_blk(i_B)%mat(i_r,i_c)
         !
       else
         !
         if (H_pos(1)>H_pos(2)) cycle
         !
         BS_mat(H_pos(1),H_pos(2))=BS_mat(H_pos(1),H_pos(2))+BS_blk(i_B)%mat(i_r,i_c)
         !
       endif
       !
       if (H_pos(1)==H_pos(2)) BS_mat_diagonal(H_pos(1))=BS_blk(i_B)%mat(i_r,i_c)
       !
     enddo
   enddo
   !
 enddo
 !
 ! Resonant part is Hermitian
 !
 do i_r=1,BS_K_dim
   do i_c=1,BS_K_dim
     if (i_r>i_c) cycle
     BS_mat(i_c,i_r)=conjg( BS_mat(i_r,i_c) )
   enddo
 enddo
 !
 ! Filling the anti-resonant and anti-coupling parts
 !
 if (BS_K_coupling) then
   !
   ! If Coupling the half lower part of K must be filled
   !
   ! Anti-resonant
   !
   forall(i_r=BS_K_dim+1:BS_H_dim,i_c=BS_K_dim+1:BS_H_dim) &
&       BS_mat(i_r,i_c)=-conjg(BS_mat(i_r-BS_K_dim,i_c-BS_K_dim))
   ! 
   ! Anti-coupling
   !
   forall(i_r=BS_K_dim+1:BS_H_dim,i_c=1:BS_K_dim) &
&       BS_mat(i_r,i_c)=-conjg(BS_mat(i_r-BS_K_dim,i_c+BS_K_dim))
   !
 endif
 !
 ! ALL 2 ALL
 !===========
 !
 ! Note here the two different levels of all2all.
 !
 ! BS_mat lives in the enitre world...
 !
 call PP_redux_wait(BS_mat)
 call PP_redux_wait(BS_mat_diagonal)
 !
 ! ... while the oscillators vector and all other similar vectors are distributed ONLY among k-points
 ! and eh transitions.
 !
 call PP_redux_wait(BSS_rhoq0,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(BSS_rhoq0,COMM=PAR_COM_eh_INDEX%COMM)
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 call PP_redux_wait(BSS_rhoq0_kerr,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(BSS_rhoq0_kerr,COMM=PAR_COM_eh_INDEX%COMM)
#endif
 if (E_SOC_nbands/=0) then
   call PP_redux_wait(BSS_eh_E_SOC_corr,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(BSS_eh_E_SOC_corr,COMM=PAR_COM_eh_INDEX%COMM)
 endif
 !
 ! GPL_EXCLUDE_END
 !
 call PP_redux_wait(BSS_eh_E,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(BSS_eh_E,COMM=PAR_COM_eh_INDEX%COMM)
 call PP_redux_wait(BSS_eh_f,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(BSS_eh_f,COMM=PAR_COM_eh_INDEX%COMM)
 call PP_redux_wait(BSS_eh_table,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(BSS_eh_table,COMM=PAR_COM_eh_INDEX%COMM)
 if (allocated(BSS_eh_W)) then
   call PP_redux_wait(BSS_eh_W,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(BSS_eh_W,COMM=PAR_COM_eh_INDEX%COMM)
 endif
 if (allocated(BSS_eh_Z)) then
   call PP_redux_wait(BSS_eh_Z,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(BSS_eh_Z,COMM=PAR_COM_eh_INDEX%COMM)
 endif
 !
end subroutine K_stored_in_a_BIG_matrix

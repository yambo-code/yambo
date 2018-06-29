!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine K_components_folded_in_serial_arrays(iq,what)
 !
 ! Here I fill the kernel (coupling included) in an entire BIG matrix to be
 ! used for diagonalization and/or invertion
 !
 !      | (K_r)     (K_c)    |  
 !  K = |                    |
 !      | (-K_c^*)  (-K_r^*) |
 !
 !
 use pars,           ONLY:SP,cI,cONE,cZERO
 use electrons,      ONLY:n_sp_pol,E_SOC_nbands
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                         PAR_COM_Xk_ibz_INDEX
 use BS_solvers,     ONLY:BS_mat,BSS_eh_E_SOC_corr,BSS_perturbative_width,run_inversion,run_Haydock,K_slk,&
&                         BSS_dipoles,BSS_eh_table,BSS_eh_E,BSS_eh_f,BSS_eh_W,BSS_eh_Z,BSS_alloc,BS_mat
 use BS,             ONLY:BS_K_dim,BS_nT_grps,BS_blk,n_BS_blks,BS_T_grp,BS_K_coupling,BS_H_dim,BS_blks_free
 !
#if defined _KERR
 use drivers,        ONLY:l_kerr
 use KERR,           ONLY:BSS_dipoles_kerr,BSS_KERR_alloc
#endif
 !
 !
#include <memory.h>
 !
 integer      :: iq
 character(*) :: what
 !
 ! Work Space
 !
 complex(SP) :: E_plus_W
 integer     :: i_c,i_r,H_pos(2),i_Tk,i_Tp,i_B,i_g,i_k_bz,i_v,i_cond,i_spin
 !
 !========================
 ! BSS COMPONENTS SECTION
 !========================
 !
 if (.not.allocated(BSS_dipoles)) then
   !
   ! ALLOCATION
   !============
   ! 
   call BSS_alloc( )
   !
#if defined _KERR
   call BSS_KERR_alloc( )
#endif
   !
   !
   ! FILLING BSS_eh_* and BSS_dipoles
   !==================================
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
       BSS_dipoles(H_pos(1))=BS_T_grp(i_g)%dipoles_R(i_c)
       if (BS_K_coupling) BSS_dipoles(H_pos(1)+BS_K_dim)=BS_T_grp(i_g)%dipoles_A(i_c)
       !
       if (E_SOC_nbands/=0) BSS_eh_E_SOC_corr(:,H_pos(1))=BS_T_grp(i_g)%E_SOC_corr(:,i_c)
       !
#if defined _KERR
       if (l_kerr) then
         BSS_dipoles_kerr(H_pos(1))=BS_T_grp(i_g)%dipoles_R_kerr(i_c)
         if (BS_K_coupling) BSS_dipoles_kerr(H_pos(1)+BS_K_dim)=BS_T_grp(i_g)%dipoles_A_kerr(i_c)
       endif
#endif
       !
       !
     enddo
   enddo
   !
   ! ALL 2 ALL
   !===========
   !
   ! BSS_eh_* and BSS_dipoles are distributed ONLY among k-points and eh transitions.
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
   call PP_redux_wait(BSS_dipoles,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(BSS_dipoles,COMM=PAR_COM_eh_INDEX%COMM)
   !
#if defined _KERR
   call PP_redux_wait(BSS_dipoles_kerr,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(BSS_dipoles_kerr,COMM=PAR_COM_eh_INDEX%COMM)
#endif
   if (E_SOC_nbands/=0) then
     call PP_redux_wait(BSS_eh_E_SOC_corr,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
     call PP_redux_wait(BSS_eh_E_SOC_corr,COMM=PAR_COM_eh_INDEX%COMM)
   endif
   !
 endif
 !
 !================
 ! BS_mat SECTION
 !================
 !
 if (allocated(BS_mat)                ) return
 if (what=="INV".and.K_slk%kind=="SLK") return
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
 ! ALL 2 ALL
 !===========
 !
 ! BS_mat lives in the entire world.
 !
 call PP_redux_wait(BS_mat)
 !
 ! After the folding the %mat's are (in some cases) not needed anymore 
 !
 if (run_Haydock) return
 if (run_inversion.and.what=="DIAGO") return
 !
 call BS_blks_free( )
 !
end subroutine K_components_folded_in_serial_arrays

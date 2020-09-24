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
subroutine K_components_folded_in_serial_arrays(iq)
 !
 ! Here I fill the kernel (coupling included) in an entire BIG matrix to be
 ! used for diagonalization and/or invertion
 !
 !      | (K_r)     (cI*K_c)    |  
 !  K = |                       |
 !      | (-cI*K_c^*)  (-K_r^*) |
 !
 !
 use electrons,      ONLY:n_sp_pol
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                         PAR_COM_Xk_ibz_INDEX
 use BS_solvers,     ONLY:BSS_eh_E_SOC_corr,&
&                         BSS_dipoles,BSS_eh_table,BSS_eh_E,BSS_eh_f,BSS_eh_W,BSS_eh_Z,BSS_alloc
 use BS,             ONLY:BS_K_dim,BS_nT_grps,BS_T_grp,BS_K_coupling,BS_perturbative_SOC
 use timing_m,       ONLY:timing
 !
#if defined _KERR
 use drivers,        ONLY:l_kerr
 use KERR,           ONLY:BSS_dipoles_kerr,BSS_KERR_alloc
#endif
 !
#if defined _PL
 use drivers,        ONLY:l_photolum
 use PHOTOLUM,       ONLY:BSS_dipoles_PL,BSS_PL_alloc,BSS_PL_f,PL_weights
#endif
 !
 implicit none
 !
 integer      :: iq
 !
 ! Work Space
 !
 integer     :: i_c,H_pos(2),i_g,i_k_bz,i_v,i_cond,i_spin
 !
 !========================
 ! BSS COMPONENTS SECTION
 !========================
 ! 
 if (.not.allocated(BSS_dipoles)) then
   !
   call timing('BSE components folding',OPR='start')
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
#if defined _PL
   call BSS_PL_alloc( )
#endif
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
       if (BS_perturbative_SOC) BSS_eh_E_SOC_corr(:,H_pos(1))=BS_T_grp(i_g)%E_SOC_corr(:,i_c)
       !
#if defined _KERR
       if (l_kerr) then
         BSS_dipoles_kerr(H_pos(1))=BS_T_grp(i_g)%dipoles_R_kerr(i_c)
         if (BS_K_coupling) BSS_dipoles_kerr(H_pos(1)+BS_K_dim)=BS_T_grp(i_g)%dipoles_A_kerr(i_c)
       endif
#endif
       !
#if defined _PL
       if (l_photolum) then
         BSS_PL_f(:,H_pos(1))=BS_T_grp(i_g)%f_PL(:,i_c)
         BSS_dipoles_PL(H_pos(1),:)=BS_T_grp(i_g)%dipoles_R_PL(i_c,:)*PL_weights(:)
         if (BS_K_coupling) BSS_dipoles_PL(H_pos(1)+BS_K_dim,:)=BS_T_grp(i_g)%dipoles_A_PL(i_c,:)*PL_weights(:)
       endif
#endif
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
#if defined _PL
   call PP_redux_wait(BSS_dipoles_PL,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(BSS_dipoles_PL,COMM=PAR_COM_eh_INDEX%COMM)
   call PP_redux_wait(BSS_PL_f,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(BSS_PL_f,COMM=PAR_COM_eh_INDEX%COMM)
#endif
   if ( BS_perturbative_SOC ) then
     call PP_redux_wait(BSS_eh_E_SOC_corr,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
     call PP_redux_wait(BSS_eh_E_SOC_corr,COMM=PAR_COM_eh_INDEX%COMM)
   endif
   !
   call timing('BSE components folding',OPR='stop')
   !
 endif
 !
end subroutine K_components_folded_in_serial_arrays

!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP AC
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
!> @brief Calculate exciton-dipole
!!
subroutine EXC_dipole(Xk,E,q,X)
 !
 use pars,           ONLY:pi,SP,rZERO
 use com,            ONLY:msg
 use stderr,         ONLY:intc
 use R_lattice,      ONLY:d3k_factor,bare_qpg,bz_samp
 use DIPOLES,        ONLY:DIPOLE_t,DIPOLES_reset
 use electrons,      ONLY:levels,spin_occ
 use EXCPH,          ONLY:EXC_q0,BS_R_left,BS_R_right,BS_R,EXCPH_states,BS_E_in
 use vec_operate,    ONLY:v_norm
 use X_m,            ONLY:X_t
 use BS,             ONLY:BS_bands,BS_T_grp,BSE_prop,BS_dip_size,BS_nT_grps
 use BS_solvers,     ONLY:BS_H_dim,BSS_Vnl_included,BSS_alloc,BSS_free
 use parallel_int,   ONLY:PARALLEL_global_indexes,PP_redux_wait,PARALLEL_index,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,     ONLY:PAR_IND_WF_b_and_k
 !
#include<memory.h>
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: Xk,q
 type(X_t),     intent(in) :: X
 !
 !
 ! Work Space
 !
 integer :: i_alpha,i_BS_mat
 type(DIPOLE_t) :: DIPs
 !
 if(any(abs(EXC_q0(:))>epsilon(1.))) then
   !
   !
   call DIPOLES_reset(DIPs)
   !
   DIPs%q0=EXC_q0/v_norm(EXC_q0)
   call msg('rs','Excitonic dipole along the direction : ', DIPs%q0)
   !
   DIPs%ib          =BS_bands(1:2)
   DIPs%ib_lim(1)   =E%nbf(1)
   DIPs%ib_lim(2)   =E%nbf(1)+1
!   
!  Unfortunatelly BSS_Vnl_included and DIPs%computed 
!  are not saved by the BS_solves I assume .TRUE. and 'R V P'
!
   DIPs%Vnl_included=.TRUE. 
   DIPs%computed    ='R V P'
   !
   call msg('s','Dipole bands range : ',BS_bands)
   call msg('s','Dipole Vnl included:  ',BSS_Vnl_included)
   !
   ! ... PARALLEL setup
   !
   call PARALLEL_global_indexes(E,Xk,q," ",RESET=.TRUE.)
   call PARALLEL_global_indexes(E,Xk,q,"Response_T_space",X=X)
   !
   ! ... EH space (no parallelization)
   call K_Transitions_setup("K",1,E,Xk,DIPs)
   ! 
   !... Parallel distribution
   call PARALLEL_global_Response_T_transitions(Xk)
   !
   ! ... EH space (with parallelization) and Transition groups build-up
   call K_Transitions_setup("T",1,E,Xk,DIPs)
   !
   !... Blocks build-up
   call K_blocks()
   !
   ! Fill BSS_dipole_opt to be done
   call K_dipoles(1,E,Xk,DIPs)
   !
   ! I deallocate BSS_eh_table to force the code
   ! to reconstruct all elements
   ! 
   call BSS_free() 
   !
   call K_components_folded_in_serial_arrays(1)
   !
   ! Recalculate the residuals
   !
   i_BS_mat=1
   call K_diago_hermitian_residuals(i_BS_mat,BS_E_in,BS_R_left,BS_R_right)
   !
 else
   !      
   call msg('rs','Use dipole of the Lin') 
   !
 endif
 !
 ! Calculate the full residual
 !
 BS_R=rZERO
 do i_alpha=EXCPH_states(1),EXCPH_states(2)
   BS_R(i_alpha)=real(abs(BS_R_left(i_alpha)*BS_R_right(i_alpha)),SP)
 enddo
 !
 ! This formula can be wrong in presence of the 2D-cutoff
 BS_R=BS_R*real(spin_occ,SP)/(2._SP*pi)**3*d3k_factor*4._SP*pi/bare_qpg(1,1)**2
 !
 call PARALLEL_global_indexes(E,Xk,q,"Response_T_space",X=X,RESET=.TRUE.)
 !
end subroutine

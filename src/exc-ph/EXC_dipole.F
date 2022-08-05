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
subroutine EXC_dip()
 !
 use pars,           ONLY:pi,SP,rZERO
 use com,            ONLY:msg
 use R_lattice,      ONLY:d3k_factor,bare_qpg
 use electrons,      ONLY:spin_occ
 use EXCPH,          ONLY:EXC_q0,BS_R_left,BS_R_right,BS_R,EXCPH_states,BS_E_in
 use vec_operate,    ONLY:v_norm
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer :: i_alpha,i_BS_mat
 !
 if(any(abs(EXC_q0(:))>epsilon(1.))) then
   !      
   EXC_q0=EXC_q0/v_norm(EXC_q0)
   call msg('rs','Excitonic dipole along the direction : ',EXC_q0)
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
 ! Calculate the residual
 !
 BS_R=rZERO
 do i_alpha=EXCPH_states(1),EXCPH_states(2)
   BS_R(i_alpha)=real(abs(BS_R_left(i_alpha)*BS_R_right(i_alpha)),SP)
 enddo
 !
 ! This formula can be wrong in presence of the 2D-cutoff
 BS_R=BS_R*real(spin_occ,SP)/(2._SP*pi)**3*d3k_factor*4._SP*pi/bare_qpg(1,1)**2
 !
end subroutine

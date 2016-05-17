!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine PL_diago_residual(BS_E_real,BS_V_left,BS_V_right,BS_R_PL,K_is_not_hermitian) 
 !
 ! PL Residuals
 !
 use pars,           ONLY:SP,rZERO,cZERO
 use wrapper_omp,    ONLY:V_dot_V_omp
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,     ONLY:PARALLEL_index
 use LIVE_t,         ONLY:live_timing
 use BS,             ONLY:BS_H_dim,BS_mat,BSS_eh_E,BSS_eh_Z,BSS_eh_f
 use PHOTOLUM,       ONLY:BSS_dipoles_PL,BSS_PL_f
 !
 implicit none
 !
 logical,    intent(in)  :: K_is_not_hermitian
 complex(SP),intent(in)  :: BS_E_real(BS_H_dim),BS_V_left(BS_H_dim,BS_H_dim),BS_V_right(BS_H_dim,BS_H_dim)
 real(SP)   ,intent(out) :: BS_R_PL(BS_H_dim,3)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i_l,i_c,i_eh
 real(SP)             ::Z_
 complex(SP)          ::left_dipole(BS_H_dim,3),right_dipole(BS_H_dim,3),R1,R2,R3,middle_R(BS_H_dim)
 !
 ! Parallel indexes
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BS_H_dim/))
 !
 ! Zeroing
 !
 BS_R_PL     =rZERO
 left_dipole =cZERO
 right_dipole=cZERO
 middle_R    =cZERO
 !
 ! Left/Right residuals
 !
 do i_eh=1,BS_H_dim
   !
   if (.not.px%element_1D(i_eh)) cycle
   !
   Z_=1._SP
   if (allocated(BSS_eh_Z)) Z_=BSS_eh_Z(i_l)
   !
   left_dipole(i_eh,1)=(BSS_dipoles_PL(i_eh,2)+BSS_dipoles_PL(i_eh,3))*sqrt(BSS_eh_f(i_eh)*Z_)
   left_dipole(i_eh,2)=(BSS_dipoles_PL(i_eh,1)+BSS_dipoles_PL(i_eh,3))*sqrt(BSS_eh_f(i_eh)*Z_)
   left_dipole(i_eh,3)=(BSS_dipoles_PL(i_eh,1)+BSS_dipoles_PL(i_eh,2))*sqrt(BSS_eh_f(i_eh)*Z_)
   !
   right_dipole(i_eh,:)=BSS_dipoles_PL(i_eh,:)*sqrt(BSS_eh_f(i_eh)*Z_)
   !
 enddo
 !
 call PP_redux_wait(right_dipole)
 call PP_redux_wait(left_dipole)
 !
 ! Live-Timing
 !
 call live_timing('PL    residuals',px%n_of_elements(myid+1))
 !
 ! Main residual
 !
 do i_l=1,BS_H_dim
   !
   if (.not.px%element_1D(i_l)) cycle
   !
   middle_R(:)=BSS_PL_f(:)*BS_V_right(:,i_l)
   !
   do i_c=1,3
     !
     if (K_is_not_hermitian) then
     else
       R1=V_dot_V_omp(BS_H_dim,left_dipole(:,i_c),BS_mat(:,i_l))
       R2=V_dot_V_omp(BS_H_dim,conjg(BS_mat(:,i_l)),middle_R)
       R3=V_dot_V_omp(BS_H_dim,BS_mat(:,i_l),right_dipole(:,i_c))
       R3=conjg(R3)
       BS_R_PL(i_l,i_c)=real(R1*R2*R3)
     endif
     !
   enddo
   !
   call live_timing(steps=1)
 enddo
 call live_timing()
 !
 call PP_redux_wait(BS_R_PL)
 !
 call PP_indexes_reset(px)
 !
end subroutine PL_diago_residual

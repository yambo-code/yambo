!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, MG, DS
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
subroutine K_diago_herm_magnons_residual(BS_E,BS_R_left,BS_R_right,BS_A_left,BS_A_right) 
 !
 ! Resonant Residuals
 !====================
 ! BS_R_right(i) = \sum_k <q0|k><k|i> =
 !                 \sum_k BS_mat(k,i) x conjg( BSS_dipoles(k) )
 ! BS_R_left(i)  = conjg( BS_R_right(i) )
 !
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:V_dot_V_omp
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,     ONLY:PARALLEL_index
 use LIVE_t,         ONLY:live_timing
 use X_m,            ONLY:global_gauge
 use BS_solvers,     ONLY:BSS_eh_Z,BSS_eh_f,BS_mat,BSS_n_eig
 use BS,             ONLY:BS_K_dim
 use MAGNONS,        ONLY:BSS_dipoles_magn
 !
 implicit none
 !
 complex(SP),intent(in)  :: BS_E(BSS_n_eig)
 complex(SP),intent(out) :: BS_R_left(2,BSS_n_eig),BS_R_right(2,BSS_n_eig)
 complex(SP),intent(out) :: BS_A_left(2,BSS_n_eig),BS_A_right(2,BSS_n_eig)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i_lambda
 complex(SP)          ::tmp_res(BS_K_dim,2)
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BSS_n_eig/))
 !
 BS_R_left =cZERO
 BS_R_right=cZERO
 call live_timing('MAG Res residuals',px%n_of_elements(myid+1))
 !
 tmp_res(:,1) = BSS_dipoles_magn(1,:) *sqrt(BSS_eh_f(:))
 tmp_res(:,2) = BSS_dipoles_magn(2,:) *sqrt(BSS_eh_f(:))
 !
 if (allocated(BSS_eh_Z)) then
   tmp_res(:,1)=tmp_res(:,1)*sqrt(BSS_eh_Z(:))
   tmp_res(:,2)=tmp_res(:,2)*sqrt(BSS_eh_Z(:))
 endif
 !
 do i_lambda=1,BSS_n_eig
   !
   if (.not.px%element_1D(i_lambda)) cycle
   !
   BS_R_left(1,i_lambda) =V_dot_V_omp(BS_K_dim,tmp_res(:,1),conjg(BS_mat(:,i_lambda)))
   BS_R_left(2,i_lambda) =V_dot_V_omp(BS_K_dim,tmp_res(:,2),conjg(BS_mat(:,i_lambda)))
   BS_R_right(:,i_lambda)=conjg(BS_R_left(:,i_lambda))
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 call PP_redux_wait(BS_R_left)
 call PP_redux_wait(BS_R_right)
 !
 ! Anti-Resonant part
 !
 BS_A_left =cZERO
 BS_A_right=cZERO
 call live_timing('MAG Ares residuals',px%n_of_elements(myid+1))
 !
 tmp_res(:,1) = conjg(BSS_dipoles_magn(2,:))*sqrt(BSS_eh_f(:))
 tmp_res(:,2) = conjg(BSS_dipoles_magn(1,:))*sqrt(BSS_eh_f(:))
 !
 if (allocated(BSS_eh_Z)) then
   tmp_res(:,1)=tmp_res(:,1)*sqrt(BSS_eh_Z(:))
   tmp_res(:,2)=tmp_res(:,2)*sqrt(BSS_eh_Z(:))
 endif
 !
 do i_lambda=1,BSS_n_eig
   !
   if (.not.px%element_1D(i_lambda)) cycle
   !
   BS_A_left(1,i_lambda) =V_dot_V_omp(BS_K_dim,tmp_res(:,1),BS_mat(:,i_lambda))
   BS_A_left(2,i_lambda) =V_dot_V_omp(BS_K_dim,tmp_res(:,2),BS_mat(:,i_lambda))
   BS_A_right(:,i_lambda)=conjg(BS_A_left(:,i_lambda))
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 call PP_redux_wait(BS_A_left)
 call PP_redux_wait(BS_A_right)
 !
 call PP_indexes_reset(px)
 !
end subroutine K_diago_herm_magnons_residual

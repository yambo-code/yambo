!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine K_diago_hermitian_residuals(BS_E_real,BS_R_left,BS_R_right) 
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
 use BS_solvers,     ONLY:BSS_dipoles,BSS_eh_E,BSS_eh_Z,BSS_eh_f,BS_mat,BSS_n_eig
 use BS,             ONLY:BS_H_dim
 !
 implicit none
 !
 real(SP)   ,intent(in)  :: BS_E_real(BS_H_dim)
 complex(SP),intent(out) :: BS_R_left(BS_H_dim),BS_R_right(BS_H_dim)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i_eh
 complex(SP)          ::tmp_res(BS_H_dim)
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BS_H_dim/))
 !
 BS_R_left =cZERO
 BS_R_right=cZERO
 call live_timing('EPS   residuals',px%n_of_elements(myid+1))
 !
 tmp_res = BSS_dipoles*sqrt(BSS_eh_f)
 !
 if (trim(global_gauge)=="velocity") tmp_res=tmp_res/BSS_eh_E
 if (allocated(BSS_eh_Z))            tmp_res=tmp_res*sqrt(BSS_eh_Z)
 !
 do i_eh=1,BSS_n_eig
   !
   if (.not.px%element_1D(i_eh)) cycle
   !
   BS_R_left(i_eh) =V_dot_V_omp(BS_H_dim,tmp_res,conjg(BS_mat(:,i_eh)))
   BS_R_right(i_eh)=conjg(BS_R_left(i_eh))
   !
   if (trim(global_gauge)=="velocity") then
     BS_R_left(i_eh) = BS_R_left(i_eh) *BS_E_real(i_eh)
     BS_R_right(i_eh)= BS_R_right(i_eh)*BS_E_real(i_eh)
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 call PP_redux_wait(BS_R_left)
 call PP_redux_wait(BS_R_right)
 !
 call PP_indexes_reset(px)
 !
end subroutine K_diago_hermitian_residuals

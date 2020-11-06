!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine K_diago_kerr_residual(BS_E,BS_V_right,BS_R_right_kerr) 
 !
 ! BS_R_right_kerr(i)=<q0_kerr|j><j|i_R>
 !                   =conjg(BSS_dipoles(j))*BS_V_right(j,i)
 !
 use pars,           ONLY:SP,cZERO,cI
 use wrapper_omp,    ONLY:V_dot_V_omp
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,     ONLY:PARALLEL_index
 use LIVE_t,         ONLY:live_timing
 use KERR,           ONLY:BSS_dipoles_kerr
 use X_m,            ONLY:global_gauge
 use BS_solvers,     ONLY:BSS_eh_E,BSS_eh_Z,BSS_eh_f,BSS_n_eig
 use BS,             ONLY:BS_K_dim,BS_H_dim
 !
 implicit none
 !
 complex(SP),intent(in)  :: BS_E(BSS_n_eig)
 complex(SP),intent(in)  :: BS_V_right(BS_H_dim,BSS_n_eig)
 complex(SP),intent(out) :: BS_R_right_kerr(BSS_n_eig)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i1
 !
 complex(SP) :: tmp_res(BS_H_dim)
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BSS_n_eig/))
 !
 BS_R_right_kerr=cZERO
 !
 call live_timing('KERR residuals ',px%n_of_elements(myid+1))
 !
 forall(i1=1:BS_K_dim(1))          tmp_res(i1) =    conjg(BSS_dipoles_kerr(i1))*sqrt(BSS_eh_f(i1)         ) 
 forall(i1=BS_K_dim(1)+1:BS_H_dim) tmp_res(i1) = cI*conjg(BSS_dipoles_kerr(i1))*sqrt(BSS_eh_f(i1-BS_K_dim(1)))
 if (allocated(BSS_eh_Z)           ) tmp_res=tmp_res*sqrt(BSS_eh_Z)
 if (trim(global_gauge)=="velocity") tmp_res=tmp_res/BSS_eh_E
 !
 do i1=1,BSS_n_eig
   if (.not.px%element_1D(i1)) cycle
   !
   BS_R_right_kerr(i1)=V_dot_V_omp(BS_H_dim,tmp_res,BS_V_right(:,i1))
   if (trim(global_gauge)=="velocity") BS_R_right_kerr(i1)=BS_R_right_kerr(i1)*BS_E(i1)
   !
   call live_timing(steps=1)
 enddo
 call PP_redux_wait(BS_R_right_kerr)
 call live_timing
 !
 call PP_indexes_reset(px)
 !
end subroutine K_diago_kerr_residual

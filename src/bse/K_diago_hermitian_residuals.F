!
!        Copyright (C) 2000-2015 the YAMBO team
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
 !                 \sum_k BS_mat(k,i) x conjg( BSS_rhoq0(k) )
 ! BS_R_left(i)  = conjg( BS_R_right(i) )
 !
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:V_dot_V_omp
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,     ONLY:PARALLEL_index
 use LIVE_t,         ONLY:live_timing
 use X_m,            ONLY:global_gauge
 use BS,             ONLY:BSS_rhoq0,BS_H_dim,BS_mat,&
&                         BSS_eh_E,BSS_eh_Z,BSS_eh_f
 !
 implicit none
 !
 real(SP)   ,intent(in)  :: BS_E_real(BS_H_dim)
 complex(SP),intent(out) :: BS_R_left(BS_H_dim),BS_R_right(BS_H_dim)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i1
 !
 complex(SP) :: tmp_res(BS_H_dim)
 !
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BS_H_dim/))
 !
 BS_R_left =cZERO
 BS_R_right=cZERO
 call live_timing('BSK   residuals',px%n_of_elements(myid+1))
 !
 tmp_res = BSS_rhoq0*sqrt(BSS_eh_f)
 if (trim(global_gauge)=="velocity") tmp_res=tmp_res/BSS_eh_E
 !
 do i1=1,BS_H_dim
   if (.not.px%element_1D(i1)) cycle
   !
   if(.not.allocated(BSS_eh_Z)) then
     BS_R_left(i1) =V_dot_V_omp(BS_H_dim,tmp_res,conjg(BS_mat(:,i1)))
     BS_R_right(i1)=conjg(BS_R_left(i1))
   else
     BS_R_left(i1) =V_dot_V_omp(BS_H_dim,      tmp_res *sqrt(BSS_eh_Z),conjg(BS_mat(:,i1)))
     BS_R_right(i1)=V_dot_V_omp(BS_H_dim,conjg(tmp_res)*sqrt(BSS_eh_Z),      BS_mat(:,i1) )
   endif
   !
   if (trim(global_gauge)=="velocity") then
     BS_R_left(i1) =BS_R_left(i1) *BS_E_real(i1)
     BS_R_right(i1)=BS_R_right(i1)*BS_E_real(i1)
   endif
   !
   call live_timing(steps=1)
 enddo
 call live_timing()
 !
 call PP_redux_wait(BS_R_left)
 call PP_redux_wait(BS_R_right)
 !
 call PP_indexes_reset(px)
 !
end subroutine K_diago_hermitian_residuals

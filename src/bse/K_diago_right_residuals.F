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
subroutine K_diago_right_residuals(mode,i_BS_mat,BS_E,BS_R_right,BS_V_right)
 !
 ! BS_R_right_XX(i)=<q0_XX|j><j|i_R>
 !                 =conjg(BSS_dipoles(j))*BS_V_right(j,i)
 !
 use pars,           ONLY:SP,cZERO,cI
 use wrapper_omp,    ONLY:V_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use X_m,            ONLY:global_gauge
 use BS_solvers,     ONLY:BSS_eh_E,BSS_eh_Z,BSS_eh_f,BSS_n_eig,BSS_dipoles_opt
 use BS,             ONLY:BS_K_dim,BS_H_dim,BS_K_coupling
 use MAGNONS,        ONLY:BSS_dipoles_magn
 use DICHROISM,      ONLY:BSS_dipoles_dich
 !
 implicit none
 !
 character(*),intent(in)  :: mode
 integer,     intent(in)  :: i_BS_mat
 complex(SP), intent(in)  :: BS_E(BSS_n_eig)
 complex(SP), intent(out) :: BS_R_right(BSS_n_eig)
 complex(SP), intent(in)  :: BS_V_right(:,:)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i_lambda,n_res,res_range(2)
 logical              ::velocity_correction
 !
 complex(SP),allocatable :: tmp_res(:)
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BSS_n_eig/))
 !
 BS_R_right=cZERO
 velocity_correction=(index(mode,"opt")>0).and.(trim(global_gauge)=="velocity")
 !
 if (.not.BS_K_coupling) res_range=(/1,BS_K_dim(i_BS_mat)/)+BS_K_dim(1)*(i_BS_mat-1)
 if (     BS_K_coupling) res_range=(/1,BS_H_dim/)
 !
 allocate(tmp_res(res_range(1):res_range(2)))
 n_res=res_range(2)-res_range(1)+1
 !
 call live_timing(trim(mode)//' residuals ',px%n_of_elements(myid+1))
 !
 if(trim(mode)=="opt1") tmp_res=BSS_dipoles_opt(1,res_range(1):res_range(2))
 if(trim(mode)=="opt2") tmp_res=BSS_dipoles_opt(2,res_range(1):res_range(2))
 if(trim(mode)=="dic1") tmp_res=BSS_dipoles_dich(1,res_range(1):res_range(2)) 
 if(trim(mode)=="dic2") tmp_res=BSS_dipoles_dich(2,res_range(1):res_range(2)) 
 if(trim(mode)=="mag1") tmp_res=BSS_dipoles_magn(1,res_range(1):res_range(2))
 if(trim(mode)=="mag2") tmp_res=BSS_dipoles_magn(2,res_range(1):res_range(2))
 !
 tmp_res = conjg(tmp_res)*sqrt(cmplx(BSS_eh_f(res_range(1):res_range(2)),kind=SP)) 
 if (allocated(BSS_eh_Z)) tmp_res=tmp_res*sqrt(BSS_eh_Z(res_range(1):res_range(2)))
 if (velocity_correction) tmp_res=tmp_res/BSS_eh_E(res_range(1):res_range(2))
 !
 do i_lambda=1,BSS_n_eig
   if (.not.px%element_1D(i_lambda)) cycle
   !
   BS_R_right(i_lambda)=V_dot_V_omp(n_res,tmp_res,BS_V_right(:,i_lambda))
   if (velocity_correction) BS_R_right(i_lambda)=BS_R_right(i_lambda)*BS_E(i_lambda)
   !
   call live_timing(steps=1)
 enddo
 call PP_redux_wait(BS_R_right)
 call live_timing
 !
 call PP_indexes_reset(px)
 deallocate(tmp_res)
 !
end subroutine K_diago_right_residuals

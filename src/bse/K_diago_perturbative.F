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
subroutine K_diago_perturbative(BS_mat_eigen,BS_corrections,pert_dim,what) 
 !
 ! Perturbative SOC
 !
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:V_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait
 use interfaces,     ONLY:PARALLEL_index
 use LIVE_t,         ONLY:live_timing
 use BS_solvers,     ONLY:BSS_eh_E_SOC_corr,BSS_eh_W,BSS_eh_E,BSS_n_eig
 use BS,             ONLY:BS_H_dim
 !
 implicit none
 !
 character(3),intent(in) :: what
 integer,     intent(in) :: pert_dim
 complex(SP), intent(in) :: BS_mat_eigen(BS_H_dim,BSS_n_eig)
 complex(SP),intent(out) :: BS_corrections(pert_dim,BSS_n_eig)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i1,i2
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BSS_n_eig/))
 !
 BS_corrections=cZERO
 call live_timing('Perturb.    '//what//' ',px%n_of_elements(myid+1))
 !
 do i1=1,BSS_n_eig
   !
   if (.not.px%element_1D(i1)) cycle
   !
   if(what=='SOC') then
     do i2=1,pert_dim
       BS_corrections(i2,i1)=V_dot_V_omp( BS_H_dim,cmplx(BSS_eh_E_SOC_corr(i2,:),0._SP,SP),BS_mat_eigen(:,i1))
     enddo
   endif
   !
   if(what=='WID') then
     BS_corrections(1,i1)=V_dot_V_omp( BS_H_dim,cmplx(0._SP,BSS_eh_W(:),SP),BS_mat_eigen(:,i1))
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 call PP_redux_wait(BS_corrections) 
 !
 call PP_indexes_reset(px)
 !
end subroutine K_diago_perturbative

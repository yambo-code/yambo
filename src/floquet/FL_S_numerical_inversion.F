!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG IA
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
subroutine FL_S_numerical_inversion(en,Xk,FL_Sm1_num_plus,FL_Sm1_num_minus,id_red,istep,i_sp,ikbz)
 !
 use pars,           ONLY:SP,cZERO,cONE,pi,cI
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,k_map
 use fl_optics,      ONLY:FL_S_plus,FL_S_minus,n_tot_fl_modes,n_tot_extra_modes,max_fl_mode,max_extra_mode,&
 &                        n_tot_duffin_modes,max_duffin_mode,FL_space_dim,FL_states,&
 &                        FL_FT_extra_M,FL_FT_extra_Mm1
 use wrapper_omp,    ONLY:M_by_M_omp,M_by_V_omp
 !
 implicit none
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: Xk
 integer,       intent(in) :: istep
 integer,       intent(in) :: id_red
 integer,       intent(in) :: i_sp
 integer,       intent(in) :: ikbz
 !
 complex(SP),  intent(inout) :: FL_Sm1_num_minus(en%nbf,en%nbf,n_tot_extra_modes)
 complex(SP),  intent(inout) :: FL_Sm1_num_plus(en%nbf,en%nbf,n_tot_extra_modes)
 !
 ! Work space
 !
 complex(SP) :: FL_S_t_plus(en%nbf,en%nbf,n_tot_extra_modes)
 complex(SP) :: FL_S_t_minus(en%nbf,en%nbf,n_tot_extra_modes)
 !
 !
 complex(SP) :: FL_Duffin_Gmatrix_plus(en%nbf,en%nbf,n_tot_duffin_modes,n_tot_duffin_modes)
 complex(SP) :: FL_Duffin_Gmatrix_minus(en%nbf,en%nbf,n_tot_duffin_modes,n_tot_duffin_modes)
 complex(SP) :: tmp_M(en%nbf,en%nbf)
 complex(SP) :: test_mat_1(en%nbf*n_tot_fl_modes,en%nbf*n_tot_fl_modes)
 complex(SP) :: test_mat_2(en%nbf*n_tot_fl_modes,en%nbf*n_tot_fl_modes)
 complex(SP) :: FL_Duffin_BIG_MATRIX_plus(en%nbf*n_tot_duffin_modes,en%nbf*n_tot_duffin_modes)
 complex(SP) :: FL_Duffin_BIG_MATRIX_minus(en%nbf*n_tot_duffin_modes,en%nbf*n_tot_duffin_modes)
 complex(SP) :: FL_Duffin_BIG_VECTOR_plus(en%nbf*n_tot_duffin_modes)
 complex(SP) :: FL_Duffin_BIG_VECTOR_minus(en%nbf*n_tot_duffin_modes)
 complex(SP) :: vector_b(en%nbf*n_tot_fl_modes)
 complex(SP) :: vtmp(en%nbf*n_tot_duffin_modes)
 complex(SP) :: diagonal_elements(en%nbf),sum_minus_trace,dummy_det
 integer     :: time_samples
 !
 integer :: i1,i2,iv1,im,idx(3),id,ikp,ikm,i_t
 integer :: i1_fl,i2_fl,i3_fl,i4_fl,i_m,i_comb_fl,i_shift_comb_fl,i1_fks,i2_fks,i1_inverted_fl,i1_df,i2_df
 integer :: i1_extra,i2_extra,i3_extra,i_comb_extra,i_start,i_finish,i1_extra_inverted,i3_extra_inverted,i1_shift_extra
 logical :: l_solve_linear_problem,l_debug_printouts
 !
 !
 l_debug_printouts = .false.
 !
 FL_S_t_plus  = cZERO
 FL_S_t_minus = cZERO
 !
 do i_t=1,n_tot_extra_modes ! loop on time
   !
   do i1_extra=1,n_tot_extra_modes
     !
     i1_shift_extra = i1_extra -(max_extra_mode+1)
     !
     FL_S_t_plus(:,:,i_t)  = FL_S_t_plus(:,:,i_t) &
                  &        + exp(-1._SP*cI*i1_shift_extra*2._SP*pi*(i_t-1._SP)/n_tot_extra_modes) &
                  &        * FL_S_plus(:,:,i1_extra,id_red+(istep-1)*3,ikbz,i_sp)
     !
     FL_S_t_minus(:,:,i_t) = FL_S_t_minus(:,:,i_t) &
                  &        + exp(-1._SP*cI*i1_shift_extra*2._SP*pi*(i_t-1._SP)/n_tot_extra_modes) &
                  &        * FL_S_minus(:,:,i1_extra,id_red+(istep-1)*3,ikbz,i_sp)
     !
   enddo
   !
   call SERIAL_inversion(en%nbf,FL_S_t_plus(:,:,i_t),dummy_det,.false.)
   call SERIAL_inversion(en%nbf,FL_S_t_minus(:,:,i_t),dummy_det,.false.)
   !
 enddo ! loop on time
 !
 !
 do i1=1,en%nbf
   do i2=1,en%nbf
   !
   call M_by_V_omp('N',n_tot_extra_modes,FL_FT_extra_Mm1,FL_S_t_plus(i1,i2,:), FL_Sm1_num_plus(i1,i2,:))
   call M_by_V_omp('N',n_tot_extra_modes,FL_FT_extra_Mm1,FL_S_t_minus(i1,i2,:),FL_Sm1_num_minus(i1,i2,:))
   !
   enddo
 enddo
 !
 !aim137 debug (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
 !call FL_check_S_inversion(&
 !        &     FL_S_plus(:,:,:,id_red+(istep-1)*3,ikbz,i_sp),&
 !        &     FL_Sm1_num_plus,en%nbf,n_tot_extra_modes,n_tot_extra_modes,&
 !        &     diagonal_elements,sum_minus_trace,100,l_debug_printouts)
 !
 !print*, ''
 !write(*,'(I2,I2,I2,100(1f13.7))') ikbz, id_red, istep, (real(diagonal_elements(i1)), i1=1,en%nbf)
  
 !if (ikbz == 3) then
 !  if (id_red == 2) then
 !    if (istep == 2) then
 !      call error('        stop here')
 !    endif
 !  endif
 !endif
 !aim137 debug )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 !
end subroutine FL_S_numerical_inversion

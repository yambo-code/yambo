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
subroutine FL_Duffin(en,Xk,FL_Sm1_Duffin_plus,FL_Sm1_Duffin_minus,id_red,istep,i_sp,ikbz)
 !
 use pars,           ONLY:SP,cZERO,cONE
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,k_map
 use fl_optics,      ONLY:FL_S_plus,FL_S_minus,n_tot_fl_modes,max_fl_mode,FL_space_dim,FL_states
 use wrapper_omp,    ONLY:M_by_M_omp
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
 complex(SP),  intent(out) :: FL_Sm1_Duffin_minus(en%nbf,en%nbf,n_tot_fl_modes)
 complex(SP),  intent(out) :: FL_Sm1_Duffin_plus(en%nbf,en%nbf,n_tot_fl_modes)
 !
 ! Work space
 !
 complex(SP) :: FL_Duffin_Gmatrix_plus(en%nbf,en%nbf,n_tot_fl_modes,n_tot_fl_modes)
 complex(SP) :: FL_Duffin_Gmatrix_minus(en%nbf,en%nbf,n_tot_fl_modes,n_tot_fl_modes)
 complex(SP) :: tmp_M(en%nbf,en%nbf)
 complex(SP) :: FL_Duffin_BIG_MATRIX_plus(en%nbf*n_tot_fl_modes,en%nbf*n_tot_fl_modes)
 complex(SP) :: FL_Duffin_BIG_MATRIX_minus(en%nbf*n_tot_fl_modes,en%nbf*n_tot_fl_modes)
 complex(SP) :: FL_Duffin_BIG_VECTOR_plus(en%nbf*n_tot_fl_modes)
 complex(SP) :: FL_Duffin_BIG_VECTOR_minus(en%nbf*n_tot_fl_modes)
 complex(SP) :: trace_over_dim,sum_minus_trace
 real(SP)    :: time
 !
 integer :: i1,i2,iv1,im,idx(3),id,ikp,ikm
 integer :: i1_fl,i2_fl,i3_fl,i4_fl,i_m,i_comb_fl,i_shift_comb_fl,i1_fks,i2_fks,i1_inverted_fl
 !
 !
 ! Build G matrix
 ! ======================
 !
 do i1_fl=1,n_tot_fl_modes
   do i2_fl=1,n_tot_fl_modes
     !
     FL_Duffin_Gmatrix_plus(:,:,i1_fl,i2_fl) = cZERO
     FL_Duffin_Gmatrix_minus(:,:,i1_fl,i2_fl) = cZERO
     !
     do i3_fl=1,n_tot_fl_modes
       !
       i_shift_comb_fl = (i1_fl-(max_fl_mode+1)) &
                      &- (i2_fl-(max_fl_mode+1)) &
                      &+ (i3_fl-(max_fl_mode+1))
       if (abs(i_shift_comb_fl) > max_fl_mode) cycle
       i_comb_fl = i_shift_comb_fl + (max_fl_mode+1)
       !
       call M_by_M_omp('N','N',en%nbf,en%nbf,en%nbf,cONE,  &
      &  FL_S_plus(:,:,i_comb_fl,id_red+(istep-1)*3,ikbz,i_sp),en%nbf, &
      &  conjg(FL_S_plus(:,:,i3_fl,id_red+(istep-1)*3,ikbz,i_sp)),en%nbf,cZERO,&
      &  tmp_M,en%nbf)
       !
       FL_Duffin_Gmatrix_plus(:,:,i1_fl,i2_fl) = FL_Duffin_Gmatrix_plus(:,:,i1_fl,i2_fl) + tmp_M
       !
       call M_by_M_omp('N','N',en%nbf,en%nbf,en%nbf,cONE,  &
      &  FL_S_minus(:,:,i_comb_fl,id_red+(istep-1)*3,ikbz,i_sp),en%nbf, &
      &  conjg(FL_S_minus(:,:,i3_fl,id_red+(istep-1)*3,ikbz,i_sp)),en%nbf,cZERO,&
      &  tmp_M,en%nbf)
       !
       FL_Duffin_Gmatrix_minus(:,:,i1_fl,i2_fl) = FL_Duffin_Gmatrix_minus(:,:,i1_fl,i2_fl) + tmp_M
       !
     enddo
       !
   enddo
 enddo
 ! 
 ! Build Duffin BIG MATRIX
 ! ========================
 ! aim137: this is a big matrix. is this deallocated when the subroutine finishes as it is not intent in/out?
 !
 do i1=1,en%nbf
   do i2=1,en%nbf
     do i1_fl=1,n_tot_fl_modes
       do i2_fl=1,n_tot_fl_modes
         !
         i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
         i2_fks = i2_fl + (i2-1)*n_tot_fl_modes
         !
         FL_Duffin_BIG_MATRIX_plus(i1_fks,i2_fks)=FL_Duffin_Gmatrix_plus(i1,i2,i1_fl,i2_fl)
         FL_Duffin_BIG_MATRIX_minus(i1_fks,i2_fks)=FL_Duffin_Gmatrix_minus(i1,i2,i1_fl,i2_fl)
         !
       enddo
     enddo
   enddo
 enddo
 ! 
 ! 
 do i2=1,en%nbf
   !
   ! Build Duffin BIG VECTORS
   ! ========================
   !
   do i1=1,en%nbf
     do i1_fl=1,n_tot_fl_modes
     !
     i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
     i1_inverted_fl =  (2*max_fl_mode+1) + 1 - i1_fl
     !
     FL_Duffin_BIG_VECTOR_plus(i1_fks)  = conjg(FL_S_plus(i1,i2,i1_inverted_fl,id_red+(istep-1)*3,ikbz,i_sp))
     FL_Duffin_BIG_VECTOR_minus(i1_fks) = conjg(FL_S_minus(i1,i2,i1_inverted_fl,id_red+(istep-1)*3,ikbz,i_sp))
     !
     enddo
   enddo
   ! 
   ! Solve linear problem
   ! ====================
   !
   call SERIAL_lin_system(en%nbf*n_tot_fl_modes,1,FL_Duffin_BIG_MATRIX_plus,FL_Duffin_BIG_VECTOR_plus)
   call SERIAL_lin_system(en%nbf*n_tot_fl_modes,1,FL_Duffin_BIG_MATRIX_minus,FL_Duffin_BIG_VECTOR_minus)
   !aim137: I understand A x = b returns b in the variable where x was passed on
   !
   !
   ! Scatter to Inverse matrix
   !
   do i1=1,en%nbf
     do i1_fl=1,n_tot_fl_modes
     !
     i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
     !
     FL_Sm1_Duffin_plus(i1,i2,i1_fl) = FL_Duffin_BIG_VECTOR_plus(i1_fks)
     FL_Sm1_Duffin_minus(i1,i2,i1_fl) = FL_Duffin_BIG_VECTOR_minus(i1_fks)
     !
     enddo
   enddo
   !
 enddo ! loop over i2
 ! 
 call FL_check_Duffin_inverse(FL_S_plus(:,:,:,id_red+(istep-1)*3,ikbz,i_sp),&
         &                    FL_Sm1_Duffin_plus,time,en%nbf,trace_over_dim,sum_minus_trace)
 !
 print*, 'uno igual a ', trace_over_dim
 !print*, 'cero igual a ', sum_minus_trace
 !
end subroutine FL_Duffin
!
!
subroutine FL_check_Duffin_inverse(FL_matrix,FL_inverse,time,dim_of_matrix,trace_over_dim,sum_minus_trace)
  !
  use pars,           ONLY:SP,cZERO,cONE
  use fl_optics,      ONLY:n_tot_fl_modes
  use wrapper_omp,    ONLY:M_by_M_omp
  !
  implicit none
  !
  complex(SP), intent(in) :: FL_matrix(dim_of_matrix,dim_of_matrix,n_tot_fl_modes)
  complex(SP), intent(in) :: FL_inverse(dim_of_matrix,dim_of_matrix,n_tot_fl_modes)
  real(SP),    intent(in) :: time
  integer,     intent(in) :: dim_of_matrix
  complex(SP), intent(out) :: trace_over_dim, sum_minus_trace 
  !
  ! Work Space
  complex(SP)  :: matrix_at_t(dim_of_matrix,dim_of_matrix)
  complex(SP)  :: inverse_at_t(dim_of_matrix,dim_of_matrix)
  complex(SP)  :: identity_at_t(dim_of_matrix,dim_of_matrix)
  complex(SP)  :: trace
  integer      :: i1,i1_fl
  !
  matrix_at_t = cZERO
  inverse_at_t = cZERO
  !
  do i1_fl=1,n_tot_fl_modes
    !
    matrix_at_t = matrix_at_t + FL_matrix(:,:,i1_fl)
    inverse_at_t = inverse_at_t + FL_inverse(:,:,i1_fl)
    !
  enddo
  !
  call M_by_M_omp('N','N',dim_of_matrix,dim_of_matrix,dim_of_matrix,cONE,  &
                       &  matrix_at_t,dim_of_matrix, &
                       &  inverse_at_t,dim_of_matrix,cZERO,&
                       &  identity_at_t,dim_of_matrix)
  !
  trace = cZERO
  do i1=1,dim_of_matrix 
    trace=trace+identity_at_t(i1,i1)
  enddo
  sum_minus_trace = sum(identity_at_t) - trace
  trace_over_dim = trace / dim_of_matrix
  !
end subroutine FL_check_Duffin_inverse

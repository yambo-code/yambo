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
 use pars,           ONLY:SP,cZERO,cONE,pi
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,k_map
 use fl_optics,      ONLY:FL_S_plus,FL_S_minus,n_tot_fl_modes,n_tot_extra_modes,max_fl_mode,max_extra_mode,FL_space_dim,FL_states
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
 complex(SP),  intent(out) :: FL_Sm1_Duffin_minus(en%nbf,en%nbf,n_tot_fl_modes)
 complex(SP),  intent(out) :: FL_Sm1_Duffin_plus(en%nbf,en%nbf,n_tot_fl_modes)
 !
 ! Work space
 !
 complex(SP) :: FL_Duffin_Gmatrix_plus(en%nbf,en%nbf,n_tot_fl_modes,n_tot_fl_modes)
 complex(SP) :: FL_Duffin_Gmatrix_minus(en%nbf,en%nbf,n_tot_fl_modes,n_tot_fl_modes)
 complex(SP) :: tmp_M(en%nbf,en%nbf)
 complex(SP) :: test_mat_1(en%nbf*n_tot_fl_modes,en%nbf*n_tot_fl_modes)
 complex(SP) :: test_mat_2(en%nbf*n_tot_fl_modes,en%nbf*n_tot_fl_modes)
 complex(SP) :: FL_Duffin_BIG_MATRIX_plus(en%nbf*n_tot_fl_modes,en%nbf*n_tot_fl_modes)
 complex(SP) :: FL_Duffin_BIG_MATRIX_minus(en%nbf*n_tot_fl_modes,en%nbf*n_tot_fl_modes)
 complex(SP) :: FL_Duffin_BIG_VECTOR_plus(en%nbf*n_tot_fl_modes)
 complex(SP) :: vector_b(en%nbf*n_tot_fl_modes)
 complex(SP) :: vtmp(en%nbf*n_tot_fl_modes)
 complex(SP) :: FL_Duffin_BIG_VECTOR_minus(en%nbf*n_tot_fl_modes)
 complex(SP) :: diagonal_elements(en%nbf),sum_minus_trace,dummy_det
 integer     :: time_samples
 !
 integer :: i1,i2,iv1,im,idx(3),id,ikp,ikm
 integer :: i1_fl,i2_fl,i3_fl,i4_fl,i_m,i_comb_fl,i_shift_comb_fl,i1_fks,i2_fks,i1_inverted_fl
 integer :: i1_extra,i2_extra,i3_extra,i_comb_extra,i_start,i_finish,i1_extra_inverted
 logical :: l_solve_linear_problem,l_debug_printouts
 !
 !
 l_solve_linear_problem = .false.
 l_debug_printouts = .true.
 !
 ! Using an example matrix
 ! =======================
 !
 ! FL_S_plus = cZERO
 ! FL_S_plus(1,1,1+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 2
 ! FL_S_plus(1,1,0+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 4
 ! FL_S_plus(1,2,5+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 7
 ! FL_S_plus(1,2,0+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 13*cONE
 ! FL_S_plus(2,1,2+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 5*cONE
 ! FL_S_plus(2,1,0+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 9
 ! FL_S_plus(3,3,1+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 2
 ! FL_S_plus(3,3,0+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 4
 ! FL_S_plus(3,4,5+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 7
 ! FL_S_plus(3,4,0+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 13*cONE
 ! FL_S_plus(4,3,2+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 5*cONE
 ! FL_S_plus(4,3,0+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 9
 ! FL_S_plus(5,3,3+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 5*cONE
 ! FL_S_plus(5,3,1+max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp) = 9
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
       if (abs(i_shift_comb_fl) > max_extra_mode) cycle !the criterion changes
       i3_extra = i3_fl + max_extra_mode - max_fl_mode
       i_comb_fl    = i_shift_comb_fl + (max_fl_mode+1)
       i_comb_extra = i_shift_comb_fl + (max_extra_mode+1)
       !
       call M_by_M_omp('N','N',en%nbf,en%nbf,en%nbf,cONE,  &
      &  FL_S_plus(:,:,i_comb_extra,id_red+(istep-1)*3,ikbz,i_sp),en%nbf, &
      &  conjg(FL_S_plus(:,:,i3_extra,id_red+(istep-1)*3,ikbz,i_sp)),en%nbf,cZERO,&
      &  tmp_M,en%nbf)
       !
       FL_Duffin_Gmatrix_plus(:,:,i1_fl,i2_fl) = FL_Duffin_Gmatrix_plus(:,:,i1_fl,i2_fl) + tmp_M
       !
       call M_by_M_omp('N','N',en%nbf,en%nbf,en%nbf,cONE,  &
      &  FL_S_minus(:,:,i_comb_extra,id_red+(istep-1)*3,ikbz,i_sp),en%nbf, &
      &  conjg(FL_S_minus(:,:,i3_extra,id_red+(istep-1)*3,ikbz,i_sp)),en%nbf,cZERO,&
      &  tmp_M,en%nbf)
       !
       FL_Duffin_Gmatrix_minus(:,:,i1_fl,i2_fl) = FL_Duffin_Gmatrix_minus(:,:,i1_fl,i2_fl) + tmp_M
       !
     enddo
       !
   enddo
 enddo
 !
 !aim137 debug (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
 if (l_debug_printouts) then
   open(111,file='G_matrix_real.dat')
     do i1_fl=1,n_tot_fl_modes
       write(111,'(100(1f7.4))') real(FL_Duffin_Gmatrix_plus(1,1,i1_fl,:))
     enddo
   close(111)
   open(111,file='G_matrix_complex.dat')
     do i1_fl=1,n_tot_fl_modes
       write(111,'(100(1f7.4))') aimag(FL_Duffin_Gmatrix_plus(1,1,i1_fl,:))
     enddo
   close(111)
 endif
 !aim137 debug )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
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
         i1_fks = i1_fl + (i2-1)*n_tot_fl_modes ! note i2
         i2_fks = i2_fl + (i1-1)*n_tot_fl_modes ! note i1
         !
         !                                                               !aim137 i1,i2 o i2,i1?
         FL_Duffin_BIG_MATRIX_plus(i1_fks,i2_fks)=FL_Duffin_Gmatrix_plus(i1,i2,i1_fl,i2_fl)
         FL_Duffin_BIG_MATRIX_minus(i1_fks,i2_fks)=FL_Duffin_Gmatrix_minus(i1,i2,i1_fl,i2_fl)
         !
       enddo
     enddo
   enddo
 enddo
 !
 !aim137 debug (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
 if (l_debug_printouts) then
   open(111,file='BigG_matrix_real.dat')
     do i1_fks=1,en%nbf*n_tot_fl_modes
       write(111,'(100(1f7.4))') real(FL_Duffin_BIG_MATRIX_plus(i1_fks,:))
     enddo
   close(111)
 endif
 !aim137 debug )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 !
 if (.not. l_solve_linear_problem) then
   !
   call SERIAL_inversion(en%nbf*n_tot_fl_modes,FL_Duffin_BIG_MATRIX_plus,dummy_det,.false.)
   call SERIAL_inversion(en%nbf*n_tot_fl_modes,FL_Duffin_BIG_MATRIX_minus,dummy_det,.false.)
   !
   !
   !aim137 debug (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
   if (l_debug_printouts) then
     open(111,file='BigGm1_matrix_real.dat')
       do i1_fks=1,en%nbf*n_tot_fl_modes
         write(111,'(100(1f7.4))') real(FL_Duffin_BIG_MATRIX_plus(i1_fks,:))
       enddo
     close(111)
     open(111,file='BigGm1_matrix_complex.dat')
       do i1_fks=1,en%nbf*n_tot_fl_modes
         write(111,'(100(1f7.4))') aimag(FL_Duffin_BIG_MATRIX_plus(i1_fks,:))
       enddo
     close(111)
   endif
   !aim137 debug )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   !
 end if
 ! 
 ! 
 ! Solving the system column by column
 ! =================================== 
 ! 
 do i_m=1,en%nbf ! solving column m of F <=> row m of H
   !
   ! Build Duffin BIG VECTORS
   ! ========================
   !
   do i1=1,en%nbf
     do i1_fl=1,n_tot_fl_modes
     !
     i1_extra = i1_fl + max_extra_mode - max_fl_mode
     i1_extra_inverted = n_tot_extra_modes + 1 - i1_extra
     !
     i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
     !
     FL_Duffin_BIG_VECTOR_plus(i1_fks)  = conjg(FL_S_plus(i1,i_m,i1_extra,id_red+(istep-1)*3,ikbz,i_sp))
     FL_Duffin_BIG_VECTOR_minus(i1_fks) = conjg(FL_S_minus(i1,i_m,i1_extra,id_red+(istep-1)*3,ikbz,i_sp))
     !aim137 this is just conjg, not transpose.
     !
     enddo
   enddo
   ! 
   ! Solve linear problem
   ! ====================
   !
   if (l_solve_linear_problem) then
     !aim137:  this needs fixing, I don't get to the same as with inversion.
     !aim137:  update, inversion was wrong. But in any case, do a final check when everything else works.
     call SERIAL_lin_system(en%nbf*n_tot_fl_modes,1,FL_Duffin_BIG_MATRIX_plus,FL_Duffin_BIG_VECTOR_plus)
     call SERIAL_lin_system(en%nbf*n_tot_fl_modes,1,FL_Duffin_BIG_MATRIX_minus,FL_Duffin_BIG_VECTOR_minus)
     !aim137: I understand A x = b returns b in the variable where x was passed on
     !
   else
     !
     !
     !aim137 debug (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
     if (l_debug_printouts) then
       open(111,file='F_vector.dat')
         do i1_fks=1,en%nbf*n_tot_fl_modes
           write(111,*) FL_Duffin_BIG_VECTOR_plus(i1_fks)
         enddo
       close(111)
     endif
     !aim137 debug )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     !
     call M_by_V_omp('N',en%nbf*n_tot_fl_modes,FL_Duffin_BIG_MATRIX_plus,FL_Duffin_BIG_VECTOR_plus,vtmp)
     FL_Duffin_BIG_VECTOR_plus = vtmp
     call M_by_V_omp('N',en%nbf*n_tot_fl_modes,FL_Duffin_BIG_MATRIX_minus,FL_Duffin_BIG_VECTOR_minus,vtmp)
     FL_Duffin_BIG_VECTOR_minus = vtmp
     !
     !aim137 debug (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
     if (l_debug_printouts) then
       open(111,file='H_vector.dat')
         do i1_fks=1,en%nbf*n_tot_fl_modes
           write(111,*) FL_Duffin_BIG_VECTOR_plus(i1_fks)
         enddo
       close(111)
     endif
     !aim137 debug )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     !
     !
   end if
   !
   ! Scatter to Inverse matrix
   ! ========================
   !
   do i1=1,en%nbf
     do i1_fl=1,n_tot_fl_modes
     !
     i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
     !
     FL_Sm1_Duffin_plus(i_m,i1,i1_fl) = FL_Duffin_BIG_VECTOR_plus(i1_fks)
     FL_Sm1_Duffin_minus(i_m,i1,i1_fl) = FL_Duffin_BIG_VECTOR_minus(i1_fks)
     !
     enddo
   enddo
   !
 enddo ! loop over i_m
 !
 !aim137 debug (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
 if (l_debug_printouts) then
   open(111,file='FL_Sm1.dat')
     do i1_fl=1,n_tot_fl_modes
       write(111,*) 'FL mode =',i1_fl
       do i1=1,en%nbf
       write(111,*) FL_Sm1_Duffin_plus(i1,:,i1_fl)
       enddo
     enddo
   close(111)
 endif
 !aim137 debug )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 ! 
 i_start = 1+max_extra_mode-max_fl_mode
 i_finish = 1+max_extra_mode+max_fl_mode
 call FL_check_Duffin_inverse(&
         &     FL_S_plus(:,:,i_start:i_finish,id_red+(istep-1)*3,ikbz,i_sp),&
         &     FL_Sm1_Duffin_plus,en%nbf,diagonal_elements,sum_minus_trace,100,l_debug_printouts)
 !
!print*, ''
 write(*,'(I2,I2,I2,100(1f7.2))') ikbz, id_red, istep, (real(diagonal_elements(i1)), i1=1,en%nbf)
 !
 if (ikbz == 3) then
         if (id_red == 2) then
                 if (istep == 2) then
                         call error('        paro aca')
                 endif
         endif
 endif
 !
end subroutine FL_Duffin
!
!
subroutine FL_check_Duffin_inverse(FL_matrix,FL_inverse,dim_of_matrix,diagonal_elements,&
                &sum_minus_trace,time_samples,l_debug_printouts)
 !
 use pars,           ONLY:SP,cZERO,cONE,cI,pi
 use units,          ONLY:HA2FSm1
 use fields,         ONLY:Efield
 use fl_optics,      ONLY:n_tot_fl_modes,max_fl_mode
 use wrapper_omp,    ONLY:M_by_M_omp
 !
 implicit none
 !
 complex(SP), intent(in) :: FL_matrix(dim_of_matrix,dim_of_matrix,n_tot_fl_modes)
 complex(SP), intent(in) :: FL_inverse(dim_of_matrix,dim_of_matrix,n_tot_fl_modes)
 integer,     intent(in) :: dim_of_matrix
 complex(SP), intent(out) :: diagonal_elements(dim_of_matrix), sum_minus_trace 
 logical,     intent(in) :: l_debug_printouts
 !
 ! Work Space
 complex(SP)  :: matrix_at_t(dim_of_matrix,dim_of_matrix,time_samples)
 complex(SP)  :: inverse_at_t(dim_of_matrix,dim_of_matrix,time_samples)
 complex(SP)  :: identity_at_t(dim_of_matrix,dim_of_matrix,time_samples)
 complex(SP)  :: trace
 integer      :: i1,i2,i1_fl,i_t,i1_shifted_fl,time_samples
 real(SP)     :: time,w0_sm1
 !
 matrix_at_t = cZERO
 inverse_at_t = cZERO
 !
 do i_t=1,time_samples
   !
   w0_sm1 = Efield(1)%frequency(1)*HA2FSm1
   time = 1/(w0_sm1) * i_t/time_samples
   !
   do i1_fl=1,n_tot_fl_modes
     !
     i1_shifted_fl = i1_fl -(max_fl_mode+1)
     !
     matrix_at_t(:,:,i_t)  = matrix_at_t(:,:,i_t)  + exp(-1*cI*i1_shifted_fl*2*pi*i_t/time_samples)*FL_matrix(:,:,i1_fl)
     inverse_at_t(:,:,i_t) = inverse_at_t(:,:,i_t) + exp(-1*cI*i1_shifted_fl*2*pi*i_t/time_samples)*FL_inverse(:,:,i1_fl)
     !
   enddo
    
   call M_by_M_omp('N','N',dim_of_matrix,dim_of_matrix,dim_of_matrix,cONE, &
                        &  matrix_at_t(:,:,i_t),dim_of_matrix, &
                        &  inverse_at_t(:,:,i_t),dim_of_matrix,cZERO, &
                        &  identity_at_t(:,:,i_t),dim_of_matrix)
   !
 enddo
 !
 trace = cZERO
 do i1=1,dim_of_matrix 
    trace=trace+identity_at_t(i1,i1,time_samples)
    diagonal_elements(i1) = identity_at_t(i1,i1,time_samples)
 enddo
  sum_minus_trace = sum(identity_at_t(:,:,time_samples)) - trace
 !
 !
 !aim137 debug (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
 if (l_debug_printouts) then
   open(111,file='S_at_t_real.dat')
   do i_t=1,time_samples
     time = 1/(w0_sm1) * i_t/time_samples
     write(111,*) time, (real(matrix_at_t(i2,:,i_t)),i2=1,dim_of_matrix)
   enddo
   close(111)
   !
   open(111,file='S_at_t_complex.dat')
   do i_t=1,time_samples
     time = 1/(w0_sm1) * i_t/time_samples
     write(111,*) time, (aimag(matrix_at_t(i2,:,i_t)),i2=1,dim_of_matrix)
   enddo
   close(111)
   !
   open(111,file='Sm1_at_t_real.dat')
   do i_t=1,time_samples
     time = 1/(w0_sm1) * i_t/time_samples
     write(111,*) time, (real(inverse_at_t(i2,:,i_t)),i2=1,dim_of_matrix)
   enddo
   close(111)
   !
   open(111,file='Sm1_at_t_complex.dat')
   do i_t=1,time_samples
     time = 1/(w0_sm1) * i_t/time_samples
     write(111,*) time, (aimag(inverse_at_t(i2,:,i_t)),i2=1,dim_of_matrix)
   enddo
   close(111)
   !
   open(111,file='I_at_t_real.dat')
   do i_t=1,time_samples
     time = 1/(w0_sm1) * i_t/time_samples
     write(111,*) time, (real(identity_at_t(i2,:,i_t)),i2=1,dim_of_matrix)
   enddo
   close(111)
   !
   open(111,file='I_at_t_complex.dat')
   do i_t=1,time_samples
     time = 1/(w0_sm1) * i_t/time_samples
     write(111,*) time, (aimag(identity_at_t(i2,:,i_t)),i2=1,dim_of_matrix)
   enddo
   close(111)
 endif
 !aim137 debug )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 ! 
end subroutine FL_check_Duffin_inverse

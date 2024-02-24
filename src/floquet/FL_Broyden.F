!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): IA DS MG
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
subroutine FL_Broyden(n_out,n_in,broyden_alpha,call_num)
 !
 ! Broyden's second method for the mixing of the charge density
 ! Johnson, PRB 38, 12807 (1988)
 ! IA: This subroutine is untested!
 !
 use pars,           ONLY: SP,rZERO,cZERO
 use fft_m,          ONLY: fft_size
 use wrapper,        ONLY: V_dot_V
 use interfaces,     ONLY: LINEAR_ALGEBRA_driver
 use linear_algebra, ONLY: INV
 use fl_optics,      ONLY: FL_max_iterations,Broyden_prev_n_out,Broyden_a,&
&                         Broyden_prev_n_in,Broyden_delta_F,Broyden_u
 !
 implicit none
 !
 real(SP),intent(inout) :: n_out(fft_size)
 real(SP),intent(in)    :: n_in(fft_size)
 real(SP),intent(in)    :: broyden_alpha
 integer,intent(in)     :: call_num
 !
 ! Work Space
 !
 integer   :: i,j
 real(SP)  :: broyden_F(fft_size),broyden_F_prev(fft_size)
 real(SP)  :: broyden_delta_n(fft_size)
 real(SP)  :: broyden_beta(call_num-1,call_num-1)
 real(SP)  :: broyden_c(call_num-1)
 real(SP)  :: broyden_w(call_num-1),broyden_w0
 real(SP)  :: broyden_normalisation
 real(SP)  :: broyden_gamma(call_num-1)
 complex(SP) :: tmp_mat(call_num-1,call_num-1)
 !
 if (call_num == 1) then
   !
   if (.not. allocated(Broyden_prev_n_in))  allocate(Broyden_prev_n_in(fft_size))
   if (.not. allocated(Broyden_prev_n_out)) allocate(Broyden_prev_n_out(fft_size))
   if (.not. allocated(Broyden_delta_F)) allocate(Broyden_delta_F(fft_size,FL_max_iterations-1))
   if (.not. allocated(Broyden_u)) allocate(Broyden_u(fft_size,FL_max_iterations-1))
   if (.not. allocated(Broyden_a)) allocate(Broyden_a(FL_max_iterations-1,FL_max_iterations-1))
   !
   Broyden_delta_F = rZERO
   Broyden_u       = rZERO
   Broyden_a       = rZERO
   !
   Broyden_prev_n_in  = n_in
   Broyden_prev_n_out = n_out
   !
   ! Broyden in first call is just linear mixing
   n_out = n_out * broyden_alpha + n_in * (1._SP - broyden_alpha)
   !
   return
   !
 endif
 !
 !
 ! 1. I calculate deltas, u, c, gamma and beta
 !
 broyden_w0 = 0.01_SP
 broyden_w = 1._SP
 !
 broyden_F = n_out - n_in
 broyden_F_prev = Broyden_prev_n_out - Broyden_prev_n_in
 !
 Broyden_delta_F(:,call_num-1) = broyden_F - broyden_F_prev
 !
 broyden_normalisation = sqrt(V_dot_V(fft_size,&
                       & Broyden_delta_F(:,call_num-1),&
                       & Broyden_delta_F(:,call_num-1) ))
 !
 Broyden_delta_F(:,call_num-1) =Broyden_delta_F(:,call_num-1) / broyden_normalisation
 !
 broyden_delta_n = ( n_in - Broyden_prev_n_in ) / broyden_normalisation
 !
 Broyden_u(:,call_num-1) = broyden_delta_n + broyden_alpha * Broyden_delta_F(:,call_num-1)
 !
 !
 do i=1,call_num-1
   !
   broyden_c(i) = broyden_w(i) * V_dot_V(fft_size,Broyden_delta_F(:,i),broyden_F)
   !
 enddo
 !
 Broyden_a(call_num-1,call_num-1) = broyden_w(call_num-1)**2
 !
 do i=1,call_num-2
   !
   Broyden_a(i,call_num-1) = V_dot_V(fft_size,Broyden_delta_F(:,i),Broyden_delta_F(:,call_num-1))
   !
   Broyden_a(call_num-1,i) = Broyden_a(i,call_num-1)
   !
 enddo
 !
 do i=1,call_num-1
   do j=1,call_num-1
   !
   broyden_beta(i,j) = Broyden_a(i,j)
   !
   enddo
   !
   broyden_beta(i,i) = broyden_beta(i,i) + broyden_w0**2
   !
 enddo
 !
 tmp_mat = cZERO
 tmp_mat = tmp_mat + broyden_beta
 call LINEAR_ALGEBRA_driver(INV,M=tmp_mat)
 broyden_beta = real(tmp_mat)
 !
 do i=1,call_num-1
   !
   broyden_gamma(i) = V_dot_V(call_num-1,broyden_c,broyden_beta(:,i))
   !
 enddo
 !
 !
 ! 2. After calculating point 1, I save for next it:
 !
 Broyden_prev_n_in  = n_in
 Broyden_prev_n_out = n_out
 !
 ! 3. Now I do the final mixing to obtain a new n_out
 !
 n_out = n_out * broyden_alpha + n_in * (1._SP - broyden_alpha)
 !
 ! and also a loop adding the gamma part:
 !
 do i=1,call_num-1
   !
   n_out = n_out - broyden_w(i) * broyden_gamma(i) * Broyden_u(:,i)
   !
 enddo
 !
end subroutine FL_Broyden

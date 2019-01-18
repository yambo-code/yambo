!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG
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
subroutine eval_GradOperator()
 !
 ! On the fft grid the gradient of a periodic function f(r) acts as
 !
 ! \nabla_r n(r_m) = \sum_{n} C_{m,n}  n(r_n)
 !
 ! where C_{m,n} is a 3-vector found as
 !
 ! 1/fft_size \sum_G iG \exp(iG(r_m-r_n))
 !
 ! [see R. Martin p.157-159]
 !
 ! This routine finds C(m,n)
 !
 use pars,      ONLY:DP,cI
 use FFT_m,     ONLY:fft_size,fft_dim,fftw_plan,fft_g_table
 use D_lattice, ONLY:R_m_R,Grad_R
 use R_lattice, ONLY:ng_vec,g_vec
#include<memory.h>
 !
 ! Work space
 !
 complex(DP)         :: v(fft_size)
 integer             :: ii,i1,i2
 !
 ! Allocation
 !
 if (.not.allocated(Grad_R)) then 
   YAMBO_ALLOC(Grad_R,(fft_size,fft_size,3))
 end if
 Grad_R(:,:,:) = (0._SP,0._SP)
 !
 ! FT of i.G in each direction
 !
 do ii = 1,3
   v(fft_g_table(1:ng_vec,1)) = cI*g_vec(1:ng_vec,ii)
#if defined _FFTW
   fftw_plan = 0
   call fft_3d(v,fft_dim,1,fftw_plan)
#else
   call fft_3d(v,fft_dim,1)
#endif
   forall(i1=1:fft_size,i2=1:fft_size) Grad_R(i1,i2,ii) = v(R_m_R(i1,i2))/fft_size
   v = (0._DP,0._DP)
 end do
 !
end subroutine eval_GradOperator

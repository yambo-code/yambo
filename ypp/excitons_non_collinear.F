!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine excitons_non_collinear(Xk,lambda_s,n_lambda,S_sq,S_z)
 !
 use pars,          ONLY:SP
 use stderr,        ONLY:intc
 use BS,            ONLY:BS_mat,BS_eh_table
 use com,           ONLY:msg,error,warning
 use FFT_m,         ONLY:fft_size
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:n_spinor
 use YPP,           ONLY:lambda,BS_H_dim
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg,error,warning
 use wrapper,       ONLY:Vstar_dot_V
 !
 implicit none
 type(bz_samp)          ::Xk
 !
 real(SP)               :: S_sq(BS_H_dim),S_z(BS_H_dim)
 ! 
 ! Work Space
 !
 integer                ::iv1,ic1,ikbz1,ikibz1,is1,neh1
 integer                ::iv2,ic2,ikbz2,ikibz2,is2,neh2
 complex(SP)            ::cc(n_spinor),vv(n_spinor)
 integer                ::i_lambda,n_lambda,lambda_s(n_lambda)
 !
 complex(SP) :: WF_symm1(fft_size,n_spinor)
 complex(SP) :: WF_symm2(fft_size,n_spinor)
 !
 S_sq(:)=0.
 S_z(:)=0.
 !
 !
 call live_timing('Exc_spin@'//trim(intc(lambda)),BS_H_dim) 
 !
 do neh1 = 1,BS_H_dim
   !
   ikbz1  = BS_eh_table(neh1,1)
   iv1    = BS_eh_table(neh1,2)
   ic1    = BS_eh_table(neh1,3)
   ikibz1 = Xk%sstar(ikbz1,1)
   is1    = Xk%sstar(ikbz1,2)
   !
   do neh2 = 1,BS_H_dim
     !
     ikbz2  = BS_eh_table(neh2,1)
     iv2    = BS_eh_table(neh2,2)
     ic2    = BS_eh_table(neh2,3)
     ikibz2 = Xk%sstar(ikbz2,1)
     is2    = Xk%sstar(ikbz2,2)
     ! 
     do i_lambda=1,n_lambda
       !
       lambda=lambda_s(i_lambda)
       !
       cc=(0.,0.)
       if ((iv1.eq.iv2).and.(ikbz1.eq.ikbz2)) then 
         !
         call WF_apply_symm((/ic1,ikibz1,is1,1/),WF_symm1)
         call WF_apply_symm((/ic2,ikibz2,is2,1/),WF_symm2)
         !
         cc(1)=Vstar_dot_V(fft_size,WF_symm2(:,1),WF_symm1(:,1))
         cc(2)=Vstar_dot_V(fft_size,WF_symm2(:,2),WF_symm1(:,2))
         !
       endif
        !
        !
        vv=(0.,0.)
        if ((ic1.eq.ic2).and.(ikbz1.eq.ikbz2)) then 
          !
          call WF_apply_symm((/iv1,ikibz1,is1,1/),WF_symm1)
          call WF_apply_symm((/iv2,ikibz2,is2,1/),WF_symm2)
          !
          vv(1)=Vstar_dot_V(fft_size,WF_symm1(:,1),WF_symm2(:,1))
          vv(2)=Vstar_dot_V(fft_size,WF_symm1(:,2),WF_symm2(:,2))
          !
        endif
        !
        S_z(lambda)=S_z(lambda)+real(BS_mat(neh1,lambda)*conjg(BS_mat(neh2,lambda))*((cc(1)-cc(2))-(vv(1)-vv(2))))
        !
      enddo  !  Lambda states            
      !
    enddo    !  Matrix elements   
    !
    call live_timing(steps=1)
    !
 enddo       ! Matrix elements
 !
 call live_timing()
 S_z=S_z/2.
 !
end subroutine

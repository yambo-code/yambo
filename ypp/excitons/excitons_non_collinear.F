!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine excitons_non_collinear(Xk,lambda_s,n_lambda,S_z,S_sq)
 !
 use pars,          ONLY:SP,cZERO
 use stderr,        ONLY:intc
 use BS,            ONLY:BS_H_dim,BS_bands
 use BS_solvers,    ONLY:BS_mat,BSS_eh_table
 use com,           ONLY:msg,error,warning
 use FFT_m,         ONLY:fft_size
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:n_spinor
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg,error,warning
 use interfaces,    ONLY:WF_load,WF_free
 use wave_func,     ONLY:WF,wf_ng
 use wrapper,       ONLY:Vstar_dot_V
 !
#include<memory.h>
 !
 type(bz_samp)          :: Xk
 integer                :: n_lambda,lambda_s(n_lambda)
 real(SP)               :: S_sq(BS_H_dim),S_z(BS_H_dim)
 ! 
 ! Work Space
 !
 integer                ::iv1,ic1,ikbz1,ikibz1,is1,neh1
 integer                ::iv2,ic2,ikbz2,ikibz2,is2,neh2
 complex(SP)            ::cc(n_spinor),vv(n_spinor)
 integer                ::i_lambda,j_lambda
 !
 complex(SP),allocatable :: WF_symm1(:,:),WF_symm2(:,:)
 !
 S_sq(:)=0._SP
 S_z(:)=0._SP
 !
 call WF_load(WF,wf_ng,1,BS_bands,(/1,Xk%nibz/),space='R',title='-EXCWF')
 !
 YAMBO_ALLOC(WF_symm1,(fft_size,n_spinor))
 YAMBO_ALLOC(WF_symm2,(fft_size,n_spinor))
 !
 do i_lambda=1,n_lambda
   !
   j_lambda=lambda_s(i_lambda)
   !
   call live_timing('Exc_spin@'//trim(intc(j_lambda)),BS_H_dim) 
   !
   do neh1 = 1,BS_H_dim
     !
     ikbz1  = BSS_eh_table(neh1,1)
     iv1    = BSS_eh_table(neh1,2)
     ic1    = BSS_eh_table(neh1,3)
     ikibz1 = Xk%sstar(ikbz1,1)
     is1    = Xk%sstar(ikbz1,2)
     !
     do neh2 = 1,BS_H_dim
       !
       ikbz2  = BSS_eh_table(neh2,1)
       iv2    = BSS_eh_table(neh2,2)
       ic2    = BSS_eh_table(neh2,3)
       ikibz2 = Xk%sstar(ikbz2,1)
       is2    = Xk%sstar(ikbz2,2)
       !
       cc=cZERO
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
       vv=cZERO
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
       S_z(j_lambda)=S_z(j_lambda)+real(BS_mat(neh1,j_lambda)*conjg(BS_mat(neh2,j_lambda))*((cc(1)-cc(2))-(vv(1)-vv(2))))
       !
     enddo  ! Matrix elements
     !
     call live_timing(steps=1)
     !
   enddo    !  Matrix elements
   !
   call live_timing()
   !
 enddo      !  Lambda states
 !
 S_z=S_z/2._SP
 !
 YAMBO_FREE(WF_symm1)
 YAMBO_FREE(WF_symm2)
 !
end subroutine

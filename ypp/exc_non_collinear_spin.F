!
! Copyright (C) 2000-2008 F. De Fausti, A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine exc_non_collinear_spin(Xk,lambda_s,n_lambda,S_sq,S_z)
 !
 use pars,          ONLY:SP
 use stderr,        ONLY:intc
 use BS,            ONLY:BS_mat,BS_eh_table
 use com,           ONLY:msg,error,warning
 use FFT_m,         ONLY:fft_size,fft_rot_r
 use R_lattice,     ONLY:bz_samp
 use wave_func,     ONLY:wf_state,wf
 use YPP,           ONLY:lambda,BS_H_dim
 use timing,        ONLY:live_timing
 use com,           ONLY:msg,error,warning
 !
 implicit none
 type(bz_samp)          ::Xk
 !
 real(SP)               :: S_sq(BS_H_dim),S_z(BS_H_dim)
 ! 
 ! Work Space
 !
#if defined _DOUBLE
 complex(SP)  :: zdotc
#else
 complex(SP)  :: cdotc
#endif 
 !
 !
 complex(SP), parameter :: cI=(0.,1.)
 integer                ::iv,ic,ikbz,ikibz,is,neh
 integer                ::iv1,ic1,ikbz1,ikibz1,is1,neh1
 complex(SP)            ::cc(2),vv(2)
 integer                ::i_lambda,n_lambda,lambda_s(n_lambda)
 !
 S_sq(:)=0.
 S_z(:)=0.
 !
 !
 call live_timing('Exc_spin@'//trim(intc(lambda)),BS_H_dim) 
 !
 do neh = 1,BS_H_dim
  !
  ikbz  = BS_eh_table(neh,1)
  iv    = BS_eh_table(neh,2)
  ic    = BS_eh_table(neh,3)
  ikibz = Xk%sstar(ikbz,1)
  is    = Xk%sstar(ikbz,2)
  !
    do neh1 = 1,BS_H_dim
      !
      ikbz1  = BS_eh_table(neh1,1)
      iv1    = BS_eh_table(neh1,2)
      ic1    = BS_eh_table(neh1,3)
      ikibz1 = Xk%sstar(ikbz1,1)
      is1    = Xk%sstar(ikbz1,2)
      ! 
      do i_lambda=1,n_lambda
        !
        lambda=lambda_s(i_lambda)
        !
        cc=(0.,0.)
        if ((iv.eq.iv1).and.(ikbz.eq.ikbz1)) then 
          !
#if defined _DOUBLE
          cc(1)=zdotc(fft_size,wf(fft_rot_r(is1,:),wf_state(ic1,ikibz1,1)),1,wf(fft_rot_r(is,:),wf_state(ic,ikibz,1)),1)
          cc(2)=zdotc(fft_size,wf(fft_rot_r(is1,:),wf_state(ic1,ikibz1,2)),1,wf(fft_rot_r(is,:),wf_state(ic,ikibz,2)),1)
#else
          cc(1)=cdotc(fft_size,wf(fft_rot_r(is1,:),wf_state(ic1,ikibz1,1)),1,wf(fft_rot_r(is,:),wf_state(ic,ikibz,1)),1)
          cc(2)=cdotc(fft_size,wf(fft_rot_r(is1,:),wf_state(ic1,ikibz1,2)),1,wf(fft_rot_r(is,:),wf_state(ic,ikibz,2)),1)
#endif
          !
        endif
        !
        !
        vv=(0.,0.)
        if ((ic.eq.ic1).and.(ikbz.eq.ikbz1)) then 
          !
#if defined _DOUBLE
          vv(1)=zdotc(fft_size,wf(fft_rot_r(is,:),wf_state(iv,ikibz,1)),1,wf(fft_rot_r(is1,:),wf_state(iv1,ikibz1,1)),1)
          vv(2)=zdotc(fft_size,wf(fft_rot_r(is,:),wf_state(iv,ikibz,2)),1,wf(fft_rot_r(is1,:),wf_state(iv1,ikibz1,2)),1)
#else
          vv(1)=cdotc(fft_size,wf(fft_rot_r(is,:),wf_state(iv,ikibz,1)),1,wf(fft_rot_r(is1,:),wf_state(iv1,ikibz1,1)),1)
          vv(2)=cdotc(fft_size,wf(fft_rot_r(is,:),wf_state(iv,ikibz,2)),1,wf(fft_rot_r(is1,:),wf_state(iv1,ikibz1,2)),1)
#endif
          !
        endif
        !
        S_z(lambda)=S_z(lambda)+real(BS_mat(neh,lambda)*conjg(BS_mat(neh1,lambda))*((cc(1)-cc(2))-(vv(1)-vv(2))))
        !
      enddo  !  Lambda states            
      !
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

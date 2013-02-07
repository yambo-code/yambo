!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
function tddft_alda_r_space(iq,ic,ik,iv,is,mode)
 !
 ! Calculates the F_xc scattering 
 !
 ! mode 1
 !
 !  (ic(1),ik(1),is(1)) --<--:...:--<-- (ic(2),ik(2),is(2))
 !                              :Fxc:
 !  (iv(1),ik(3),is(3)) -->--:...:-->-- (iv(2),ik(4),is(4))
 !
 ! mode 2
 !
 !  (ic(1),ik(1),is(1)) --<--:...:--<-- (iv(2),ik(2),is(2))
 !                              :Fxc:
 !  (iv(1),ik(3),is(3)) -->--:...:-->-- (ic(2),ik(4),is(4))
 !
 ! iq==1 => ik(1)=ik(3) is(1)=is(3) ik(2)=ik(4) is(2)=is(4)
 !
 use pars,           ONLY:SP,pi
 use com,            ONLY:error
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:wf,wf_state
 use xc_functionals, ONLY:F_xc
 use D_lattice,      ONLY:nsym,i_time_rev
 use wrapper,        ONLY:V_dot_V
 use electrons,      ONLY:n_spinor,n_spin
 !
 implicit none
 !
 integer     :: iq,ic(2),iv(2),ik(4),is(4),mode
 complex(SP) :: tddft_alda_r_space
 ! 
 ! Work Space
 !
 integer     :: i_wf_v(n_spinor),i_wf_c(n_spinor)
 complex(SP) :: rhotwr1(fft_size)
 complex(SP) :: rhotwr2(fft_size)
 complex(SP) :: WF_symm1(fft_size,n_spinor)
 complex(SP) :: WF_symm2(fft_size,n_spinor)
 !
 if(n_spin>1) call error('TDDFT/ALDA not implemented for n_spin>1')
 !
 ! Simplified procedure when at the Gamma point.
 !
 !  (ic(1),ik(1),is(1)) --<--:...:--<-- (ic(2),ik(2),is(2))
 !                           :Fxc:
 !  (iv(1),ik(1),is(1)) -->--:...:-->-- (iv(2),ik(2),is(2))
 !
 tddft_alda_r_space=cmplx(0.,0.,SP)
 !
 i_wf_c(1)=wf_state(ic(1),ik(1),1)
 call WF_apply_symm(is(1),i_wf_c,WF_symm1) 
 !
 i_wf_v(1)=wf_state(iv(1),ik(3),1)
 call WF_apply_symm(is(3),i_wf_v,WF_symm2)
 !
 rhotwr1(:)=conjg(WF_symm1(:,1))*WF_symm2(:,1)
 !
 i_wf_c(1)=wf_state(ic(2),ik(2),1)
 call WF_apply_symm(is(1),i_wf_c,WF_symm1) 
 !
 i_wf_v(1)=wf_state(iv(2),ik(4),1)
 call WF_apply_symm(is(4),i_wf_v,WF_symm2)
 !
 if (mode==1) rhotwr2(:)=conjg(WF_symm1(:,1))*WF_symm2(:,1)
 if (mode==2) rhotwr2(:)=conjg(WF_symm2(:,1))*WF_symm1(:,1)
 !
 ! Sum
 !
 tddft_alda_r_space=V_dot_V(fft_size,rhotwr1,rhotwr2)
 !
 ! tddft_alda_r_space'd be 2*cdotc/u*real(fft_size,SP)/DL_vol/Nq 
 ! but it is multiplied by Co
 !
 !  Co=8._SP*pi/DL_vol/real(q%nbz,SP)
 !
 ! in K
 !
 tddft_alda_r_space=tddft_alda_r_space*real(fft_size,SP)/4._SP/pi
 !
end function

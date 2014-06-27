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
function TDDFT_ALDA_R_space(ic,ik,iv,is,mode)
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
 use pars,           ONLY:SP,pi
 use FFT_m,          ONLY:fft_size
 use xc_functionals, ONLY:F_xc
 use D_lattice,      ONLY:nsym,i_time_rev
 use wrapper,        ONLY:V_dot_V
#ifdef _OPENMP
 use wrapper,        ONLY:V_dot_V_omp
#endif
 use electrons,      ONLY:n_spinor
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 integer     :: ic(2),iv(2),ik(4),is(4)
 character(3):: mode
 complex(SP) :: TDDFT_ALDA_R_space
 ! 
 ! Work Space
 !
 complex(SP) :: rhotwr1(fft_size)
 complex(SP) :: rhotwr2(fft_size)
 complex(SP) :: WF_symm1(fft_size,n_spinor)
 complex(SP) :: WF_symm2(fft_size,n_spinor)
 !
#if defined _TIMING
 call timing('T_space ALDA scatt.',OPR='start')
#endif
 !
 ! Simplified procedure when at the Gamma point.
 !
 TDDFT_ALDA_R_space=cmplx(0.,0.,SP)
 !
 call WF_apply_symm((/ic(1),ik(1),is(1),1/),WF_symm1) 
 call WF_apply_symm((/iv(1),ik(3),is(3),1/),WF_symm2)
 !
 rhotwr1(:)=conjg(WF_symm1(:,1))*WF_symm2(:,1)
 !
 call WF_apply_symm((/ic(2),ik(2),is(2),1/),WF_symm1) 
 call WF_apply_symm((/iv(2),ik(4),is(4),1/),WF_symm2)
 !
 if (mode=="RES") rhotwr2(:)=F_xc(:)*WF_symm1(:,1)*conjg(WF_symm2(:,1))
 if (mode=="CPL") rhotwr2(:)=F_xc(:)*WF_symm2(:,1)*conjg(WF_symm1(:,1))
 !
 ! SUM
 !=====
#ifdef _OPENMP
 TDDFT_ALDA_R_space=V_dot_V_omp(fft_size,rhotwr1,rhotwr2)
#else
 TDDFT_ALDA_R_space=V_dot_V(fft_size,rhotwr1,rhotwr2)
#endif
 !
 ! tddft_alda_r_space'd be 2*cdotc/u*real(fft_size,SP)/DL_vol/Nq 
 ! but it is multiplied by Co
 !
 !  Co=8._SP*pi/DL_vol/real(q%nbz,SP)
 !
 ! in K
 !
 TDDFT_ALDA_R_space=TDDFT_ALDA_R_space*real(fft_size,SP)/4._SP/pi
 !
#if defined _TIMING
 call timing('T_space ALDA scatt.',OPR='stop')
#endif
 !
end function

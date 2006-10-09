!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 function tddft_alda_r_space(iq,ic,ik,iv,is_in,mode)
!===================================================
!
! Calculates the F_xc scattering 
!
! mode 1
!
!  (ic(1),ik(1),is_in(1)) --<--:...:--<-- (ic(2),ik(2),is_in(2))
!                              :Fxc:
!  (iv(1),ik(3),is_in(3)) -->--:...:-->-- (iv(2),ik(4),is_in(4))
!
! mode 2
!
!  (ic(1),ik(1),is_in(1)) --<--:...:--<-- (iv(2),ik(2),is_in(2))
!                              :Fxc:
!  (iv(1),ik(3),is_in(3)) -->--:...:-->-- (ic(2),ik(4),is_in(4))
!
! iq==1 => ik(1)=ik(3) is_in(1)=is_in(3) ik(2)=ik(4) is_in(2)=is_in(4)
!
 use pars,           ONLY:SP,pi
 use FFT_m,          ONLY:fft_size,fft_rot_r
 use wave_func,      ONLY:wf,wf_state
 use xc_functionals, ONLY:F_xc
 use D_lattice,      ONLY:nsym,i_time_rev,DL_vol
 use R_lattice,      ONLY:nqbz
 implicit none
 integer     :: iq,ic(2),iv(2),ik(4),is_in(4),mode
 complex(SP) :: tddft_alda_r_space
!
!ws
 integer     :: i1,is(4),iv1fft,ic1fft,i2fft,j2fft
 logical     :: l_s_tr(4)
 complex(SP) :: rhotwr1(fft_size),rhotwr2(fft_size)
#if defined DP
 complex(DP):: zdotc,zdotu
#else
 complex(SP):: cdotc,cdotu
#endif
!
! Init
!------
 tddft_alda_r_space=cmplx(0.,0.,SP)
!
! Symmetries
!------------
 if (iq==1) then
   do i1=1,2
     is(i1)=is_in(i1)
     if (is_in(i1)>nsym/(i_time_rev+1)) is(i1)=is_in(i1)-nsym/(i_time_rev+1)
     l_s_tr(i1)=is(i1)/=is_in(i1)
   enddo
   l_s_tr(3:4)=l_s_tr(1:2)
 endif
!
! FFT indexes
!
 ic1fft=wf_state(ic(1),ik(1),1)
 iv1fft=wf_state(iv(1),ik(3),1)
 if (mode==1) then
   i2fft=wf_state(ic(2),ik(2),1)
   j2fft=wf_state(iv(2),ik(4),1)
 else
   i2fft=wf_state(iv(2),ik(2),1)
   j2fft=wf_state(ic(2),ik(4),1)
 endif
!
! Simplified procedure when at the Gamma point.
!
!  (ic(1),ik(1),is(1)) --<--:...:--<-- (ic(2),ik(2),is(2))
!                           :Fxc:
!  (iv(1),ik(1),is(1)) -->--:...:-->-- (iv(2),ik(2),is(2))
!
 if (iq==1) then
   rhotwr1(:)=conjg(wf(fft_rot_r(is(1),:),ic1fft))*wf(fft_rot_r(is(1),:),iv1fft)

   if (l_s_tr(2)) then
     rhotwr2(:)=F_xc(:)*&
&               conjg( wf(fft_rot_r(is(2),:),i2fft) )*wf(fft_rot_r(is(2),:),j2fft)
   else
     rhotwr2(:)=F_xc(:)*wf(fft_rot_r(is(2),:),i2fft)*conjg(wf(fft_rot_r(is(2),:),j2fft))
   endif
 endif
!
! Sum
!=====
!
 if (l_s_tr(1)) then
#if defined DP
   tddft_alda_r_space=zdotc(fft_size,rhotwr1,1,rhotwr2,1)
#else
   tddft_alda_r_space=cdotc(fft_size,rhotwr1,1,rhotwr2,1)
#endif
 else
#if defined DP
   tddft_alda_r_space=zdotu(fft_size,rhotwr1,1,rhotwr2,1)
#else
   tddft_alda_r_space=cdotu(fft_size,rhotwr1,1,rhotwr2,1)
#endif
 endif
!
! tddft_alda_r_space'd be 2*cdotc/u*real(fft_size,SP)/DL_vol/Nq 
! but it is multiplied by Co
!
!  Co=8._SP*pi/DL_vol/real(q%nbz,SP)
!
! in K
!
 tddft_alda_r_space=tddft_alda_r_space*real(fft_size,SP)/4._SP/pi

 end function
!

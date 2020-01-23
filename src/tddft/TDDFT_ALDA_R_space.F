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
function TDDFT_ALDA_R_space(is,os,isp,osp,mode)
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
 use pars,           ONLY:SP,pi,cZERO
 use FFT_m,          ONLY:fft_size
 use xc_functionals, ONLY:F_xc
 use wrapper_omp,    ONLY:V_dot_V_omp
 use electrons,      ONLY:n_spinor,n_sp_pol,n_spin
 use timing_m,       ONLY:timing
 use interfaces,     ONLY:WF_apply_symm
 !
 implicit none
 !
 integer     :: is(4),os(4),isp(4),osp(4)
 character(3):: mode
 complex(SP) :: TDDFT_ALDA_R_space
 ! 
 ! Work Space
 !
 complex(SP) :: rhotwr1(fft_size,n_spinor)
 complex(SP) :: rhotwr2(fft_size,n_spinor)
 complex(SP) :: WF_symm1(fft_size,n_spinor)
 complex(SP) :: WF_symm2(fft_size,n_spinor)
 !
 call timing('T_space ALDA scatt.',OPR='start')
 !
 if (is(4)/=os(4).or.isp(4)/=osp(4)) call error(" TDDFT_ALDA_R_space: wrong spin index")
 if (mode/="RES" .and. mode/="CPL" ) call error(" TDDFT_ALDA_R_space: unkown mode")
 !
 TDDFT_ALDA_R_space=cZERO
 !
 call WF_apply_symm(is,WF_symm1) 
 call WF_apply_symm(os,WF_symm2)
 !
 rhotwr1=conjg(WF_symm1)*WF_symm2
 !
 if (mode=="RES") then
   call WF_apply_symm(isp,WF_symm1) 
   call WF_apply_symm(osp,WF_symm2)
 else if (mode=="CPL") then
   call WF_apply_symm(isp,WF_symm2)
   call WF_apply_symm(osp,WF_symm1)
 endif
 !
 if( n_spin==1 ) rhotwr2(:,1)=F_xc(:,  1  ,   1  )*WF_symm1(:,1)*conjg(WF_symm2(:,1))
 !
 if(n_sp_pol==2) rhotwr2(:,1)=F_xc(:,is(4),isp(4))*WF_symm1(:,1)*conjg(WF_symm2(:,1))
 !
 if(n_spinor==2) then
   rhotwr2(:,1)=F_xc(:,1,1)*WF_symm1(:,1)*conjg(WF_symm2(:,1))+&
                  F_xc(:,1,2)*WF_symm1(:,2)*conjg(WF_symm2(:,2))
   rhotwr2(:,2)=F_xc(:,2,1)*WF_symm1(:,1)*conjg(WF_symm2(:,1))+&
                  F_xc(:,2,2)*WF_symm1(:,2)*conjg(WF_symm2(:,2))
 endif
 !
 ! SUM
 !=====
 TDDFT_ALDA_R_space=V_dot_V_omp(fft_size*n_spinor,rhotwr1,rhotwr2)
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
 call timing('T_space ALDA scatt.',OPR='stop')
 !
end function

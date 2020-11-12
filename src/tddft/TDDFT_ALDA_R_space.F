!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use BS,             ONLY:l_BS_magnons
 use wrapper_omp,    ONLY:V_dot_V_omp
 use electrons,      ONLY:n_spinor,n_sp_pol,n_spin
 use timing_m,       ONLY:timing
 use interfaces,     ONLY:WF_apply_symm
 !
 implicit none
 !
 complex(SP) :: TDDFT_ALDA_R_space
 !
 integer,      intent(in) :: is(4),os(4),isp(4),osp(4)
 character(3), intent(in) :: mode
 ! 
 ! Work Space
 !
 integer     :: i_sp_fxc(2)
 complex(SP) :: rhotwr1(fft_size,n_spinor)
 complex(SP) :: rhotwr2(fft_size,n_spinor)
 complex(SP) :: WF_symm1(fft_size,n_spinor)
 complex(SP) :: WF_symm2(fft_size,n_spinor)
 !
 call timing('T_space ALDA scatt.',OPR='start')
 !
 if ((is(4)/=os(4).or.isp(4)/=osp(4)).and.((.not.l_BS_magnons).or. n_sp_pol==1)) call error(" TDDFT_ALDA_R_space: wrong spin index")
 if ((is(4)==os(4).or.isp(4)==osp(4)).and.((     l_BS_magnons).and.n_sp_pol==2)) call error(" TDDFT_ALDA_R_space: wrong spin index")
 if (mode/="RES" .and. mode/="CPL" ) call error(" TDDFT_ALDA_R_space: unkown mode")
 !
 TDDFT_ALDA_R_space=cZERO
 !
 i_sp_fxc(1)=is(4)
 call WF_apply_symm(is,WF_symm1) 
 call WF_apply_symm(os,WF_symm2)
 !
 rhotwr1=conjg(WF_symm1)*WF_symm2
 !
 if (mode=="RES") then
   call WF_apply_symm(isp,WF_symm1) 
   call WF_apply_symm(osp,WF_symm2)
   i_sp_fxc(2)=isp(4)
 else if (mode=="CPL") then
   call WF_apply_symm(isp,WF_symm2)
   call WF_apply_symm(osp,WF_symm1)
   i_sp_fxc(2)=osp(4)
 endif
 !
 if( n_spin==1 ) rhotwr2(:,1)=F_xc(:,  1  ,   1  )*WF_symm1(:,1)*conjg(WF_symm2(:,1))
 !
 if(n_sp_pol==2) rhotwr2(:,1)=F_xc(:,i_sp_fxc(1),i_sp_fxc(2))*WF_symm1(:,1)*conjg(WF_symm2(:,1))
 !
 if(n_spinor==2) then
   ! This is only the optical part of the kernel
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
 ! tddft_alda_r_space should be mutiplied by X, it is mutiplied by Co in K
 ! X  = fft_size*spin_occ/DL_vol/Nq 
 ! Co =     4*pi*spin_occ/DL_vol/Nq
 ! --> X/Co = fft_size/4/pi
 !
 TDDFT_ALDA_R_space=TDDFT_ALDA_R_space*real(fft_size,SP)/4._SP/pi
 !
 call timing('T_space ALDA scatt.',OPR='stop')
 !
end function

!
! Copyright (C) 2000-2010 C. Attaccalite, M. Gruning, A. Marini and the YAMBO team 
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
subroutine RT_X_two(en)
 !
 use YPP,            ONLY:RT_P_t,X_rt,Use_FFT,RT_J_t,W_RT_induced_step
 use pars,           ONLY:SP,DP,pi,cI
 use units,          ONLY:HARTREE,SPEED_OF_LIGHT
 use electrons,      ONLY:levels
 use com,            ONLY:error
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:NE_steps,RT_step,RT_ind_J,RT_output,RT_P
 use fields,         ONLY:Efield
 implicit none
 !
 type(levels)  :: en
 !
 ! Work Space
 !
 integer, parameter :: nfreqs=2
 complex(DP), allocatable :: A_vecpot_vel_w(:,:),F_m(:),F_p(:)
 complex(SP), allocatable :: freqs(:)
 complex(SP) :: X_tot,outfreqs(nfreqs)
 complex(DP) :: F_2plus,F_2minus,X_irrep(nfreqs,3)
 integer     :: i_w
 real(SP)    :: W_step, W_reached, delta,W_reached2
 !
 if(trim(Efield(1)%ef_name)/="QSSIN") &
&  call error(" Non linear optics only with QSSIN field! ")
 !
 call section("=",'Second order susceptibility')
 !=========================
 !
 ! Field: Allocation
 !
 delta = 1._SP/Efield(1)%damping
 X_rt%energy_range(1)=-4._SP*(Efield(1)%frequency(1))
 X_rt%energy_range(2)=4._SP*(Efield(1)%frequency(1))
 W_step=W_RT_induced_step
 X_rt%energy_steps= (X_rt%energy_range(2))/W_step
 X_rt%energy_steps= 2*X_rt%energy_steps
! print*, "#", X_rt%energy_range, W_step, X_rt%energy_steps, delta
! stop
 !
 allocate(freqs(X_rt%energy_steps))
 allocate(A_vecpot_vel_w(X_rt%energy_steps,3),F_m(X_rt%energy_steps),F_p(X_rt%energy_steps))
 call mem_est("A_vecpot_vel_w F_m F_p",&
      &(/size(A_vecpot_vel_w),size(F_m),size(F_p)/),(/2*DP,2*DP,2*DP/))
 !
 ! Field: FFT
 !
 A_vecpot_vel_w=(0._DP,0._DP)
 freqs=(0._SP,0._SP)
 do i_w = 1, X_rt%energy_steps
   freqs(i_w)=cmplx(Efield(1)%frequency(1)-((i_w-1)*W_step+X_rt%energy_range(1)),0._SP)
 enddo
 !call Fourier_Transform(freqs,A_vecpot_vel_w,X_rt%energy_steps,A_vecpot_vel_t,NE_steps,RT_step,3)
 A_vecpot_vel_w(:,:)=A_vecpot_vel_w(:,:)*RT_step 
 forall(i_w=1:X_rt%energy_steps) F_m(i_w) = &
      &sum(A_vecpot_vel_w(i_w,:)*Efield(1)%versor(:))*SPEED_OF_LIGHT
 !
 A_vecpot_vel_w=(0._DP,0._DP)
 freqs=(0._SP,0._SP)
 do i_w = 1, X_rt%energy_steps
   freqs(i_w)=cmplx(Efield(1)%frequency(1)+(i_w-1)*W_step+X_rt%energy_range(1),0._SP)
 enddo
 !call Fourier_Transform(freqs,A_vecpot_vel_w,X_rt%energy_steps,A_vecpot_vel_t,NE_steps,RT_step,3)
 A_vecpot_vel_w(:,:)=A_vecpot_vel_w(:,:)*RT_step 
 forall(i_w=1:X_rt%energy_steps) F_p(i_w) = &
      &sum(A_vecpot_vel_w(i_w,:)*Efield(1)%versor(:))*SPEED_OF_LIGHT
 !
 !
 ! Field: Convolution
 !
 F_2plus = (0._DP,0._DP)
 F_2minus = (0._DP,0._DP)
 do i_w = 1, X_rt%energy_steps
   F_2plus = F_2plus + F_p(i_w)*conjg(F_p(i_w))*W_step
   F_2minus = F_2minus + F_m(i_w)*F_p(i_w)*W_step
 end do
! print*, "#", F_2plus, F_2minus
 !
 ! Field: Deallocation
 !
 deallocate(A_vecpot_vel_w,F_p,F_m,freqs)
 call mem_est("A_vecpot_vel_w F_m F_p")
 !
 ! Polarization: FT at 0 and twice the field frequency
 !
 outfreqs(1)=(0._SP,0._SP)                           ! Optical rectification
 outfreqs(2)=cmplx(Efield(1)%frequency(1)*2._SP,0._SP)  ! SHG
 !call Fourier_Transform(outfreqs,X_irrep,nfreqs,RT_P_t,NE_steps,RT_step,3)
 X_irrep(:,:)=X_irrep(:,:)*RT_step
 !
 X_irrep(1,:)=PI*X_irrep(1,:)/F_2plus
 X_irrep(2,:)=2_DP*PI*X_irrep(2,:)/F_2minus
 ! 
 call RT_output("open nonlinear")
 !
 do i_w = 1, nfreqs
   !
   W_reached = real(outfreqs(i_w))
   X_tot=sum(X_irrep(i_w,:)*Efield(1)%versor(:))
   call RT_output('nonlinear',(/aimag(X_tot),real(X_tot)/),E=W_reached*HARTREE)
   !
 enddo
 call RT_output("close nonlinear")
 !
end subroutine RT_X_two

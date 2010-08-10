!
! Copyright (C) 2000-2010 C. Attaccalite, A. Marini and the YAMBO team 
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
! Partialy inspired from Octopus v. 3.1.0
!
! Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
subroutine RT_X_one(en,k)
 !
 use YPP,            ONLY:RT_P_t,X_rt,Use_FFT,RT_J_t,A_vecpot_vel_t,W_RT_induced_step
 use pars,           ONLY:SP,DP,pi,cI
 use units,          ONLY:FS2AUT,HARTREE,SPEED_OF_LIGHT 
 use electrons,      ONLY:levels
 use com,            ONLY:warning
 use stderr,         ONLY:real2ch
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:bz_samp
 use timing,         ONLY:live_timing
 use real_time,      ONLY:NE_steps,RT_step,RT_ind_J,RT_output,RT_P,RT_output
 use fields,         ONLY:Efield,A_vecpot,global_gauge 
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: k
 !
 ! Work Space
 !
 integer  :: i_t,i_w,i_c,i_t_start,i_t_end
 real(SP) :: W_step,Time_reached,W_reached
 complex(SP), allocatable :: EELS(:),EPS(:),freqs(:)
 complex(DP), allocatable :: X_irrep(:,:),A_vecpot_vel_w(:,:)
 !
 ! FFT
 !
 integer     :: fft_sign
 integer(8)  :: fftw_plan
 !
 call section("=",'Optics')
 !=========================
 !
 ! 1D FFT setup
 !--------------
 !
 if(Use_FFT) then
   !
   ! I want to print only the positive frequencies
   ! i_w > energy_steps/2 correspond to the negative ones
   !
   X_rt%energy_steps   =NE_steps/2
   X_rt%energy_range(1)=0.
   X_rt%energy_range(2)=W_RT_induced_step*NE_steps/2.
   !
   allocate(EELS(NE_steps),EPS(NE_steps),X_irrep(NE_steps,3))  
   allocate(A_vecpot_vel_w(NE_steps,3))
   !
 else
   !
   allocate(EELS(X_rt%energy_steps),EPS(X_rt%energy_steps),X_irrep(NE_steps,3)) 
   allocate(A_vecpot_vel_w(X_rt%energy_steps,3))
   !
 endif
 !
 call mem_est("EELS EPS X_irrep",(/size(EELS),size(EPS),size(X_irrep)/),(/2*SP,2*SP,2*DP/))
 call mem_est("A_vecpot_vel_w",(/size(A_vecpot_vel_w)/),(/2*DP/))
 !
 EELS   =(0._DP,0._DP)
 EPS    =(0._DP,0._DP)
 X_irrep=(0._DP,0._DP)
 A_vecpot_vel_w=(0._DP,0._DP)
 !
 ! FFT 
 !=====
 !
 W_step=(X_rt%energy_range(2) - X_rt%energy_range(1))/X_rt%energy_steps
 !
 allocate(freqs(X_rt%energy_steps))
 freqs=(0._SP,0._SP)
 !
 do i_w = 1, X_rt%energy_steps
   freqs(i_w)=cmplx((i_w-1)*W_step+X_rt%energy_range(1),0._SP)
 enddo
 !
 if(Use_FFT) then
   !
   fftw_plan=0
   fft_sign =-1
   !
   do i_c=1,3
     !
     X_irrep(:,i_c)        = RT_P_t(:,i_c)
     A_vecpot_vel_w(:,i_c) = A_vecpot_vel_t(:,i_c)
     !
#if defined _FFTW
    call fft_1d(X_irrep(:,i_c),NE_steps,fft_sign,fftw_plan)
    call fft_1d(A_vecpot_vel_w(:,i_c),NE_steps,fft_sign,fftw_plan)
#else
    call fft_1d(X_irrep(:,i_c),NE_steps,fft_sign)
    call fft_1d(A_vecpot_vel_w(:,i_c),NE_steps,fft_sign)
#endif
     !
   enddo
   !
   X_irrep(:,:)=conjg(X_irrep(:,:))
   A_vecpot_vel_w(:,:)=conjg(A_vecpot_vel_w(:,:))
   !
 else
   ! 
   call Fourier_Transform(freqs,X_irrep,X_rt%energy_steps,RT_P_t,NE_steps,RT_step,3)
   call Fourier_Transform(freqs,A_vecpot_vel_w,X_rt%energy_steps,A_vecpot_vel_t,NE_steps,RT_step,3)
   !
 endif
 !
 ! Due to the definition of FFT 
 !
 X_irrep(:,:)       =X_irrep(:,:)*RT_step
 A_vecpot_vel_w(:,:)=A_vecpot_vel_w(:,:)*RT_step
 !
 select case ( trim(Efield(1)%ef_name))
   !
   case('DELTA')
       X_irrep(:,:)=X_irrep(:,:)/Efield(1)%amplitude
   case default
     do i_w=1,X_rt%energy_steps
       X_irrep(i_w,:)=-X_irrep(i_w,:)/(sum(A_vecpot_vel_w(i_w,:)*Efield(1)%versor(:))/SPEED_OF_LIGHT)
     enddo
   !  
 end select
 !
 ! P(w)=\tilde(X(w))*E_tot(w)=X(w)*E_ext(w) 
 !
 ! => EELS=1+4pi*P(w)/E_ext(w)
 ! => EPS=1./EELS
 !
 call RT_output("open eps eels")
 do i_w = 1, X_rt%energy_steps
   !
   EPS(i_w) =1._DP-4._SP*pi*sum(X_irrep(i_w,:)*Efield(1)%versor(:))
   EELS(i_w)=1._DP/EPS(i_w)
   !
   W_reached = real(freqs(i_w))
   call RT_output('eps',(/ aimag(EPS(i_w)),real(EPS(i_w))/),TIME_or_E=W_reached*HARTREE)
   call RT_output('eel',(/-aimag(EELS(i_w)),real(EELS(i_w))/),TIME_or_E=W_reached*HARTREE)
   !
 enddo
 call RT_output("close eps eel")
 !
 deallocate(EELS,EPS,X_irrep,A_vecpot_vel_w)
 call mem_est("EELS EPS X_irrep A_vecpot_vel_w")
 !
end subroutine

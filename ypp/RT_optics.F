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
subroutine RT_optics(en,k)
 !
 use YPP,            ONLY:RT_P_t,X_rt,Use_FFT,RT_J_t
 use pars,           ONLY:SP,DP,pi,cI
 use units,          ONLY:FS2AUT,HARTREE
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
 real(SP) :: W_RT_induced_step,W_step,Time_reached,W_reached
 real(SP),    allocatable :: damp_function(:)
 complex(SP), allocatable :: current_ext_w(:,:), A_vecpot_vel_w(:,:),EELS(:),EPS(:),E_induced_t(:,:),X_irrep(:,:)
 !
 ! FFT
 !
 integer     :: fft_sign
 integer(8)  :: fftw_plan
 !
 call section("=",'Optics')
 !=========================
 !
 ! Allocations
 !
 allocate(current_ext_w(NE_steps,3))
 call mem_est("Current_ext_w",(/size(current_ext_w)/),(/2*SP/))
 allocate(A_vecpot_vel_w(NE_steps,3))
 call mem_est("A_vecpot_vel_w",(/size(A_vecpot_vel_w)/),(/2*SP/))
 allocate(damp_function(NE_steps))
 call mem_est("Damp_Function",(/NE_steps/),(/SP/))
 allocate(E_induced_t(NE_steps,3))
 call mem_est("E_induced_t",(/size(E_induced_t)/),(/2*SP/))
 current_ext_w =(0._SP,0._SP)
 A_vecpot_vel_w=(0._SP,0._SP)
 !
 ! Reduced Time range
 !====================
 !
 i_t_start=int(X_rt%time_range(1)/RT_step)
 i_t_end  =int(X_rt%time_range(2)/RT_step)
 !
 if(i_t_start<=0.or.i_t_start>NE_steps) i_t_start=1
 if(i_t_end<=0.or.i_t_end>NE_steps)     i_t_end  =NE_steps
 !
 W_RT_induced_step=2.*pi/(real(NE_steps,SP)*RT_step)  ! Frequency step in real-time dynamics
 ! 
 ! DAMPING
 !=========
 !
 ! If there is a time-windows the damping function
 ! has be applied also the initial part otherwise 
 ! there will problems doing FFT for the non-zero starting point 
 !
 if(X_rt%damp_factor<W_RT_induced_step) then
   X_rt%damp_factor=W_RT_induced_step
   call warning('Damping factor redefined to be '//trim(real2ch(W_RT_induced_step*HARTREE))//' eV')
 endif
 !
 damp_function(:) = 1.0
 select case ( trim(X_rt%damp) )
   case("LORENTZIAN")  
     forall(i_t=1:NE_steps) damp_function(i_t) = exp(-abs((i_t-1)*RT_step)*X_rt%damp_factor)
   case("GAUSSIAN") 
     forall(i_t=1:NE_steps) damp_function(i_t) =  exp(-((i_t-1)*RT_step)**2*X_rt%damp_factor**2)
 end select
 ! 
 ! apply the damping
 !===================
 !
 forall(i_t=1:NE_steps) RT_J_t(i_t,:)=RT_J_t(i_t,:)*damp_function(i_t)
 forall(i_t=1:NE_steps) RT_P_t(i_t,:)=RT_P_t(i_t,:)*damp_function(i_t)
 !
 ! Print the dumped current and polarization 
 !-------------------------------------------
 call RT_output("open cur pol")
 do i_t=1,NE_steps
   RT_ind_J=RT_J_t(i_t,:)
   RT_P    =RT_P_t(i_t,:)
   call RT_output('cur pol',TIME_or_E=(i_t-1)*RT_step/FS2AUT)
 enddo
 call RT_output("close cur pol")
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
   !
 else
   !
   allocate(EELS(X_rt%energy_steps),EPS(X_rt%energy_steps),X_irrep(NE_steps,3))  
   !
 endif
 !
 call mem_est("EELS EPS X_irrep",(/size(EELS),size(EPS),size(X_irrep)/),(/2*SP,2*SP,2*SP/))
 !
 EELS   =(0._DP,0._DP)
 EPS    =(0._DP,0._DP)
 X_irrep=(0._DP,0._DP)
 !
 ! FFT 
 !=====
 !
 W_step=(X_rt%energy_range(2) - X_rt%energy_range(1))/X_rt%energy_steps
 !
 if(Use_FFT) then
   !
   fftw_plan=0
   fft_sign =-1
   !
   do i_c=1,3
     !
     X_irrep(:,i_c)  = RT_P_t(:,i_c)
     !
#if defined _FFTW
    call fft_1d(X_irrep(:,i_c),NE_steps,fft_sign,fftw_plan)
#else
    call fft_1d(X_irrep(:,i_c),NE_steps,fft_sign)
#endif
     !
   enddo
   !
   X_irrep(:,:)=conjg(X_irrep(:,:))
   !
 else
   ! 
   call live_timing('FFT:', X_rt%energy_steps)
   !
   do i_w = 1, X_rt%energy_steps
     W_reached = (i_w-1)*W_step+X_rt%energy_range(1)
     !
     do i_t = 1, NE_steps
       Time_reached = i_t*RT_step
       X_irrep(i_w,1:3) = X_irrep(i_w,1:3)+RT_P_t(i_t,1:3)*exp(cI*W_reached*Time_reached)
     enddo
     !
     call live_timing(steps=1)
     !
   enddo
   !
   call live_timing()
   !
 endif
 !
 ! Due to the definition of FFT 
 !
 X_irrep(:,:)=X_irrep(:,:)*RT_step/Efield(1)%amplitude
 !
 ! P(w)=\tilde(X(w))*E_tot(w)=X(w)*E_ext(w) 
 !
 ! => EELS=1+4pi*P(w)/E_ext(w)
 ! => EPS=1./EELS
 !
 call RT_output("open eps eels")
 do i_w = 1, X_rt%energy_steps
   !
   EPS(i_w) =1._DP-4._SP*pi* (X_irrep(i_w,1)*Efield(1)%versor(1)+ &
                              X_irrep(i_w,2)*Efield(1)%versor(2)+X_irrep(i_w,3)*Efield(1)%versor(3))
   EELS(i_w)=1._DP/EPS(i_w)
   !
   W_reached = (i_w-1)*W_step+X_rt%energy_range(1)
   call RT_output('eps',(/ aimag(EPS(i_w)),real(EPS(i_w))/),TIME_or_E=W_reached*HARTREE)
   call RT_output('eel',(/-aimag(EELS(i_w)),real(EELS(i_w))/),TIME_or_E=W_reached*HARTREE)
   !
 enddo
 call RT_output("close eps eel")
 !
 deallocate(EELS,EPS,damp_function,A_vecpot_vel_w,current_ext_w,X_irrep)
 call mem_est("Current_ext_w A_vecpot_vel_w EELS EPS Damp_Function X_irrep")
 !
end subroutine

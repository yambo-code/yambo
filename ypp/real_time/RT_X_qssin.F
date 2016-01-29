!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA MG
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
subroutine RT_X_qssin
 !
 use YPP,            ONLY:RT_P_t,X_rt,Use_FFT,i_t_start,i_t_end,&
&                         W_RT_induced_step,RED_steps,J_steps,J_time_step
 use pars,           ONLY:SP,DP,pi,schlen
 use units,          ONLY:HA2EV,SVCMm12VMm1,AU2VMm1
 use memory_m,       ONLY:mem_est
 use com,            ONLY:error
 use rt_ctl,         ONLY:RT_output,RT_IO_JPO_steps
 use fields,         ONLY:Efield,small_a
 use functions,      ONLY:theta_function
 use LIVE_t,         ONLY:live_timing
 !
 implicit none
 !
 ! Work Space
 !
 integer, parameter :: nfreqs=2
 complex(DP), allocatable :: E_m(:),E_p(:),E_t(:)
 complex(SP), allocatable :: freqs(:)
 complex(SP) :: X_tot,outfreqs(nfreqs),E_of_t(2),A_of_t(2),theta,delta
 complex(DP) :: E_2plus,E_2minus,X_irrep(nfreqs,3)
 integer     :: i_w, i_t, i_c,i_Probe
 real(SP)    :: W_step,Field_time
 character(9) :: FT_title
 character(schlen) :: X_file_name
 !
 call section("=",'Second order susceptibility')
 !
 ! Implementation based on JCP 127, 154114 (2007)
 !
 !=========================
 !
 ! Field: Definitions & Allocation
 i_Probe=1
 !
 X_rt%energy_range(1)=-4._SP*(Efield(i_Probe)%frequency(1)) ! Here I define the integration cutoff
 X_rt%energy_range(2)= 4._SP*(Efield(i_Probe)%frequency(1)) ! in Eqs 20-22 as 4 time the field freq. TEST?!?
 W_step=W_RT_induced_step
 X_rt%energy_steps= (X_rt%energy_range(2))/W_step  ! and I define the number of freq step
 X_rt%energy_steps= 2*X_rt%energy_steps            ! as a consequence
 !
 allocate(E_t(J_steps))
 do i_t=1,J_steps
   Field_time=(i_t-1)*J_time_step-Efield(i_Probe)%t_initial
   A_of_t =small_a(Field_time,Efield(i_Probe),0)
   E_of_t =small_a(Field_time,Efield(i_Probe),1)
   theta  =theta_function(Field_time,J_time_step,0)
   delta  =theta_function(Field_time,J_time_step,1)
   select case(Efield(i_Probe)%ef_pol)
   case("linear")
     E_t(i_t)=Efield(i_Probe)%amplitude*(E_of_t(1)*theta+A_of_t(1)*delta)
   case("circular")
     call error(' Circular polarization not yet implemented')
   end select
 enddo
 allocate(freqs(X_rt%energy_steps))
 allocate(E_m(X_rt%energy_steps),E_p(X_rt%energy_steps))
 call mem_est("E_m E_p",&
      &(/size(E_m),size(E_p)/),(/2*DP,2*DP/))
 !
 ! Field: FFT
 !
 E_m=(0._DP,0._DP) ! E(Wo-W), where Wo is the field frequency   
 freqs=(0._SP,0._SP)
 do i_w = 1, X_rt%energy_steps
   freqs(i_w)=cmplx(Efield(i_Probe)%frequency(1)-((i_w-1)*W_step+X_rt%energy_range(1)),0._SP)
 enddo
 call RT_1D_Fourier_Transform('T2W',' (E_m field)',freqs,E_m(:),X_rt%energy_steps,&
&                             E_t(i_t_start:i_t_end),RED_steps,J_time_step,&
&                             (i_t_start-1)*J_time_step,Use_FFT,1)
 !
 E_p=(0._DP,0._DP) ! E(Wo+W), where Wo is the field frequency   
 freqs=(0._SP,0._SP)
 do i_w = 1, X_rt%energy_steps
   freqs(i_w)=cmplx(Efield(i_Probe)%frequency(1)+(i_w-1)*W_step+X_rt%energy_range(1),0._SP)
 enddo
 call RT_1D_Fourier_Transform('T2W',' (E_p field)',freqs,E_p(:),X_rt%energy_steps,&
&                             E_t(i_t_start:i_t_end),RED_steps,J_time_step,&
&                             (i_t_start-1)*J_time_step,Use_FFT,1)
 !
 ! Field: Convolution
 !
 E_2plus = (0._DP,0._DP)
 E_2minus = (0._DP,0._DP)
 call live_timing('Fields Convolution:',X_rt%energy_steps)
 do i_w = 1, X_rt%energy_steps
   E_2plus = E_2plus + E_p(i_w)*conjg(E_p(i_w))*W_step
   E_2minus = E_2minus + E_m(i_w)*E_p(i_w)*W_step
   call live_timing(steps=1)
 end do
 call live_timing()
 !
 ! Field: Deallocation
 !
 deallocate(E_p,E_m,E_t,freqs)
 call mem_est("E_m E_p E_t freqs")
 !
 ! Polarization: FT at 0 and twice the field frequency
 !
 outfreqs(1)=(0._SP,0._SP)                                    ! Optical rectification
 outfreqs(2)=cmplx(Efield(i_Probe)%frequency(1)*2._SP,0._SP)  ! SHG
 do i_c=1,3
   !
   if (i_c==1) FT_title=' (X axis)'
   if (i_c==2) FT_title=' (Y axis)'
   if (i_c==3) FT_title=' (Z axis)'
   !
   call RT_1D_Fourier_Transform('T2W',FT_title,outfreqs,X_irrep(:,i_c),nfreqs,&
&                               RT_P_t(i_t_start:i_t_end,i_c),RED_steps,J_time_step,&
&                               (i_t_start-1)*J_time_step,Use_FFT,1)
   !
 enddo
 !
 ! P(2wo,ic)= 2\pi E_2minus \sum_{ij} E_versor(i) E_versor(j)\tilde X_ij(2wo,ic)
 ! P(0,ic)  = 2\pi E_2plus  \sum_{ij} E_versor(i) E_versor(j)\tilde X_ij(0,ic)
 !
 ! so what I get is  \sum_{ij} E_versor(i) E_versor(j) \tilde X_ij(W,ic), W=0,2wo
 ! depending on the symmetry of the system I device calcs so to get the desired X_ij(W,ic)
 !
 X_irrep(1,:)=pi*X_irrep(1,:)/E_2plus
 X_irrep(2,:)=2_DP*pi*X_irrep(2,:)/E_2minus
 !
 ! Output results
 !
 do i_w = 1, nfreqs
   if (i_w==1) X_file_name="X_probe_order_0"
   if (i_w==2) X_file_name="X_probe_order_2"
   !
   call RT_output("open "//trim(X_file_name))
   !
   X_tot=4._SP*pi*sum(X_irrep(i_w,:)*Efield(i_Probe)%versor(:))
   call RT_output(trim(X_file_name),VALUEs=(/aimag(X_tot*SVCMm12VMm1/AU2VMm1),real(X_tot*SVCMm12VMm1/AU2VMm1)/),&
&                      E=Efield(i_Probe)%frequency(1)*HA2EV)
   !
   call RT_output("close "//trim(X_file_name))
 enddo
  !
end subroutine RT_X_QSSIN
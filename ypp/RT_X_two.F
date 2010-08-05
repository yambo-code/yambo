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
subroutine RT_X_two(en,k)
 !
 use YPP,            ONLY:RT_P_t,X_rt,Use_FFT,RT_J_t,A_vecpot_vel_t
 use pars,           ONLY:SP,DP,pi,cI
 use units,          ONLY:HARTREE,SPEED_OF_LIGHT
 use electrons,      ONLY:levels
 use com,            ONLY:error
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:NE_steps,RT_step,RT_ind_J,RT_output,RT_P,RT_output
 use fields,         ONLY:Efield
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: k
 !
 ! Work Space
 !
 integer, parameter :: nfreqs=6
 complex(DP), allocatable :: A_vecpot_vel_w(:,:),X_irrep(:,:)
 complex(SP) :: X_tot,freqs(nfreqs)
 complex(DP) :: E_tot
 integer     :: i_w
 real(SP)    :: W_reached
 !
 if(trim(Efield(1)%ef_name)/="QSSIN") &
&  call error(" Non linear optics only with QSSIN field! ")
 !
 call section("=",'Non-Linear Optics')
 !=========================
 !
 ! Allocation
 !
 allocate(A_vecpot_vel_w(1,3),X_irrep(nfreqs,3)) 
 call mem_est("X_irrep A_vecpot_vel_w",(/size(X_irrep),size(A_vecpot_vel_w)/),(/2*DP,2*DP/))
 !
 do i_w=1,nfreqs
   freqs(i_w)=cmplx(Efield(1)%frequency*i_w,0._SP)
 enddo
 !
 call Fourier_Transform(freqs,A_vecpot_vel_w,1,A_vecpot_vel_t,NE_steps,RT_step,3)
 call Fourier_Transform(freqs,X_irrep,nfreqs,RT_P_t,NE_steps,RT_step,3)
 !
 ! Due to the definition of FFT 
 !
 X_irrep(:,:)       =X_irrep(:,:)*RT_step
 A_vecpot_vel_w(:,:)=A_vecpot_vel_w(:,:)*RT_step
 !
 E_tot=-sum(A_vecpot_vel_w(1,:)*Efield(1)%versor(:))/SPEED_OF_LIGHT
 !
 ! Evaluate X1, X2, .... ,Xnfreqs
 !
 writE(*,*) ' E_tot ',E_tot
 do i_w=1,nfreqs
   write(100,*) i_w,E_tot**real(i_w,SP),sum(X_irrep(i_w,:)*Efield(1)%versor(:))
   X_irrep(i_w,:)=X_irrep(i_w,:)/(E_tot**real(i_w,SP))
 enddo
 !
 call RT_output("open nonlinear")
 !
 do i_w = 1, nfreqs
   !
   W_reached = real(freqs(i_w))
   X_tot=sum(X_irrep(i_w,:)*Efield(1)%versor(:))
   call RT_output('nonlinear',(/aimag(X_tot),real(X_tot)/),TIME_or_E=W_reached*HARTREE)
   !
 enddo
 call RT_output("close nonlinear")
 !
 deallocate(A_vecpot_vel_w,X_irrep)
 call mem_est("A_vecpot_vel_w X_irrep")

 !
end subroutine

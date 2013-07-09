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
subroutine el_density_matrix(G_lesser,en,Xk,rho,lowest_band)
 !
 ! Electronic density matrix (meaning including the off-diagonal elements of the
 ! lesser Green function)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym,i_time_rev
 use FFT_m,         ONLY:fft_size,fft_rot_r
 use wave_func,     ONLY:wf_state,wf
 use SC,            ONLY:SC_bands
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_WF_k,PAR_COM_WF_k_A2A,PAR_IND_B_mat_ordered
 use matrix_operate,ONLY:matrix_linear_index
 !
 implicit none
 type(bz_samp)::Xk
 complex(SP), intent(in), optional :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),Xk%nibz)
 integer,     intent(in) :: lowest_band
 type(levels) ::en       
 real(SP)     ::rho(fft_size)
 !
 ! Work Space 
 !
 integer :: ib1,ib2,ik,ifft1,rho_syms,ifft2,ir
 real(SP):: rho_no_sym(fft_size),f_occ
 !
 rho=0._SP
 rho_no_sym=0._SP
 !
 ! Off-diagonal contributions
 !
 do ik=1,Xk%nibz
   !
   if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
   !
   do ib1=lowest_band,SC_bands(1)-1
     ifft1=wf_state(ib1,ik,1)
     f_occ=en%f(ib1,ik,1)
     rho_no_sym(:)=rho_no_sym(:)+f_occ*Xk%weights(ik)*abs(wf(:,ifft1))**2./real(PAR_COM_WF_k_A2A%n_CPU)
   enddo
   !      
   do ib1=SC_bands(1),SC_bands(2)
     !
     do ib2=ib1,SC_bands(2)
       !
       if (.not.PAR_IND_B_mat_ordered%element_1D(  matrix_linear_index(ib1-SC_bands(1)+1,ib2-SC_bands(1)+1)-1 )) cycle
       !      
       ifft1=wf_state(ib1,ik,1)
       ifft2=wf_state(ib2,ik,1)
       !
       if (ib1==ib2) then
         f_occ=real(-(0._SP,1._SP)*G_lesser(ib1,ib2,ik),SP)
       else
         f_occ=real(-(0._SP,2._SP)*G_lesser(ib1,ib2,ik),SP)
       endif
       !
       rho_no_sym(:)=rho_no_sym(:)+Xk%weights(ik)*f_occ*real(wf(:,ifft1)*conjg(wf(:,ifft2)))
       !
     enddo
   enddo
   !
 enddo
 !
 call PP_redux_wait(rho_no_sym)
 !
 ! Simmetrization
 !
 rho_syms=nsym/(i_time_rev+1)
 !
 do ir=1,rho_syms
   rho(:)=rho(:)+real(rho_no_sym(fft_rot_r(ir,:)),SP)/real(nsym,SP)
 enddo
 !
 rho(:)=(1+i_time_rev)*rho(:)
 !
end subroutine

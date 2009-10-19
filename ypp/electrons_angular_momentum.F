! 
! Copyright (C) 2000-2008 A. Marini, D. Sangalli and the YAMBO team
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
subroutine electrons_angular_momentum()
 !
 ! Important:
 ! We are in presence of a potential vector field, so
 ! L = r x p                  is the quantized canonical momentum
 ! M = r x (p-A) = r x mv     is the mechanical angula momentum,
 !                            which is gauge invariant
 !
 use pars,           ONLY:SP
 use xc_functionals, ONLY:el_mag
 use magnetic,       ONLY:wf_x,wf_y,MAG_B_z,MAG_gauge,MAG_landau,MAG_pauli,&
&                         A_magn_x,A_magn_y,x,y,gauge_factors,&
&                         MAG_hamiltonian_type
 use FFT_m,          ONLY:fft_dim,fft_size
 use D_lattice,      ONLY:a
 use wave_func,      ONLY:wf,wf_state,wf_load,WF_free
 use QP_m,           ONLY:QP_nk
 use SC,             ONLY:SC_bands,SC_R
 use electrons,      ONLY:n_sp_pol
 use timing,         ONLY:live_timing
 use units,          ONLY:HARTREE,BOHR
 !
 implicit none
 !
 ! Input variables
 integer                  :: call_type
 !
 ! Internal variables
 complex(SP), allocatable :: Lz_tmp(:),Mz_tmp(:),r_tmp(:),r2_tmp(:),v2_tmp(:)
 complex(SP)              :: Lz,Mz,r,r2,v2
 !
 ! Dummyes
 integer                  :: ifft,ib,ik,i_spin
 real(SP), allocatable    :: cosphi(:),sinphi(:),rad(:)
 ! DEBUG <
 real(SP), allocatable    :: r_cell_cc(:,:)
 integer                  :: ir,ir1,ir2,ir3,i1
 ! DEBUG >
 !
#if defined _DOUBLE
 complex(SP):: zdotc
#else
 complex(SP):: cdotc
#endif
 !
 call WF_derivative(.true.,(/1,SC_bands/),(/1,QP_nk/),(/.true.,.true.,.false./))
 call wf_load(0,1,(/1,SC_bands/),(/1,QP_nk/),title='-MAG')
 !
 MAG_landau=trim(MAG_hamiltonian_type)=='landau' .or. &
&           trim(MAG_hamiltonian_type)=='all'
 MAG_pauli=trim(MAG_hamiltonian_type)=='pauli' .or. &
&          trim(MAG_hamiltonian_type)=='all'
 ! Build up the vector potential
 call MAG_common_build_A(.true.)
 !
 ! Prepare files for the output
 ! Evaluate L_z and Mz
 open(unit=332211,file='Lz_SC.txt',status='unknown')
 if(MAG_landau) open(unit=332213,file='Mz_SC.txt',status='unknown')
 ! Evaluate v2=[(py-Ay)*sin(a)-(px-Ax)*cos(a)]^2
 open(unit=232211,file='v2_SC.txt',status='unknown')
 ! Evaluate r2=x^2+y^2
 open(unit=132211,file='r2_SC.txt',status='unknown')
 ! Summary
 open(unit=112211,file='Summary_SC.txt',status='unknown')
 !
 allocate(Lz_tmp(fft_size))
 if(MAG_landau) allocate(Mz_tmp(fft_size))
 allocate(r2_tmp(fft_size))
 allocate(v2_tmp(fft_size))
 allocate(r_tmp(fft_size))
 allocate(cosphi(fft_size),sinphi(fft_size),rad(fft_size))
 !
 Lz_tmp=(0.,0.)
 Lz=(0.,0.)
 if(MAG_landau) then
 Mz_tmp=(0.,0.)
 Mz=(0.,0.)
 endif
 r2_tmp(:)=(0.,0.)
 r2=(0.,0.)
 v2_tmp=(0.,0.)
 v2=(0.,0.)
 r_tmp(:)=(0.,0.)
 r=(0.,0.)
 cosphi(1)=0.
 sinphi(1)=0.
 rad(:)=sqrt(x(:)**2+y(:)**2)
 do i1=1,fft_size
   if ( rad(i1) < epsilon(1.) ) rad(i1)=epsilon(1.) 
 enddo
 cosphi(:) = x(:)/rad(:)
 sinphi(:) = y(:)/rad(:)
 !
 call live_timing("MAG-eval Lz",QP_nk*n_sp_pol*SC_bands)                        !*SC_bands) 
 if(MAG_landau) call live_timing("MAG-eval Lz and Mz ",QP_nk*n_sp_pol*SC_bands) !*SC_bands)
 !
 do ik=1,QP_nk
   ! I load the wf and the derivatives (first the derivatives, important!)
!   call WF_derivative(.true.,(/1,SC_bands/),(/ik,ik/),(/.true.,.true.,.false./))
!   call wf_load(0,1,(/1,SC_bands/),(/ik,ik/),title='-MAG')
   !
   do i_spin=1,n_sp_pol
     do ib=1,SC_bands
       ifft=wf_state(ib,ik,i_spin)
       if(i_spin==2.and..not.MAG_pauli) cycle ! This will be needed for non collinear systems
       !
       !  r x p = Lz (this is quantized)
       Lz_tmp(:)=x(:)*wf_y(:,ifft)-y(:)*wf_x(:,ifft)
       !  r2 and r
       r2_tmp(:)=rad(:)**2*wf(:,ifft)
       r_tmp(:)=rad(:)*wf(:,ifft)
       !
       if(MAG_landau) then
         ! Subtract to Lz the expectation value of (r x A)
         ! Mz= Lz - (r x A)
         Mz_tmp(:)=Lz_tmp(:)-(x(:)*A_magn_y(:)*gauge_factors(1)- &
&                             y(:)*A_magn_x(:)*gauge_factors(2))*wf(:,ifft)
         ! v2 --> (< [(py-Ay)*cons(a) - (px-Ax)*sin(a)]^2 >)
         v2_tmp(:)=  (wf_y(:,ifft) - A_magn_y(:)*wf(:,ifft))*sinphi(:) - &
&                    (wf_x(:,ifft) - A_magn_y(:)*wf(:,ifft))*cosphi(:)  
       else
         ! v2 --> < (py*cos(a)-px*sin(a))^2 >
         v2_tmp(:)= wf_y(:,ifft)*cosphi(:) - wf_x(:,ifft)*sinphi(:)
       endif
       !
#if defined _DOUBLE
       Lz = zdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
       if(MAG_landa) Mz = zdotc(fft_size,wf(:,ifft),1,Mz_tmp(:),1)
       r  = zdotc(fft_size,wf(:,ifft),1,r_tmp(:),1)
       r2 = zdotc(fft_size,wf(:,ifft),1,r2_tmp(:),1)
       v2 = zdotc(fft_size,v2_tmp(:),1,v2_tmp(:),1)
#else
       Lz = cdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
       if(MAG_landau)  Mz = cdotc(fft_size,wf(:,ifft),1,Mz_tmp(:),1)
       r  = cdotc(fft_size,wf(:,ifft),1,r_tmp(:),1)
       r2 = cdotc(fft_size,wf(:,ifft),1,r2_tmp(:),1)
       v2 = cdotc(fft_size,v2_tmp(:),1,v2_tmp(:),1)
#endif
       !
       write(332211,*) 'Lz',ib,i_spin,ik,Lz
       if(MAG_landau) write(332213,*) 'Mz',ib,i_spin,ik,Mz
       write(232211,*) 'v2',ib,i_spin,ik,v2
       write(132211,*) 'r2',ib,i_spin,ik,r2
       write(112211,*) real(Lz),real(Mz),0.5*real(v2)*HARTREE,&
&            real(r)*BOHR,sqrt(real(r2))*BOHR
       !
       call live_timing(steps=1)
       !
     enddo
   enddo
 enddo
 !
 call live_timing() 
 !
 close(unit=332211)
 if(MAG_landau) close(unit=332213)
 close(unit=232211)
 close(unit=132211)
 close(unit=112211)
 !
 deallocate(Lz_tmp,Mz_tmp,r_tmp,r2_tmp,v2_tmp)
 deallocate(cosphi,sinphi,rad)
 !
 if(MAG_landau) call MAG_common_build_A(.false.)
 if(MAG_landau) call WF_derivative(.false.,(/0,0/),(/0,0/),(/.true.,.true.,.false./))    ! Deallocate the derivatives...
 call WF_free()
 !
 return
 !
end subroutine

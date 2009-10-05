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
  !
 ! Important:
 ! We are in presence of a potential vector field, so
 ! L = r x P = r x (p-A_gauge)    
 ! is the canonical angula momentum, which is gauge invariant
 ! Remember that for the LDA wf A=0, while for the SC wf A_gauge
 ! could be different from zero
 !
 use pars,           ONLY:SP
 use xc_functionals, ONLY:el_mag
 use magnetic,       ONLY:wf_x,wf_y,MAG_B_z,MAG_gauge,MAG_landau,&
&                         A_magn_x,A_magn_y,x,y,gauge_factors,&
&                         MAG_hamiltonian_type
 use FFT_m,          ONLY:fft_dim,fft_size
 use D_lattice,      ONLY:a
 use wave_func,      ONLY:wf,wf_state,wf_load,WF_free
 use QP_m,           ONLY:QP_nk
 use SC,             ONLY:SC_bands,SC_R
 use electrons,      ONLY:n_sp_pol
 use timing,         ONLY:live_timing
 !
 implicit none
 !
 ! Input variables
 integer                  :: call_type
 !
 ! Internal variables
 complex(SP), allocatable :: Lz_tmp(:),Lz(:,:,:,:)
 !
 ! Dummyes
 integer                  :: ifft,jfft,ib,ibp,ik,i_spin
 ! DEBUG <
 real(SP),allocatable     :: r_cell_cc(:,:)
 integer                  :: ir,ir1,ir2,ir3
 ! DEBUG >
 !
#if defined _DOUBLE
 complex(SP):: zdotc
#else
 complex(SP):: cdotc
#endif
 !
 ! Then evaluate L_z
 open(unit=332211,file='Lz_SC.txt',status='unknown')
 open(unit=332212,file='Lz_SC_diag.txt',status='unknown')
 open(unit=332213,file='Mz_SC.txt',status='unknown')
 open(unit=332214,file='Mz_SC_diag.txt',status='unknown')
 !
 call WF_derivative(.true.,(/1,SC_bands/),(/1,QP_nk/),(/.true.,.true.,.false./))
 call wf_load(0,1,(/1,SC_bands/),(/1,QP_nk/),title='-MAG')
 !
 MAG_landau=trim(MAG_hamiltonian_type)=='landau' .or. &
&           trim(MAG_hamiltonian_type)=='all'
 ! Build up the vector potential
 call MAG_common_build_A(.true.)
 !
 allocate(Lz_tmp(fft_size))
 allocate(Lz(SC_bands,SC_bands,QP_nk,n_sp_pol))
 Lz_tmp(:)=(0.,0.)
 Lz(:,:,:,:)=(0.,0.)
 !
 call live_timing("MAG-eval Lz and Mz ",QP_nk*n_sp_pol*SC_bands*SC_bands)
 !
 do ik=1,QP_nk
   ! I load the wf and the derivatives (first the derivatives, important!)
!   call WF_derivative(.true.,(/1,SC_bands/),(/ik,ik/),(/.true.,.true.,.false./))
!   call wf_load(0,1,(/1,SC_bands/),(/ik,ik/),title='-MAG')
   !
   do i_spin=1,n_sp_pol
     do ib=1,SC_bands
       ifft=wf_state(ib,ik,i_spin)
       do ibp=1,SC_bands
         if(i_spin==2) cycle ! This will be needed for non collinear systems
         jfft=wf_state(ibp,ik,i_spin)
         !
         !  r x p component (This is quantized)
         Lz_tmp(:)=x(:)*wf_y(:,jfft)-y(:)*wf_x(:,jfft)
         !
#if defined _DOUBLE
         Lz(ib,ibp,ik,i_spin) = zdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
#else
         Lz(ib,ibp,ik,i_spin) = cdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
#endif
         !
         write(332211,*) 'Lz',ib,ibp,i_spin,ik,Lz(ib,ibp,ik,i_spin)
         if(ib==ibp) write(332212,*) 'Lz',ib,i_spin,ik,Lz(ib,ib,ik,i_spin)
         !
         ! -r x A component
         if(MAG_landau) then
           Lz_tmp(:)=Lz_tmp(:)-(x(:)*A_magn_y(:)*gauge_factors(1)- &
&                               y(:)*A_magn_x(:)*gauge_factors(2))*wf(:,jfft)
#if defined _DOUBLE
           Lz(ib,ibp,ik,i_spin) = zdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
#else
           Lz(ib,ibp,ik,i_spin) = cdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
#endif
           write(332213,*) 'Mz',ib,ibp,i_spin,ik,Lz(ib,ibp,ik,i_spin)
         endif
         !
         call live_timing(steps=1)
         !
       enddo
       write(332214,*) 'Mz',ib,i_spin,ik,Lz(ib,ib,ik,i_spin)
     enddo
   enddo
 enddo
 !
 call live_timing() 
 !
 close(unit=332211)
 close(unit=332212)
 close(unit=332213)
 close(unit=332214) 
 !
 deallocate(Lz_tmp,Lz)
 !
 if(MAG_landau) call MAG_common_build_A(.false.)
 if(MAG_landau) call WF_derivative(.false.,(/0,0/),(/0,0/),(/.true.,.true.,.false./))    ! Deallocate the derivatives...
 call WF_free()
 !
 return
 !
end subroutine

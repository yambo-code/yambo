!
! Copyright (C) 2000-2008 D. Sangalli and the YAMBO team 
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
subroutine MAG_eval_Lz(call_type) 
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
 use magnetic,       ONLY:wf_x,wf_y,A_magn_x,A_magn_y,MAG_B_z,gauge_factors,&
&                         x,y
 use FFT_m,          ONLY:fft_dim,fft_size
 use D_lattice,      ONLY:a
 use wave_func,      ONLY:wf,wf_state
 use QP_m,           ONLY:QP_nk
 use SC,             ONLY:SC_bands,SC_R
 use electrons,      ONLY:n_sp_pol
 !
 implicit none
 !
 ! Input variables
 integer                  :: call_type
 !
 ! Internal variables
 complex(SP), allocatable :: Lz_tmp(:),Lz(:,:,:,:),wf_x_new(:,:),wf_y_new(:,:)

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
 if(call_type==1) then
   !
   ! First evaluate Lz for the LDA wf
   ! 
   allocate(Lz_tmp(fft_size))
   allocate(Lz(SC_bands,SC_bands,QP_nk,n_sp_pol)) 
   Lz_tmp(:)=(0.,0.)
   Lz(:,:,:,:)=(0.,0.)
   !
   open(unit=112233,file='Lz_LDA.txt',status='unknown')
   open(unit=112234,file='Lz_LDA_diag.txt',status='unknown')
   !
   do ik=1,QP_nk
     do i_spin=1,n_sp_pol   
       do ib=1,SC_bands   
         ifft=wf_state(ib,ik,i_spin)  
         do ibp=1,SC_bands
           !
           if(i_spin==2) cycle
           !
           jfft=wf_state(ibp,ik,i_spin) 
           !
           ! Verify Lz hermetian proprieties
           Lz_tmp(:)=x(:)*wf_y(:,jfft)-y(:)*wf_x(:,jfft)
           !
#if defined _DOUBLE
           Lz(ib,ibp,ik,i_spin) = zdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
#else
           Lz(ib,ibp,ik,i_spin) = cdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
#endif
           write(112233,*) 'Lz',ib,ibp,i_spin,ik,Lz(ib,ibp,ik,i_spin)
         enddo
         write(112234,*) 'Lz',ib,i_spin,ik,Lz(ib,ib,ik,i_spin)
       enddo
     enddo
   enddo
   !
   deallocate(Lz_tmp,Lz)
   !
   close(unit=112233)
   close(unit=112234)
   !
   return
 endif
 !
 ! Then evaluate Lz after the SC loop
 ! Step 1: rotate the wf_derivatives
 allocate(Lz(SC_bands,SC_bands,QP_nk,n_sp_pol))
 allocate(Lz_tmp(fft_size))
 Lz_tmp(:)=(0.,0.)
 Lz(:,:,:,:)=(0.,0.)
 allocate(wf_x_new(fft_size,QP_nk*n_sp_pol*SC_bands),wf_y_new(fft_size,QP_nk*n_sp_pol*SC_bands))
 wf_x_new(:,:)=(0.,0.)
 wf_y_new(:,:)=(0.,0.)
 !
 do ik=1,QP_nk
   do i_spin=1,n_sp_pol
     do ib  = 1,SC_bands
       ifft = wf_state(ib,ik,i_spin)
       do ibp = 1,SC_bands
         jfft = wf_state(ibp,ik,i_spin)
         !
         call caxpy(fft_size,SC_R(ibp,ib,ik,i_spin),wf_x(:,jfft),1,wf_x_new(:,ifft),1)
         !
         call caxpy(fft_size,SC_R(ibp,ib,ik,i_spin),wf_y(:,jfft),1,wf_y_new(:,ifft),1)
       enddo
     enddo
   enddo
 enddo
 !
 deallocate(wf_x,wf_y)
 !
 ! DEBUG <
 allocate(r_cell_cc(fft_size,3))
 r_cell_cc(:,:)=0.
 open(unit=3459, file='wf_new.txt', status='unknown')
 open(unit=34510, file='wf_x_new.txt', status='unknown')
 !open(unit=3458, file='wf_y.txt', status='unknown')
 do ir1 = 1, fft_dim(1)-1
   do ir2 = 0, fft_dim(2)-1
     do ir3 = 0, fft_dim(3)-1
       ir = 1 + ir1 + ir2*fft_dim(1) + ir3*fft_dim(1)*fft_dim(2)
       ! Definition: rcell_i=a1_i+a_2i+a3_i     i=x,y,z
       !             in cartesian coordinates
       r_cell_cc(ir,:) =  ir1*a(1,:)/fft_dim(1) +&
 &                         ir2*a(2,:)/fft_dim(2) +&
 &                         ir3*a(3,:)/fft_dim(3)
     enddo
   enddo
 enddo
 do ir = 1, fft_dim(1)
   write(3459,*) r_cell_cc(ir,1),real(wf(ir,6:7)),aimag(wf(ir,6:7))
   write(34510,*) r_cell_cc(ir,1),-aimag(wf_x_new(ir,6:7)),real(wf_x_new(ir,6:7))
 enddo
 close(unit=3459)
 close(unit=34510)
 !close(unit=3458)
 ! DEBUG >
 !
 ! Step 2: evaluate L_z for the new wf
 open(unit=332211,file='Lz_SC.txt',status='unknown')
 open(unit=332212,file='Lz_SC_diag.txt',status='unknown')
 !
 do ik=1,QP_nk
   do i_spin=1,n_sp_pol
     do ib=1,SC_bands   
       ifft=wf_state(ib,ik,i_spin)  
       do ibp=1,SC_bands
         if(i_spin==2) cycle
         jfft=wf_state(ibp,ik,i_spin) 
         !
         !  r x p component
         Lz_tmp(:)=x(:)*wf_y_new(:,jfft)-y(:)*wf_x_new(:,jfft)
         ! -r x A component
!         Lz_tmp(:)=Lz_tmp(:)-(x(:)*A_magn_y(:)*gauge_factors(1)- &
!&                             y(:)*A_magn_x(:)*gauge_factors(2))*wf(:,jfft)
         ! -r x A_gauge component
         Lz_tmp(:)=Lz_tmp(:)-(x(:)*A_magn_y(:)*(gauge_factors(1)-1)- &
&                             y(:)*A_magn_x(:)*(gauge_factors(2)-1))*wf(:,jfft)
         !
#if defined _DOUBLE
         Lz(ib,ibp,ik,i_spin) = zdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
#else
         Lz(ib,ibp,ik,i_spin) = cdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
#endif
         !
         write(332211,*) 'Lz',ib,ibp,i_spin,ik,Lz(ib,ibp,ik,i_spin)
       enddo
       write(332212,*) 'Lz',ib,i_spin,ik,Lz(ib,ib,ik,i_spin)
     enddo
   enddo
 enddo
 !
 close(unit=332211)
 close(unit=332212)
 !
 ! Final clean
 ! 
 deallocate(Lz,Lz_tmp,wf_x_new,wf_y_new)
 deallocate(A_magn_x,A_magn_y)
 deallocate(x,y)
 !
 !
end subroutine

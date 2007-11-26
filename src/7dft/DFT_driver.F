!
! Copyright (C) 2000-2005 A. Marini, M. Gruning and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine DFT_driver(X,E,k,q)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:DL_vol
 use QP_m,           ONLY:QP_state,QP_nb,QP_nk,QP_Vxc,QP_n_states,QP_table
 use DFT,            ONLY:DFT_SC,DFT_bands, DFT_iterations,DFT_threshold
 use wave_func,      ONLY:wf_load, wf, wf_ncx, wf_state,wf_free
 use memory_m,       ONLY:mem_est
 use FFT_m,          ONLY:fft_size
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK
 use com,            ONLY:msg
 use X_m,            ONLY:X_t
 implicit none
 type(levels) ::E
 type(bz_samp)::k,q
 type(X_t)    ::X
 ! 
 ! Work Space
 !
 integer                  :: it_on_disk,ir,i1,i2,it,ik,ifft,ifftp,ib,ibp
 real(SP)                 :: test_convergence
 complex(SP)              :: Vh
 real(SP), allocatable    :: rho_nm1(:),rho(:),vhr(:),vxc(:)
 complex(SP), allocatable :: Ho(:,:,:), Hsc(:,:), wf_nm1(:,:),Htmp(:,:), c_ws(:)
#if defined _DOUBLE
 complex(DP)             ::zdotc,zdotu
#else 
 complex(SP)             ::cdotc,cdotu
#endif
 !
 it_on_disk = 1
 DFT_threshold = 1.e-4
 DFT_iterations = 100
 !
 call section('*','DFT '//trim(DFT_SC)//' potentials')
 !
 ! Table for QP indexes
 !
 QP_nb=DFT_bands
 QP_nk=k%nibz
 if (allocated(QP_state)) deallocate(QP_state)
 allocate(QP_state(QP_nb,QP_nk))
 QP_state=.TRUE.
 !
 call QP_state_table_setup(E)
 !
 ! WF
 !
 call wf_load(0,1,(/1,DFT_bands/),(/1,k%nibz/),title=' ')
 allocate(wf_nm1(fft_size,QP_nb*QP_nk))
 wf_nm1 = wf
 !
 ! Vxc[rho_0] and Vh[rho_0]
 !
 allocate(rho(fft_size),vhr(fft_size),c_ws(fft_size)) 
 allocate(QP_Vxc(QP_n_states))
 call mem_est("QP_Vxc",(/QP_n_states/))
 call DFT_Vxc(E,k)
 call el_density(E,k,rho,.FALSE.)
 call V_Hartree(rho,vhr)
 !
 do i1=1,QP_n_states
   !
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   ik =QP_table(i1,3)
   !
   ! <ifft|V_hr|ifftp>
   !
   ifft =wf_state(ib,ik,1)
   ifftp=wf_state(ibp,ik,1)
   c_ws(:)=vhr(:)*wf(:,ifftp)
   !
#if defined _DOUBLE
   Vh =zdotc(fft_size,wf(:,ifft),1,c_ws(:),1)
#else 
   Vh =cdotc(fft_size,wf(:,ifft),1,c_ws(:),1)
#endif
   QP_Vxc(i1) = QP_Vxc(i1) + Vh 
 end do
 !
 !  The Ho = -nabla**2. + V_ext is deduced from the input energies en0:
 !  H = Ho + v_hxc[rho_in] and
 !  H psi_0(n) = en0(n) psi_0(n)
 !  then Ho_n,m = en0(n) delta_n,m - v_hxc[rho_0]_n,m
 !  where the matrix elements are calculated from the  psi_0(n)
 !
 allocate(Ho(QP_nb,QP_nb,QP_nk))
 Ho = (0.,0.)
 call mem_est("Ho",(/QP_n_states/))
 do i1 = 1, QP_n_states
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   ik =QP_table(i1,3)
   if (ib.eq.ibp) Ho(ib,ibp,ik) = E%E(ib,ik,1) + E%Efermi(1)
   Ho(ib,ibp,ik) = Ho(ib,ibp,ik) - QP_Vxc(i1)
 end do
 deallocate(QP_Vxc)
 !
 ! SCF cycle
 !
 allocate(Hsc(QP_nb,QP_nb),rho_nm1(fft_size),vxc(fft_size))
 print*, '0, 1', (E%E(i1,1,1),i1=1,7)  
 print*, '0, 2', (E%E(i1,2,1),i1=1,7)  
 do it = it_on_disk, DFT_iterations
   wf = (0.,0.)
   do ik = 1, QP_nk ! Loop on k_points
     !
     ! New Vxc
     !
     !vxc = 0.
     !call Test_Vx(rho,vxc)
     !
     call V_exx(X,E,k,Vxc)
     !
     Hsc = (0.,0.)
     !
     do i1=1,QP_n_states
       !
       ib =QP_table(i1,1)
       ibp=QP_table(i1,2)
       !
       ! <ifft|V_hxc|ifftp>
       !
       ifft =wf_state(ib,ik,1)
       ifftp=wf_state(ibp,ik,1)
       c_ws(:)=(vhr(:)+vxc(:))*wf_nm1(:,ifftp)
       !
#if defined _DOUBLE
       Hsc(ib,ibp) = zdotc(fft_size,wf_nm1(:,ifft),1,c_ws(:),1)
#else 
       Hsc(ib,ibp) = cdotc(fft_size,wf_nm1(:,ifft),1,c_ws(:),1)
#endif
       !
     end do
     Hsc(:,:) = Ho(:,:,ik)  + Hsc(:,:)
     !
     ! Diagonalize the H matrix
     !
     call mat_dia_inv(DIAGO,USE_LK,Hsc,E_real=E%E(:,ik,1))
     ! debug write (messaging needed)
     !
     print*, ik, (E%E(i1,ik,1),i1 = 1,7)
     ! New wave functions (Here blas cdotc?) 
     !
     do i1 = 1,QP_nb
       do i2 = 1,QP_nb
         ifft = wf_state(i1,ik,1)
         ifftp = wf_state(i2,ik,1)
         wf(:,ifft)= wf(:,ifft) + wf_nm1(:,ifftp)*Hsc(i2,i1)
       end do
     end do 
     !
     !  Transform Ho in the new basis 
     !
     allocate(Htmp(QP_nb,QP_nb))
#if defined _DOUBLE
       call zgemm('n','n',QP_nb,QP_nb,QP_nb,(1._SP,0._SP),Ho(:,:,ik),QP_nb,Hsc,QP_nb,(0._SP,0._SP),Htmp,QP_nb)
#else
       call cgemm('n','n',QP_nb,QP_nb,QP_nb,(1._SP,0._SP),Ho(:,:,ik),QP_nb,Hsc,QP_nb,(0._SP,0._SP),Htmp,QP_nb)
#endif
       Ho(:,:,ik) = Htmp(:,:)
#if defined _DOUBLE
       call zgemm('c','n',QP_nb,QP_nb,QP_nb,(1._SP,0._SP),Hsc,QP_nb,Ho(:,:,ik),QP_nb,(0._SP,0._SP),Htmp,QP_nb)
#else
       call cgemm('c','n',QP_nb,QP_nb,QP_nb,(1._SP,0._SP),Hsc,QP_nb,Ho(:,:,ik),QP_nb,(0._SP,0._SP),Htmp,QP_nb)
#endif
       Ho(:,:,ik) = Htmp(:,:)
       deallocate(Htmp)
       !
     end do !Exit loop on kpoints
     !
     wf_nm1 = wf
     !
     ! New density 
     !
     rho_nm1 = rho 
     call el_density(E,k,rho,.FALSE.)
     !
     ! Test convergence
     !
     test_convergence = 0.
     do ir=1,fft_size
       test_convergence = test_convergence + abs(rho(ir)-rho_nm1(ir))/DL_vol
     enddo
     ! debug write (messaging needed)
     print*, it, test_convergence   
     !
     if (test_convergence.lt.DFT_threshold) then
       call msg('r','Accuracy (reached)  :',test_convergence)
       call msg('r','Iterations          :',it)
       exit
     endif
     !
     ! Linear mixing
     !
     if (it>1) rho = 0.3 * rho + 0.7 * rho_nm1 
     !
     ! Hartree potential
     !
     call V_Hartree(rho,vhr)     
     !
   end do
   !
   ! exit SCF cycle
   !
   deallocate(Ho,Hsc,wf_nm1)
   !
   !
   !
   contains !temporary test subroutine
     subroutine Test_Vx(rho_sp,vx_sp)
       !
       use pars,  ONLY: pi,SP,DP
       use FFT_m, ONLY: fft_size
       use D_lattice, ONLY: DL_vol
       !
       implicit none
       real(SP) :: rho_sp(fft_size),vx_sp(fft_size)
       real(DP) :: rho(fft_size),dvx(fft_size), &
&              vx(fft_size),ex(fft_size),rspts(fft_size)
       !
       ex = 0_DP
       vx = 0_DP
       dvx = 0_DP
       rho=rho_sp*real(fft_size,DP)/real(DL_vol,DP)
       forall (i1=1:fft_size) rspts(i1)=(3._DP/4._DP/pi/rho(i1))**(1._DP/3._DP)
       call xcxalp(1._DP,ex,fft_size,1,rspts,vx,dvx)
       vx_sp = vx
     end subroutine Test_Vx
     !
 end subroutine DFT_driver
 !
 ! Sigma_x
 !
 !call QP_XX_Vxc(E,k,k,q,.FALSE.,.FALSE.) 


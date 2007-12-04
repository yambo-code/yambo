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
subroutine SC_driver(X,E,k,q)
 !
 use pars,           ONLY:SP, HARTREE
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,qindx_S
 use D_lattice,      ONLY:DL_vol
 use QP_m,           ONLY:QP_state,QP_nb,QP_nk,QP_Vxc,QP_n_states,QP_table,&
&                         QP_ng_Sx
 use SC,             ONLY:SC_bands, SC_iterations,SC_threshold
 use wave_func,      ONLY:wf_load, wf, wf_state
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
 real(SP), allocatable    :: rho_nm1(:),rho(:),vhr(:),vxc(:),vxc_nm1(:)
 complex(SP), allocatable :: Ho(:,:,:), Hsc(:,:), wf_tmp(:,:),Ho_tmp(:,:), c_ws(:)
#if defined _DOUBLE
 complex(DP)             ::zdotc,zdotu
#else 
 complex(SP)             ::cdotc,cdotu
#endif
 !
 it_on_disk = 1
 !
 call section('*','Self-Consistenty')
 !
 ! Table for QP indexes
 !
 QP_nb=SC_bands
 QP_nk=k%nibz
 if (allocated(QP_state)) deallocate(QP_state)
 allocate(QP_state(QP_nb,QP_nk))
 QP_state=.TRUE.
 !
 call QP_state_table_setup(E)
 !
 ! WF
 !
 call wf_load(max(QP_ng_Sx,X%ng),maxval(qindx_S(:,:,2)),&
&             (/1,max(SC_bands,X%ib(2))/),(/1,k%nibz/),title=' ')
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
 allocate(Hsc(QP_nb,QP_nb),rho_nm1(fft_size),vxc(fft_size),vxc_nm1(fft_size))
 !
 do it = it_on_disk, SC_iterations
   !
   ! New Vxc
   !
   vxc_nm1 = vxc
   call V_exx(X,E,k,q,vxc)
   !
   if (it>1) vxc = .3* vxc + .7*vxc_nm1 !linear mixing
   !
   ! Hartree potential
   !
   call V_Hartree(rho,vhr)
   !
   ! Loop on k_points
   !
   do ik = 1, QP_nk 
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
       c_ws(:)=(vhr(:)+vxc(:))*wf(:,ifftp)
       !
#if defined _DOUBLE
       Hsc(ib,ibp) = zdotc(fft_size,wf(:,ifft),1,c_ws(:),1)
#else 
       Hsc(ib,ibp) = cdotc(fft_size,wf(:,ifft),1,c_ws(:),1)
#endif
       !
     end do
     Hsc(:,:) = Ho(:,:,ik)  + Hsc(:,:)   
     !
     ! Diagonalize the H matrix
     !
     call mat_dia_inv(DIAGO,USE_LK,Hsc,E_real=E%E(:,ik,1))
     !
     ! write energies (HERE messaging needed)
     !
     print*, ik, (E%E(i1,ik,1)*HARTREE,i1 = 1,7)
     !
     ! New wave functions (Here blas cdotc?) 
     !
     allocate(wf_tmp(fft_size,QP_nb))
     wf_tmp = (0.,0.)
     do i1 = 1,QP_nb
       do i2 = 1,QP_nb
         ifft = wf_state(i1,1,1)
         ifftp = wf_state(i2,ik,1)
         wf_tmp(:,ifft)= wf_tmp(:,ifft) + wf(:,ifftp)*Hsc(i2,i1)
       end do
     end do 
     forall(i1 = 1:QP_nb) wf(:,wf_state(i1,ik,1))= wf_tmp(:,wf_state(i1,1,1))  
     deallocate(wf_tmp)
     !
     !  Transform Ho in the new basis 
     !
     allocate(Ho_tmp(QP_nb,QP_nb))
#if defined _DOUBLE
     call zgemm('n','n',QP_nb,QP_nb,QP_nb,(1._SP,0._SP),Ho(:,:,ik),QP_nb,Hsc,QP_nb,(0._SP,0._SP),Ho_tmp,QP_nb)
#else
     call cgemm('n','n',QP_nb,QP_nb,QP_nb,(1._SP,0._SP),Ho(:,:,ik),QP_nb,Hsc,QP_nb,(0._SP,0._SP),Ho_tmp,QP_nb)
#endif
     Ho(:,:,ik) = Ho_tmp(:,:)
#if defined _DOUBLE
     call zgemm('c','n',QP_nb,QP_nb,QP_nb,(1._SP,0._SP),Hsc,QP_nb,Ho(:,:,ik),QP_nb,(0._SP,0._SP),Ho_tmp,QP_nb)
#else
     call cgemm('c','n',QP_nb,QP_nb,QP_nb,(1._SP,0._SP),Hsc,QP_nb,Ho(:,:,ik),QP_nb,(0._SP,0._SP),Ho_tmp,QP_nb)
#endif
     Ho(:,:,ik) = Ho_tmp(:,:)
     deallocate(Ho_tmp)
     !
   end do !Exit loop on kpoints
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
   !
   ! write scf status (HERE messaging needed)
   !
   print*, it, test_convergence   
   !
   if (test_convergence.lt.SC_threshold) then
     call msg('r','Accuracy (reached)  :',test_convergence)
     call msg('r','Iterations          :',it)
     exit
   endif
   !
   ! Linear mixing
   !
   rho = 0.3 * rho + 0.7 * rho_nm1 
   !
 end do
 !
 ! exit SCF cycle
 !
 deallocate(Ho,Hsc)
 !
end subroutine SC_driver

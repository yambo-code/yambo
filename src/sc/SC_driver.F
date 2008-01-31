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
 use pars,           ONLY:SP,HARTREE,schlen
 use drivers,        ONLY:l_xxvxc,l_chosex,l_sc_hartree,l_sc_xx
 use electrons,      ONLY:levels,nel
 use R_lattice,      ONLY:bz_samp,qindx_S
 use QP_m,           ONLY:QP_state,QP_nb,QP_nk,QP_Vxc,QP_n_states,QP_table,&
&                         QP_ng_Sx,QP_bands_mixed
 use SC,             ONLY:SC_bands, SC_iterations,SC_E_threshold,&
&                         SC_rho_threshold,SC_potential
 use wave_func,      ONLY:wf_load, wf, wf_state
 use memory_m,       ONLY:mem_est
 use FFT_m,          ONLY:fft_size
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK
 use com,            ONLY:msg
 use X_m,            ONLY:X_t
 use timing,         ONLY:live_time_msg
 use stderr,         ONLY:tty_size,intc,real2ch
 !
 use xc_functionals, ONLY:V_xc
 !
 implicit none
 type(levels) ::E
 type(bz_samp)::k,q
 type(X_t)    ::X
 !
 ! Iterations
 !
 integer                  :: it_on_disk,it,it_done
 real(SP)                 :: rho_convergence,E_convergence,E_save(SC_bands,k%nibz),&
&                            E_previous(SC_bands,k%nibz),dE(SC_bands,k%nibz)
 real(SP),    allocatable :: rho_nm1(:),rho_n(:),V_xc_n(:),V_xc_nm1(:),V_hr(:)
 complex(SP), allocatable :: WS_wf(:,:),WS_Ho(:,:),H_sc(:,:)
 ! 
 ! Work Space
 !
 integer                  :: ir,i1,i2,ik,ifft,ifftp,ib,ibp
 complex(SP), allocatable :: Ho(:,:,:)
 character(schlen)        :: dumb_ch
#if defined _DOUBLE
 complex(DP)             ::zdotc
#else 
 complex(SP)             ::cdotc
#endif
 !
 it_on_disk = 1
 !
 l_sc_hartree = trim(SC_potential)=='Hartree'
 l_xxvxc      =trim(SC_potential)=='Hartree-Fock'
 l_sc_xx      =trim(SC_potential)=='XX'
 l_chosex     =trim(SC_potential)=='CHOSEX'
 !
 if (.not.any((/l_sc_hartree,l_xxvxc,l_sc_xx,l_chosex/))) return
 !
 call section('*','Self-Consistent '//trim(SC_potential))
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
 call msg('r' ,'[SC] Bands                    :',SC_bands)
 call msg('r' ,'[SC] Mixed bands              :',QP_bands_mixed)
 call msg('r' ,'[SC] Treshold on density      :',SC_rho_threshold)
 call msg('rn','[SC]          on energies [ev]:',SC_E_threshold*HARTREE)
 !
 ! WF
 !
 call wf_load(max(QP_ng_Sx,X%ng),maxval(qindx_S(:,:,2)),&
&             (/1,max(SC_bands,X%ib(2))/),(/1,k%nibz/),title=' ')
 !
 ! Allocation(s)...
 !
 allocate(H_sc(QP_nb,QP_nb),Ho(QP_nb,QP_nb,QP_nk))
 call mem_est("Hs",(/QP_nb**2+QP_nb**2*QP_nk/))
 !
 allocate(rho_n(fft_size),rho_nm1(fft_size),&
&         V_xc_n(fft_size),V_xc_nm1(fft_size),V_hr(fft_size))
 call mem_est("Potentials",(/5*fft_size/),(/SP/))
 !
 allocate(WS_Ho(QP_nb,QP_nb),WS_wf(fft_size,QP_nb))
 call mem_est("WS",(/QP_nb**2+fft_size*QP_nb/))
 !
 allocate(QP_Vxc(QP_n_states))
 call mem_est("QP_Vxc",(/QP_n_states/))
 !
 !  Vxc[rho_0] and Vh[rho_0]
 !===========================
 !
 call DFT_Vxc(E,k)
 call el_density(E,k,rho_n,.FALSE.)
 call V_Hartree(rho_n,V_hr)
 !
 ! The Ho = -nabla**2. + V_ext is deduced from the input energies en0:
 !
 ! H = Ho + v_hxc[rho_in] and
 ! H psi_0(n) = en0(n) psi_0(n)
 !
 ! then Ho_n,m = en0(n) delta_n,m - v_hxc[rho_0]_n,m
 !
 ! where the matrix elements are calculated from the  psi_0(n)
 !
 ! ... so first store -Vhartree in Ho ...
 !========================================
 !
 Ho=(0.,0.)
 do ik=1,QP_nk
   call V_to_H(ik,-V_hr,Ho(:,:,ik))
 enddo
 !
 ! ... then remoce Vxc[rho_0] also
 !=================================
 !
 do i1 = 1, QP_n_states
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   ik =QP_table(i1,3)
   !
   if (ib==ibp) Ho(ib,ibp,ik) = Ho(ib,ibp,ik) + E%E(ib,ik,1) + E%Efermi(1)
   !
   Ho(ib,ibp,ik) = Ho(ib,ibp,ik) - QP_Vxc(i1)
   !
 end do
 deallocate(QP_Vxc)
 call mem_est("QP_Vxc")
 !
 ! SCF cycle
 !===========
 !
 V_xc_n  =(0.,0.)
 V_xc_nm1=(0.,0.)
 !
 E_save=E%E(:SC_bands,:,1)
 !
 do it = it_on_disk, SC_iterations
   !
   E_previous=E%E(:SC_bands,:,1)
   !
   ! Stop all messagging
   !
   if (it >  1 )call SC_mute("report log io_in io_out")
   if (it == 1 )call SC_mute("report io_in io_out")
   !
   V_xc_nm1=V_xc_n
   !
   ! TEST xc potential
   !===================
   !
   ! Kinds
   !  XC_EXCHANGE             = 1,  &
   !  XC_CORRELATION          = 2,  &
   !  XC_EXCHANGE_CORRELATION = 3
   !
   ! the LDAs
   !  XC_LDA_X             =  1,  &  ! Exchange
   !  XC_LDA_C_WIGNER      =  2,  &  ! Wigner parametrization
   !  XC_LDA_C_RPA         =  3,  &  ! Random Phase Approximation
   !  XC_LDA_C_HL          =  4,  &  ! Hedin & Lundqvist
   !  XC_LDA_C_GL          =  5,  &  ! Gunnarson & Lundqvist
   !  XC_LDA_C_XALPHA      =  6,  &  ! Slaters Xalpha
   !  XC_LDA_C_VWN         =  7,  &  ! Vosko, Wilk, & Nussair
   !  XC_LDA_C_PZ          =  8,  &  ! Perdew & Zunger
   !  XC_LDA_C_OB_PZ       =  9,  &  ! Ortiz & Ballone (PZ)
   !  XC_LDA_C_PW          = 10,  &  ! Perdew & Wang
   !  XC_LDA_C_OB_PW       = 11,  &  ! Ortiz & Ballone (PW)
   !  XC_LDA_C_LYP         = 12,  &  ! Lee, Yang, & Parr LDA
   !  XC_LDA_C_AMGB        = 13,  &  ! Attacalite et al
   !  XC_LDA_C_KP          = 14      ! Kurth & Perdew
   !
   !call xc_lda_driver(E,k,3,2,1)
   !V_xc_n=V_xc(:,1)
   !
   ! XX 
   !==== 
   !
   if (l_sc_xx) call V_exx(X,E,k,q,V_xc_n)
   !
   ! Mixing 
   !========
   !
   if (it>1) V_xc_n = .3* V_xc_n + .7*V_xc_nm1 !linear mixing
   !
   ! Hartree potential
   !===================
   !
   if (it>1) call V_Hartree(rho_n,V_hr)
   !
   do ik = 1, QP_nk 
     !
     H_sc = Ho(:,:,ik)
     !
     ! Build H_sc
     !============
     !
     call V_to_H(ik,V_hr+V_xc_n,H_sc)
     !
     ! Diagonalize H_sc 
     !==================
     !
     call mat_dia_inv(DIAGO,USE_LK,H_sc,E_real=E%E(:,ik,1))
     !
     ! New wave functions 
     !====================
     !
     ! <r|n'>= <m|n'><r|m>
     !
     ! with <:|n'>=H_sc(:,n')
     !
     ! WS_wf(:,n')= sum_m wf(:,m) H_sc(m,n')
     !
     WS_wf = (0.,0.)
     !
     do ib  = 1,QP_nb
       ifft = wf_state(ib,ik,1)
       do ibp = 1,QP_nb
         ifftp = wf_state(ibp,ik,1)
         !
         call caxpy(fft_size,H_sc(ibp,ib),wf(:,ifftp),1,WS_wf(:,ib),1)
         !
       end do
     end do 
     !
     forall(ib = 1:QP_nb) wf(:,wf_state(ib,ik,1))= WS_wf(:,ib)
     !
     !  Transform Ho in the new basis 
     !
     !  <n'|H_o|k'>=sum_{m,p} <n'|m> H_o(m,p) <p|k'>
     !    
     !   (H_o)' = (H_sc^{\dagger}) H_o H_sc
     !
     ! [1] WS_Ho = (H_sc^{\dagger}) H_o 
     !     H_o   = WS_Ho
     !
#if defined _DOUBLE
     call zgemm('c',       'n',       QP_nb,QP_nb,QP_nb,(1._SP,0._SP),&
&               H_sc,QP_nb,Ho(:,:,ik),QP_nb,(0._SP,0._SP),WS_Ho,QP_nb)
#else
     call cgemm('c',       'n',       QP_nb,QP_nb,QP_nb,(1._SP,0._SP),&
&               H_sc,QP_nb,Ho(:,:,ik),QP_nb,(0._SP,0._SP),WS_Ho,QP_nb)
#endif
     Ho(:,:,ik) = WS_Ho(:,:)
     !
     ! [2] WS_Ho = H_o H_sc
     !     H_o   = WS_Ho
     !
#if defined _DOUBLE
     call zgemm('n',             'n',       QP_nb,QP_nb,QP_nb,(1._SP,0._SP),&
&               Ho(:,:,ik),QP_nb,H_sc,QP_nb,(0._SP,0._SP),WS_Ho,QP_nb)
#else
     call cgemm('n',             'n',       QP_nb,QP_nb,QP_nb,(1._SP,0._SP),&
&               Ho(:,:,ik),QP_nb,H_sc,QP_nb,(0._SP,0._SP),WS_Ho,QP_nb)
#endif
     Ho(:,:,ik) = WS_Ho(:,:)
     !
   end do !Exit loop on kpoints
   !
   it_done=it
   !
   ! New density 
   !
   rho_nm1 = rho_n 
   call el_density(E,k,rho_n,.FALSE.)
   !
   ! Convergences
   !
   E_convergence=0.
   do ik=1,k%nibz
     E_convergence = max( E_convergence,&
&                         maxval(abs(E%E(:SC_bands,ik,1)-E_previous(:,ik)-&
&                                    E%E(1,ik,1)+E_previous(1,ik)       )))
   enddo
   !
   rho_convergence = 0.
   do ir=1,fft_size
     rho_convergence = rho_convergence + abs(rho_n(ir)-rho_nm1(ir))/nel
   enddo
   !
   ! write scf status
   !
   call SC_mute("report io_in io_out")
   !
   dumb_ch='[SC] Iteration '//trim(intc(it))//' Accuracies rho*E :'//&
&          trim(real2ch(rho_convergence))//'*'//trim(real2ch(E_convergence*HARTREE))
   if (tty_size<0) call live_time_msg("n",dumb_ch,"","%s")
   if (tty_size>0) call live_time_msg("r",dumb_ch,"","%s")
   !
   if (E_convergence<SC_E_threshold.and.rho_convergence<SC_rho_threshold) exit
   !
   ! Linear mixing
   !
   rho_n = 0.3 * rho_n + 0.7 * rho_nm1 
   !
 end do
 !
 ! exit SCF cycle and report
 !
 call SC_mute("")
 call msg('nr','[SC] Iterations                     :',it_done)
 call msg('r' ,'[SC] Accuracy on density      :',rho_convergence)
 call msg('rn','[SC]          on energies [ev]:',E_convergence*HARTREE)
 !
 forall (ik=1:k%nibz,ib=1:SC_bands) dE(ib,ik)=E%E(ib,ik,1)-E_save(ib,ik)
 !
 do ik=1,k%nibz
   call report_energies(E%E(:SC_bands,:,:),k,SC_bands,(/ik,ik/),'E  ',.TRUE.)
   call report_energies(E_save+dE(1,1),k,SC_bands,(/ik,ik/),'E_o',.FALSE.)
   call report_energies(dE-dE(1,1),k,SC_bands,(/ik,ik/),'d_E',.FALSE.)
 enddo
 !
 deallocate(H_sc,Ho,rho_n,rho_nm1,V_xc_n,V_xc_nm1,V_hr,WS_Ho,WS_wf)
 call mem_est("Hs Potentials WS")
 !
end subroutine SC_driver

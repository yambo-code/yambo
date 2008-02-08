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
 use drivers,        ONLY:l_sc_xx,l_sc_chosex,l_sc_hartree,l_sc_pz,l_sc_lda_x,&
&                         l_sc_hf
 use electrons,      ONLY:levels,nel
 use R_lattice,      ONLY:bz_samp,qindx_S
 use D_lattice,      ONLY:Tel
 use QP_m,           ONLY:QP_state,QP_nb,QP_nk,QP_Vxc,QP_n_states,QP_table,&
&                         QP_ng_Sx,QP_bands_mixed,QP_Sx
 use SC,             ONLY:SC_bands, SC_iterations,SC_E_threshold,&
&                         SC_rho_threshold,SC_potential,it_now
 use wave_func,      ONLY:wf_load,wf,WF_rotate,wf_n_states
 use memory_m,       ONLY:mem_est
 use FFT_m,          ONLY:fft_size,fft_size
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK
 use com,            ONLY:msg
 use X_m,            ONLY:X_t
 use timing,         ONLY:live_time_msg
 use stderr,         ONLY:tty_size,intc,real2ch
 use xc_functionals, ONLY:V_xc
 use par_proc_m,     ONLY:master_node
 !
 implicit none
 type(levels) ::E
 type(bz_samp)::k,q
 type(X_t)    ::X
 !
 ! Iterations
 !
 integer                  :: it_on_disk,it
 real(SP)                 :: rho_convergence,E_convergence,E_save(SC_bands,k%nibz),&
&                            E_previous(SC_bands,k%nibz),dE(SC_bands,k%nibz),&
&                            n_weight
 real(SP),    allocatable :: rho_nm1(:),rho_n(:),V_xc_n(:),V_xc_nm1(:),V_hr(:)
 complex(SP), allocatable :: WFo(:,:)
 ! 
 ! Work Space
 !
 integer                  :: ir,i1,ik,ib,ibp,WF_G_max,WF_Go_indx
 complex(SP)              :: H_sc(SC_bands,SC_bands),Ho(SC_bands,SC_bands,k%nibz),&
&                            R(SC_bands,SC_bands,k%nibz)
 character(schlen)        :: dumb_ch
 logical                  :: local_V
 !
 it_on_disk = 1
 !
 l_sc_hartree =trim(SC_potential)=='HARTREE'
 l_sc_hf      =trim(SC_potential)=='HARTREE-FOCK'
 l_sc_xx      =trim(SC_potential)=='XX'
 l_sc_chosex  =trim(SC_potential)=='CHOSEX'
 l_sc_lda_x   =trim(SC_potential)=='LDA_X'
 l_sc_pz      =trim(SC_potential)=='PZ'
 !
 if (.not.any((/l_sc_hartree,l_sc_hf,l_sc_xx,l_sc_chosex,l_sc_lda_x,l_sc_pz/))) return
 !
 local_V=l_sc_xx.or.l_sc_hartree.or.l_sc_lda_x.or.l_sc_pz
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
 !
 if (l_sc_hf.or.l_sc_xx.or.l_sc_chosex) then
   WF_G_max=max(QP_ng_Sx,X%ng)
   WF_Go_indx=maxval(qindx_S(:,:,2))
 else
   WF_G_max=0
   WF_Go_indx=1
 endif
 call wf_load(WF_G_max,WF_Go_indx,(/1,SC_bands/),(/1,k%nibz/),title=' ')
 !
 ! Allocation(s)...
 !
 allocate(rho_n(fft_size),rho_nm1(fft_size),&
&         V_xc_n(fft_size),V_xc_nm1(fft_size),V_hr(fft_size))
 call mem_est("Potentials",(/5*fft_size/),(/SP/))
 !
 allocate(WFo(fft_size,wf_n_states))
 call mem_est("WF_save",(/fft_size*wf_n_states/))
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
   call V_to_H(ik,-V_hr,Ho(:,:,ik),wf)
 enddo
 !
 ! ... then remove Vxc[rho_0] also
 !=================================
 !
 do i1 = 1, QP_n_states
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   ik =QP_table(i1,3)
   !
   if (ib==ibp) Ho(ib,ibp,ik) = Ho(ib,ibp,ik) + E%E(ib,ik,1)
   !
   Ho(ib,ibp,ik) = Ho(ib,ibp,ik) - QP_Vxc(i1)
   !
   if (ib==ibp) Ho(ib,ibp,ik) = real(Ho(ib,ibp,ik))
   !
 end do
 deallocate(QP_Vxc)
 call mem_est("QP_Vxc")
 !
 ! SCF cycle
 !===========
 !
 V_xc_n   =(0.,0.)
 V_xc_nm1 =(0.,0.)
 !
 E_save=E%E(:SC_bands,:,1)
 WFo   =wf
 !
 do it = it_on_disk, SC_iterations
   !
   E_previous=E%E(:SC_bands,:,1)
   !
   it_now=it
   !
   n_weight=.3
   if (it>=10) n_weight=.2
   if (it>=50) n_weight=.1
   !
   ! Stop all messagging
   !
   call SC_mute("report io_in io_out")
   !
   if (local_V) then
     !
     if (l_sc_xx) call V_exx(X,E,k,q,V_xc_n)
     !
     if (l_sc_hartree) V_xc_n=(0.,0.)
     !
     if (l_sc_lda_x) call xc_lda_driver(E,k,1,1,1)
     if (l_sc_pz)    call xc_lda_driver(E,k,3,8,1)
     if (l_sc_pz.or.l_sc_lda_x) V_xc_n=V_xc(:,1)
     !
     if (it>1) then
       !
       ! Mixing 
       !========
       !
       V_xc_nm1=V_xc_n
       !
       V_xc_n = n_weight*V_xc_n + (1.-n_weight)*V_xc_nm1 
       !
     endif
     !
   else
     !
     ! Hartee-Fock
     !
     if (l_sc_hf) call QP_XX_Vxc(E,k,k,q)
     !
   endif
   !
   ! Hartree potential
   !===================
   !
   if (it>1) then
     !
     rho_n = n_weight*rho_n + (1.-n_weight)*rho_nm1
     !
     call V_Hartree(rho_n,V_hr)
     !
   endif
   !
   do ik = 1, QP_nk 
     !
     ! Build H_sc
     !============
     !
     if (local_V) then
       H_sc = Ho(:,:,ik)
       call V_to_H(ik,V_hr+V_xc_n,H_sc,WFo)
     else
       H_sc=(0.,0.)
       !
       ! Add Sigma_x
       !
       do i1 = 1, QP_n_states
         if (QP_table(i1,3)/=ik) cycle
         H_sc(QP_table(i1,1),QP_table(i1,2)) =  QP_Sx(i1)
       end do
       !
       if (it>1) call rotate_H(R(:,:,ik),H_sc,SC_bands,-1)
       H_sc = H_sc + Ho(:,:,ik)
       !
       ! Add V_Hartree
       !
       call V_to_H(ik,V_hr,H_sc,WFo)
       !
     endif
     !
     ! Diagonalize H_sc 
     !==================
     !
     call mat_dia_inv(DIAGO,USE_LK,H_sc,E_real=E%E(:SC_bands,ik,1))
     !
     ! Rotation (WFo->wf) matrix
     !
     R(:,:,ik)=H_sc
     !
     ! New wave functions 
     !====================
     !
     call WF_rotate(ik,SC_bands,R(:,:,ik),WFo,wf_out=wf)
     !
   end do
   !
   ! New density 
   !=============
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
   dumb_ch='[SC] Iteration '//trim(intc(it))//' Accuracies rho % E(ev) :'//&
&          trim(real2ch(rho_convergence))//' % '//trim(real2ch(E_convergence*HARTREE))
   !
   if (tty_size>0.or.it==1) then
     call live_time_msg("n",dumb_ch,"","%s")
   else
     if (local_V) then
       if (tty_size>0) call live_time_msg("r",dumb_ch,"","%s")
       if (tty_size<0) call live_time_msg("n",dumb_ch,"","%s")
     else
       call live_time_msg("n",dumb_ch,"","%s")
     endif
   endif
   !
   if (E_convergence<SC_E_threshold.and.rho_convergence<SC_rho_threshold) exit
   !
 end do
 !
 ! exit SCF cycle and report
 !
 call SC_mute("")
 call msg('nr','[SC] Iterations                     :',it_now)
 call msg('r' ,'[SC] Accuracy on density      :',rho_convergence)
 call msg('rn','[SC]          on energies [ev]:',E_convergence*HARTREE)
 !
 call fermi_level(E,k,Tel,1)
 !
 forall (ik=1:k%nibz,ib=1:SC_bands) dE(ib,ik)=E%E(ib,ik,1)-E_save(ib,ik)
 !
 do ik=1,k%nibz
   call report_energies(E%E(:SC_bands,:,:),k,SC_bands,(/ik,ik/),'E  ',.TRUE.)
   call report_energies(E_save,k,SC_bands,(/ik,ik/),'E_o',.FALSE.)
   call report_energies(dE,k,SC_bands,(/ik,ik/),'d_E',.FALSE.)
 enddo
 !
 deallocate(rho_n,rho_nm1,V_xc_n,V_xc_nm1,V_hr,WFo)
 call mem_est("Hs Potentials WF_save")
 !
end subroutine SC_driver

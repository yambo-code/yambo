!
! Copyright (C) 2000-2005 A. Marini, M. Gruening and the SELF team 
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
subroutine SC_driver(X,Xw,Xk,E,k,q)
 !
 use pars,           ONLY:SP,HARTREE,schlen
 use drivers,        ONLY:l_sc_exx,l_sc_chosex,l_sc_hartree,l_sc_pz,l_sc_lda_x,&
&                         l_sc_hf,l_sc_exxc
 use electrons,      ONLY:levels,nel,n_full_bands,n_met_bands
 use R_lattice,      ONLY:bz_samp,qindx_S,G_m_G
 use D_lattice,      ONLY:Tel
 use QP_m,           ONLY:QP_state,QP_nb,QP_nk,QP_Vxc,QP_n_states,QP_table,&
&                         QP_ng_Sx,QP_Sx,QP_t,QP_reset,CHOSEX_use_empties,QP_sc
 use SC,             ONLY:SC_bands,SC_iterations,SC_E_threshold,SC_kind,SC_xc_functional,&
&                         SC_rho_threshold,SC_potential,it_now,E_convergence,&
&                         rho_convergence,SC_R,V_xc_sc,local_V,H_nl_sc,H_rotate,SC_bands_mixed,&
&                         SC_cycle_mixing,V_hartree_sc,NSC_shot
 use wave_func,      ONLY:wf_load,wf,WF_rotate,wf_n_states
 use memory_m,       ONLY:mem_est
 use FFT_m,          ONLY:fft_size,fft_size
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK
 use com,            ONLY:msg,warning
 use X_m,            ONLY:X_t
 use timing,         ONLY:live_time_msg
 use stderr,         ONLY:tty_size,intc,real2ch
 use xc_functionals, ONLY:V_xc,XC_LDA_X,XC_LDA_C_PZ,XC_EXCHANGE_CORRELATION,XC_EXCHANGE
 use frequency,      ONLY:w_samp
 use IO_m,           ONLY:OP_RD,OP_RD_CL,RD_CL_IF_END,io_control,REP,VERIFY,&
&                         OP_WR,WR_CL_IF_END,NONE,WR,OP_WR_CL
 use global_XC,      ONLY:SC_EXX,SC_HF,SC_CHOSEX,SC_EXXC
 use vec_operate,    ONLY:degeneration_finder
 !
 implicit none
 type(levels) ::E
 type(bz_samp)::k,q,Xk
 type(X_t)    ::X(2)  ! Xx Em1s
 type(w_samp) ::Xw(2)
 !
 ! Iterations
 !
 integer                  :: it
 real(SP)                 :: E_bare(SC_bands,k%nibz),spread(SC_bands,k%nibz),&
&                            E_previous(SC_bands,k%nibz),dE(SC_bands,k%nibz)
 real(SP),    allocatable :: rho_n(:),V_xc_nm1(:),rho_nm1(:)
 complex(SP), allocatable :: WFo(:,:),QP_Sxc_nm1(:)
 ! 
 ! Work Space
 !
 integer                  :: ir,i1,i2,ik,ib,ibp,WF_G_max,WF_Go_indx,ID(3),io_E,io_V,io_WF,WF_bands
 real(SP)                 :: max_proj
 complex(SP)              :: Ho(SC_bands,SC_bands,k%nibz)
 character(schlen)        :: dumb_ch
 integer, external        :: io_SC_components,eval_G_minus_G
 logical, external        :: stop_now
 logical                  :: SC_cycle,RESTART
 type(QP_t)               :: qp_dummy
 !
 ! ws for states spread evaluation
 !
 real(SP)                 :: WS_spread(SC_bands)
 integer                  :: first_el(SC_bands),n_of_el(SC_bands),n_deg_grp,&
&                            b_of_max_proj(SC_bands,k%nibz)
 !
 call QP_reset(qp_dummy)
 !
 select case (trim(SC_potential))
   case('HARTREE')
     l_sc_hartree=.true.
     SC_kind=0
     SC_xc_functional=0
   case('HARTREE-FOCK')
     l_sc_hf=.true.
     SC_kind=SC_HF
     SC_xc_functional=0
   case('EXX')
     l_sc_exx=.true.
     SC_kind=SC_EXX
     SC_xc_functional=0
     if (n_full_bands/=n_met_bands) return
   case('EXXC')
     l_sc_exxc=.true.
     SC_kind=SC_EXXC
     SC_xc_functional=XC_LDA_C_PZ
     if (n_full_bands/=n_met_bands) return
   case('CHOSEX')
     l_sc_chosex=.true.
     SC_kind=SC_CHOSEX
     SC_xc_functional=0
     CHOSEX_use_empties=.FALSE.
   case('LDA_X')
     l_sc_lda_x=.true.
     SC_kind=XC_EXCHANGE
     SC_xc_functional=XC_LDA_X
   case('PZ')
     l_sc_pz=.true.
     SC_kind=XC_EXCHANGE_CORRELATION
     SC_xc_functional=XC_LDA_C_PZ
 end select
 !
 if (.not.any((/l_sc_hartree,l_sc_hf,l_sc_exx,l_sc_exxc,l_sc_chosex,l_sc_lda_x,l_sc_pz/))) return
 !
 local_V=l_sc_exx.or.l_sc_exxc.or.l_sc_hartree.or.l_sc_lda_x.or.l_sc_pz
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
 ! Reference energies
 !
 E_bare=E%E(:SC_bands,:,1)
 !
 call QP_state_table_setup(E)
 !
 ! Perform a first I/O to check which DBs are present.
 !
 SC_cycle=SC_iterations> 0
 RESTART=.false.
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(1)) ! Energies
 io_E=io_SC_components('E' ,E,ID(1))
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2))  ! WFs
 io_WF=io_SC_components('WF',E,ID(2))
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID(3)) ! Local potential
 io_V=io_SC_components('V' ,E,ID(3))
 !
 RESTART=all((/io_E==0,io_WF==0,io_V==0/))
 NSC_shot=SC_iterations==1.and.all((/io_E/=0,io_WF/=0,io_V==0.or.io_V==-2/)).and.local_V
 if (NSC_shot) SC_cycle=.false.
 !
 ! Self-Consistent run, Non-SC shot ot nothing ?
 !-----------------------------------------------
 !
 if (io_E==0.and.RESTART) then
   call msg('nr','[SC] Found   accuracy on density      :',rho_convergence)
   call msg('rn','[SC]                  on energies [ev]:',E_convergence*HARTREE)
   SC_cycle=.not.(E_convergence<SC_E_threshold.and.rho_convergence<SC_rho_threshold)
   !
   ! Even if the accuracy on DB is not small enough with 0 iterations
   ! I can skeep the SC cycle
   !
   if (SC_cycle) SC_cycle=SC_iterations>0
   !
 endif
 !
 if (.not.SC_cycle.and.RESTART) then
   !
   NSC_shot=.false.
   !
   ! load SC_R rotation matrices
   !-----------------------------
   allocate(SC_R(SC_bands,SC_bands,k%nibz))
   call mem_est("SC_R ",(/size(SC_R)/))
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2))
   io_WF=io_SC_components('WF',E,ID(2))
   if (io_WF==0) then
     do ik=1,QP_nk
       call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID(2))
       io_WF=io_SC_components('WF',E,ID(2))
     enddo
     call msg('r',' ')
   else
     return
   endif
   !
   goto 1
   !
 endif
 !
 if (.not.any((/RESTART,SC_cycle,NSC_shot/))) return
 !
 call msg('nr','[SC] Self-Consistency (SC)            :',SC_cycle)
 call msg('r' ,'[SC] Restart + SC                     :',RESTART)
 call msg('r' ,'[SC] Non SC single shot               :',NSC_shot)
 call msg('r' ,'[SC] Bands                            :',SC_bands)
 call msg('r' ,'[SC] Mixed bands                      :',SC_bands_mixed)
 if (SC_cycle) then
   call msg('r' ,'[SC] N (N-1) order mixing             :',SC_cycle_mixing)
   call msg('r' ,'[SC] Imposed accuracy on density      :',SC_rho_threshold)
   call msg('rn','[SC]                  on energies [ev]:',SC_E_threshold*HARTREE)
 endif
 !
 ! before loading the WF in R-space I check and/or calculate
 ! the optical oscillators that are done in G-space and would
 ! force the reloading of the WFs
 !
 if ((l_sc_exx.or.l_sc_exxc).and.SC_cycle) then
   X(1)%ib(2)=SC_bands
   X(1)%ng=QP_ng_Sx
   call X_O_strengths(E, Xk, X(1), X(1)%q0)
 endif
 !
 ! WF
 !
 ! x Local potentials (not including EXX)
 !
 WF_G_max=0
 WF_Go_indx=1
 WF_bands=SC_bands
 !
 if (SC_cycle.and.(l_sc_hf.or.l_sc_exx.or.l_sc_exxc.or.l_sc_chosex)) then
   WF_Go_indx=maxval(qindx_S(:,:,2)) ! Sigma_x/c
   if (l_sc_chosex) then
     X(2)%ng=eval_G_minus_G(X(2)%ng,0)
     WF_G_max=max(QP_ng_Sx,maxval(G_m_G))
   endif
   if (l_sc_exx.or.l_sc_exxc)  then
     WF_G_max=max(QP_ng_Sx,X(1)%ng)
     WF_bands=max(SC_bands,X(1)%ib(2))
   else if (l_sc_chosex)  then
     WF_G_max=max(QP_ng_Sx,X(2)%ng)
     WF_bands=max(SC_bands,X(2)%ib(2))
   endif
 endif
 call wf_load(WF_G_max,WF_Go_indx,(/1,WF_bands/),(/1,k%nibz/),title=' ')
 !
 ! Allocation(s)...
 !
 allocate(rho_n(fft_size),rho_nm1(fft_size),&
&         V_xc_sc(fft_size),V_xc_nm1(fft_size),V_hartree_sc(fft_size))
 call mem_est("Potentials",(/5*fft_size/),(/SP/))
 !
 allocate(WFo(fft_size,wf_n_states))
 call mem_est("WF_save",(/fft_size*wf_n_states/))
 !
 allocate(QP_Vxc(QP_n_states))
 call mem_est("QP_Vxc",(/QP_n_states/))
 !
 allocate(SC_R(SC_bands,SC_bands,k%nibz),H_nl_sc(SC_bands,SC_bands))
 call mem_est("SC_R H_nl_sc",(/size(SC_R),size(H_nl_sc)/))
 !
 if (.not.local_V) then
   allocate(QP_Sxc_nm1(QP_n_states))
   call mem_est("QP_Sxc_nm1",(/QP_n_states/))
 endif
 !
 if (l_sc_chosex) then
   allocate(QP_Sc(QP_n_states,1))
   call mem_est("QP_Sc",(/QP_n_states/))
 endif
 !
 !  Vxc[rho_0] and Vh[rho_0]
 !===========================
 !
 call DFT_Vxc(E,k)
 call el_density(E,k,rho_n,.FALSE.)
 call V_Hartree(rho_n,V_hartree_sc)
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
   call V_to_H(ik,-V_hartree_sc,Ho(:,:,ik),wf)
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
   if (ib==ibp) Ho(ib,ibp,ik) = Ho(ib,ibp,ik) + E_bare(ib,ik)
   !
   Ho(ib,ibp,ik) = Ho(ib,ibp,ik) - QP_Vxc(i1)
   !
   if (ib==ibp) Ho(ib,ibp,ik) = real(Ho(ib,ibp,ik))
   !
 end do
 deallocate(QP_Vxc)
 call mem_est("QP_Vxc")
 !
 ! Reference wavefunctions
 !
 WFo   =wf
 !
 ! Start-Up setup.  Eventually restart setup.
 !
 ! WFs
 !-----
 call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2))
 io_WF=io_SC_components('WF',E,ID(2))
 !
 if (RESTART) then
   !
   ! RESTART
   !
   do ik=1,QP_nk
     call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID(2))
     io_WF=io_SC_components('WF',E,ID(2))
     call WF_rotate(ik,SC_bands,fft_size,SC_R(:,:,ik),WFo,wf_out=wf)
   enddo
   !
   ! ... rho_n and V_hartree_sc
   !
   call el_density(E,k,rho_n,.FALSE.)
   call V_Hartree(rho_n,V_hartree_sc)
   !
   V_xc_nm1 = V_xc_sc
   rho_nm1  = rho_n
   !
 else if (NSC_shot) then
   !
   ! Non-SC shot
   !
   it_now=0
   ! ... reload again the scV DB to update the Hartree potential
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(3)) 
   io_V=io_SC_components('V' ,E,ID(3))
   !
 else
   !
   it_now=0
   !
   V_xc_sc  =(0.,0.)
   V_xc_nm1 =(0.,0.)
   !
 endif
 !
 ! SCF cycle & NSC shot
 !=====================
 !
 do it = 1, SC_iterations
   !
   E_previous=E%E(:SC_bands,:,1)
   !
   it_now=it_now+1
   !
   if (local_V.and.SC_cycle) then
     !
     if (l_sc_exx.or.l_sc_exxc) call V_exx(X,E,k,q,V_xc_sc)
     !
     if (l_sc_hartree) V_xc_sc=(0.,0.)
     !
     call SC_mute("report log io_in io_out")
     !
     if (l_sc_lda_x)            call xc_lda_driver(E,k,1,1,1)
     if (l_sc_pz.or.l_sc_exxc)  call xc_lda_driver(E,k,3,8,1)
     if (l_sc_pz.or.l_sc_lda_x) V_xc_sc=V_xc(:,1)
     if (l_sc_exxc) V_xc_sc=V_xc_sc+V_xc(:,1)
     !
     call SC_mute(" ")
     !
     ! Mixing 
     !========
     !
     if (it>1) V_xc_sc = SC_cycle_mixing*V_xc_sc + (1.-SC_cycle_mixing)*V_xc_nm1 
     !
     V_xc_nm1=V_xc_sc
     !
   else if (SC_cycle) then
     !
     ! Stop all messagging
     !
     call SC_mute("report io_in io_out")
     !
     ! Hartee-Fock
     !
     call QP_XX_Vxc(E,k,k,q)
     !
     call SC_mute("report io_out")
     !
     ! CHOSEX
     !
     if (l_sc_chosex) call QP_ppa_chosex(X(2),Xk,E,k,q,qp_dummy,Xw(2),-1)
     !
     call SC_mute(" ")
     !
     ! Mixing 
     !========
     !
     if (it>1) then
       if (l_sc_chosex) then
         forall (i1=1:QP_n_states)  QP_Sx(i1)=SC_cycle_mixing*QP_Sx(i1)+&
&                                             (1.-SC_cycle_mixing)/2.*QP_Sxc_nm1(i1)
         forall (i1=1:QP_n_states)  QP_Sc(i1,1)=SC_cycle_mixing*QP_Sc(i1,1)+&
&                                               (1.-SC_cycle_mixing)/2.*QP_Sxc_nm1(i1)
       else
         forall (i1=1:QP_n_states)  QP_Sx(i1)=SC_cycle_mixing*QP_Sx(i1)+&
&                                             (1.-SC_cycle_mixing)*QP_Sxc_nm1(i1)
       endif
     endif
     !
     forall (i1=1:QP_n_states)                  QP_Sxc_nm1(i1)= QP_Sx(i1)
     if (l_sc_chosex) forall (i1=1:QP_n_states) QP_Sxc_nm1(i1)= QP_Sxc_nm1(i1)+QP_sc(i1,1)
     !
   endif
   !
   ! Hartree potential
   !===================
   !
   if (it>1) then
     !
     rho_n = SC_cycle_mixing*rho_n + (1.-SC_cycle_mixing)*rho_nm1
     !
     call V_Hartree(rho_n,V_hartree_sc)
     !
   endif
   !
   ! IO [V_xc]: this is the only DB that must be written on-fly
   !            Both WFs and Energies are written ay the end of the loop
   !===========
   !
   if (     local_V) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(3))
   if (.not.local_V) call io_control(ACTION=OP_WR,   COM=NONE,SEC=(/1/),ID=ID(3))
   io_V=io_SC_components('V' ,E,ID(3))
   !
   do ik = 1, QP_nk 
     !
     ! Build H_nl_sc
     !===============
     !
     if (local_V) then
       !
       H_nl_sc = Ho(:,:,ik)
       call V_to_H(ik,V_hartree_sc+V_xc_sc,H_nl_sc,WFo)
       !
     else
       !
       H_nl_sc=(0.,0.)
       !
       ! Add Sigma_x and, eventually, Sigma_c (CHOSEX)
       !
       do i1 = 1, QP_n_states
         if (QP_table(i1,3)/=ik) cycle
         ib =QP_table(i1,1)
         ibp=QP_table(i1,2)
         H_nl_sc(ib,ibp) =  QP_Sx(i1)
         if (l_sc_chosex) H_nl_sc(ib,ibp) = H_nl_sc(ib,ibp) + QP_Sc(i1,1)
         !
         ! Force to be Hermitian
         !
         if (ib/=ibp) H_nl_sc(ibp,ib) = conjg(H_nl_sc(ib,ibp))
         !
       end do
       !
       ! Rotate H_nl_sc in the WFo basis.
       !
       if (it_now>1) call H_rotate(SC_R(:,:,ik),H_nl_sc,SC_bands,-1)
       !
       ! IO [H_nl_sc]
       !==============
       !
       call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID(3))
       io_V=io_SC_components('V',E,ID(3))
       !
       H_nl_sc = H_nl_sc + Ho(:,:,ik)
       !
       ! Add V_Hartree
       !
       call V_to_H(ik,V_hartree_sc,H_nl_sc,WFo)
       !
     endif
     !
     ! Diagonalize H_nl_sc 
     !=====================
     !
     call mat_dia_inv(DIAGO,USE_LK,H_nl_sc,E_real=E%E(:SC_bands,ik,1))
     !
     ! Rotation (WFo->wf) matrix
     !
     SC_R(:,:,ik)=H_nl_sc
     !
     ! New wave functions 
     !====================
     !
     call WF_rotate(ik,SC_bands,fft_size,SC_R(:,:,ik),WFo,wf_out=wf)
     !
   end do
   !
   if (SC_cycle) then
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
&                           maxval(abs(E%E(:SC_bands,ik,1)-E_previous(:,ik)-&
&                                      E%E(1,ik,1)+E_previous(1,ik)       )))
     enddo
     !
     rho_convergence = 0.
     do ir=1,fft_size
       rho_convergence = rho_convergence + abs(rho_n(ir)-rho_nm1(ir))/nel
     enddo
     !
     dumb_ch='[SC] Iteration '//trim(intc(it_now))//' Accuracies rho(%) - E(ev) :'//&
&            trim(real2ch(rho_convergence))//'% - '//trim(real2ch(E_convergence*HARTREE))
     !
     if (tty_size>0.or.it_now==1) then
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
   endif
   !
   ! IO [Energies]
   !===============
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(1))
   io_E=io_SC_components('E' ,E,ID(1))
   !
   !
   ! IO [SC_R]
   !===========
   !
   if (it==1) call io_control(ACTION=OP_WR,COM=REP, SEC=(/1/),ID=ID(2))
   if (it> 1) call io_control(ACTION=OP_WR,COM=NONE,SEC=(/1/),ID=ID(2))
   io_WF=io_SC_components('WF',E,ID(2))
   do ik = 1, QP_nk 
     call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID(2))
     io_WF=io_SC_components('WF',E,ID(2))
   enddo
   !
   if ( (E_convergence<SC_E_threshold.and.rho_convergence<SC_rho_threshold) .or.&
&       stop_now(.FALSE.) ) exit
   !
 end do
 !
 if (SC_cycle) then
   !
   ! exit SCF cycle and report
   !
   call msg('nr','[SC] Iterations               :',it_now)
   call msg('r' ,'[SC] Accuracy on density      :',rho_convergence)
   call msg('rn','[SC]          on energies [ev]:',E_convergence*HARTREE)
 else
   call msg('r','')
 endif
 !
1 continue
 !
 call fermi_level(E,k,Tel,1)
 !
 do ik=1,k%nibz
   !
   ! Check for degenrate bands in the bare energies
   !
   call degeneration_finder(E_bare(:,ik),SC_bands,first_el,n_of_el,n_deg_grp,0.0001/HARTREE)
   !
   do ib=1,SC_bands
     !
     ! Then search for the max projection   
     ! 
     max_proj=-1.
     do i1=1,SC_bands
       if (abs(SC_R(i1,ib,ik))**2.>max_proj) then
         b_of_max_proj(ib,ik)=i1
         max_proj=abs(SC_R(i1,ib,ik))**2.
       endif
     enddo
     ! 
     ! simmetrize the spreads summing over all possible degenerate
     ! bands
     !
     forall (ibp=1:SC_bands) WS_spread(ibp)=abs(SC_R(ibp,ib,ik))**2.*100./HARTREE
     !
     ibp=b_of_max_proj(ib,ik)
     spread(ib,ik)=WS_spread( ibp )
     !
     do i1=1,n_deg_grp
       if (ibp>=first_el(i1).and.ibp<=first_el(i1)+n_of_el(i1)-1) then
         spread(ib,ik)=0.
         do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
           spread(ib,ik)=spread(ib,ik)+WS_spread(i2)
         enddo
       endif
     enddo
   enddo
   !
   dE(:,ik)=E%E(:SC_bands,ik,1)-E_bare(:,ik)
   !
 enddo
 !
 do ik=1,k%nibz
   call report_energies(E%E(:SC_bands,:,:),k,SC_bands,(/ik,ik/),'E  ',.TRUE.)
   call report_energies(E_bare,k,SC_bands,(/ik,ik/),'E_o',.FALSE.)
   call report_energies(dE,k,SC_bands,(/ik,ik/),'d_E',.FALSE.)
   call report_energies(spread,k,SC_bands,(/ik,ik/),'Spr',.FALSE.)
   call report_energies(real(b_of_max_proj)/HARTREE,k,SC_bands,(/ik,ik/),'Bnd',.FALSE.)
 enddo
 !
 deallocate(SC_R)
 call mem_est("SC_R")
 if (SC_cycle.or.NSC_shot) then
   deallocate(rho_n,rho_nm1,V_xc_sc,V_xc_nm1,V_hartree_sc,WFo,H_nl_sc)
   call mem_est("Hs Potentials WF_save H_nl_sc")
   if (.not.local_V) then
     deallocate(QP_Sxc_nm1)
     call mem_est("QP_Sxc_nm1")
   endif
   if (l_sc_chosex) then
     deallocate(QP_Sc)
     call mem_est("QP_Sc")
   endif
 endif
 !
end subroutine SC_driver

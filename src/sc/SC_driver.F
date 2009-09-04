
! Copyright (C) 2000-2008 A. Marini, M. Gruening and the YAMBO team 
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
subroutine SC_driver(X,Xw,Xk,E,k,q)
 !
 use pars,           ONLY:SP,schlen
 use units,          ONLY:HARTREE
 use drivers,        ONLY:l_sc_exx,l_sc_chosex,l_sc_contains_lda,&
&                         l_sc_hf,l_sc_magnetic
 use electrons,      ONLY:levels,nel,n_met_bands,n_sp_pol,n_spin,spin
 use R_lattice,      ONLY:bz_samp,qindx_S,G_m_G
 use D_lattice,      ONLY:Tel
 use QP_m,           ONLY:QP_nk,QP_Sc,QP_n_states,QP_table,&
&                         QP_ng_Sx,QP_Sx,QP_t,QP_reset,SC_E_threshold,SC_bands_mixed
 use SC,             ONLY:SC_bands,SC_iterations,rho_n,&
&                         SC_rho_threshold,SC_potential,it_now,E_convergence,&
&                         rho_convergence,SC_R,V_xc_sc,local_V,H_nl_sc,H_rotate,&
&                         SC_cycle_mixing,V_hartree_sc,NSC_shot,SC_fft_size,        &
&                         SC_ng_Sx,SC_X_bands,SC_X_size,Ho,E_bare,SC_alloc,SC_free, &
&                         WF_G_max,WF_Go_indx,SC_kind,SC_xc_functional,l_use_RHO_IO,&
&                         G_lesser
 use wave_func,      ONLY:wf_load,wf,wf_n_states,WF_free
 use memory_m,       ONLY:mem_est
 use FFT_m,          ONLY:fft_size
 use parser_m,       ONLY:parser 
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK
 use com,            ONLY:msg,warning
 use X_m,            ONLY:X_t
 use timing,         ONLY:live_time_msg
 use stderr,         ONLY:tty_size,intc,real2ch
 use xc_functionals, ONLY:V_xc
 use frequency,      ONLY:w_samp
 use IO_m,           ONLY:OP_RD,OP_RD_CL,RD_CL_IF_END,io_control,REP,VERIFY,&
&                         OP_WR,WR_CL_IF_END,NONE,WR,OP_WR_CL
 use vec_operate,    ONLY:degeneration_finder
 use timing,         ONLY:live_timing
 use oscillators_IO, ONLY:oscillators_IO_alloc,oscillators_IO_free,ioRHO_RHO_VX,&
&                         V_kind
 use global_XC,      ONLY:SC_HF    
#if defined _MAGNETIC
 use magnetic,       ONLY:MAG_hamiltonian_type,MAG_B_z,eval_Lz
#endif
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
 real(SP)                 :: spread(SC_bands,k%nibz,n_sp_pol),&
&                            E_previous(SC_bands,k%nibz,n_sp_pol),dE(SC_bands,k%nibz,n_sp_pol),&
&                            E_conv(n_sp_pol)
 real(SP),    allocatable :: V_xc_nm1(:,:),rho_nm1(:)
 real(SP),    allocatable :: delta_rho(:)
 complex(SP), allocatable :: WFo(:,:)
 ! 
 ! Work Space
 !
 integer                  :: ir,i1,i2,ik,ib,ibp,ID(4),io_E,io_V,io_WF,i_spin
 real(SP)                 :: max_proj,V_xc_sc_shift(n_spin)
 character(schlen)        :: dumb_ch
 integer, external        :: io_SC_components,eval_G_minus_G
 logical, external        :: stop_now,XC_switch
 logical                  :: SC_cycle,RESTART
 logical                  :: l_TF_precond
 type(QP_t)               :: qp_dummy
 !
 ! ws for states spread evaluation
 !
 real(SP)                 :: WS_spread(SC_bands)
 integer                  :: first_el(SC_bands),n_of_el(SC_bands),n_deg_grp,&
&                            b_of_max_proj(SC_bands,k%nibz,n_sp_pol)
 !
 ! Extended Oscillators RHO_IO
 ! 
 integer                  :: io_RHO_err
 logical, allocatable     :: QP_DB_done(:)

 !
 ! Magnetic logical setup
 !
#if defined _MAGNETIC
 !
 call MAG_Hamiltonian(.TRUE.)
 !
#endif
 !
 ! Switch to the user defined potential (if appropriate)
 !
 if (.not.XC_switch()) return
 !
 ! Define SC internal variables
 !
 SC_ng_Sx  =QP_ng_Sx
 SC_X_bands=X(2)%ib
 SC_X_size =X(2)%ng
 !
 call QP_reset(qp_dummy)
 !
 if (trim(SC_potential)/='none') call section('*','Self-Consistent '//trim(SC_potential))
 !
 ! Reference energies
 !
 allocate(E_bare(SC_bands,k%nibz,n_sp_pol))
 call mem_est("E_bare ",(/size(E_bare)/))
 E_bare=E%E(:SC_bands,:,:)
 !
 ! Perform a first I/O to check which DBs are present. In this VERIFY call
 ! even if the fft_size is not set it is verified anyway as it is defined
 ! by the Sigma_x/Chi RL vectors.
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(1)) ! Energies
 io_E=io_SC_components('E' ,E,ID(1))
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2)) ! WFs
 io_WF=io_SC_components('WF',E,ID(2))
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID(3))  ! Local potential
 io_V=io_SC_components('V' ,E,ID(3))
 !
#if defined _MAGNETIC
 !
 ! precaution : disable RESTART and NSC_shot
 !
#endif
 !
 RESTART=all((/io_E==0,io_WF==0,io_V==0/)).and..not.l_sc_magnetic
 !
 NSC_shot=SC_iterations==1.and.all((/io_E/=0,io_WF/=0,io_V==0.or.io_V==-2/)).and.&
&                        local_V.and..not.l_sc_magnetic
 !
 SC_cycle=SC_iterations> 0
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
   allocate(SC_R(SC_bands,SC_bands,k%nibz,n_sp_pol))
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
 ! before loading the WF in R-space I check and/or calculate
 ! the optical oscillators that are done in G-space and would
 ! force the reloading of the WFs
 !
 if (l_sc_exx.and.SC_cycle) then
   X(1)%ib(2)=SC_bands
   X(1)%ng=QP_ng_Sx
   call Dipole_driver(E, Xk, X(1), X(1)%q0)
 endif
 !
 ! WF
 !
 ! x Local potentials (not including EXX)
 !
 WF_G_max=0
 WF_Go_indx=1
 !
 if (SC_cycle.and.(l_sc_hf.or.l_sc_exx.or.l_sc_chosex)) then
   WF_Go_indx=maxval(qindx_S(:,:,2)) ! Sigma_x/c
   if (l_sc_chosex) then
     X(2)%ng=eval_G_minus_G(X(2)%ng,0)
     WF_G_max=max(QP_ng_Sx,maxval(G_m_G))
   endif
   if (l_sc_exx)  then
     WF_G_max=max(QP_ng_Sx,X(1)%ng)
   endif
 endif
 !
#if defined _MAGNETIC
 !
 if (l_sc_magnetic) then
   !
   ! Now SC_add_magnetic evaluates the derivative,
   ! does the fft of the derivative. Then call WF_free and load the wf in
   ! real space, compute the magnetic part of the hamiltonian and, 
   ! allocate and initialise Ho, write Hmagnetic in Ho 
   !
   call MAG_Hamiltonian(.FALSE.)
   !
 endif
 !
#endif
 !
 call wf_load(WF_G_max,WF_Go_indx,(/1,max(SC_bands,E%nbm)/),(/1,k%nibz/),title='-SC')
 !
 if (NSC_shot.and.SC_fft_size/=fft_size) then
   call warning(' FFT size of V_xc does not fit. Skipping NSC run')
   call WF_free()
   return
 endif
 !
 SC_fft_size=fft_size
 call parser('TF_precondition',l_TF_precond) 
 call parser('UseRhoIO',l_use_RHO_IO) 
 !
 ! Allocation(s)...
 !
 call SC_alloc(E)
 !
 ! Initialize SC_R matrix
 !
 if(.not.RESTART.and.l_use_RHO_IO) then
   !      
   SC_R=(0.,0.)
   forall(i1=1:SC_bands)
     SC_R(i1,i1,:,:)=1.
   end forall
   !
 endif
 ! 
 !
 ! Checking extended oscillators
 !
 if(l_use_RHO_IO) then
   !
   V_kind=-1
   if(l_sc_hf) V_kind=SC_HF
   !
   call io_control(ACTION=OP_RD_CL,SEC=(/1/),MODE=VERIFY,ID=ID(4)) 
   allocate(QP_DB_done(QP_n_states)) 
   io_RHO_err=ioRHO_RHO_VX(0,ID(4),QP_DB_done)
   !
   if(io_RHO_err/=0.or..not.all(QP_DB_done)) then
      l_use_RHO_IO=.false.
      call warning(' Extented Oscillators do not fit, UseRhoIO set to .false. ')
   endif
   deallocate(QP_DB_done)
   !
 endif
 !                       
 ! Local allocation(s)...
 !
 call SC_local_alloc()
 !
 call msg('nr','[SC] Self-Consistency (SC)            :',SC_cycle)
 call msg('r' ,'[SC] Restart + SC                     :',RESTART)
 call msg('r' ,'[SC] Non SC single shot               :',NSC_shot)
 call msg('r' ,'[SC] Bands                            :',SC_bands)
 call msg('r' ,'[SC] Mixed bands                      :',SC_bands_mixed)
 call msg('r' ,'[SC] Use Extented Oscillators         :',l_use_RHO_IO)
 call msg('r' ,'[SC] Use Thomas-Fermi precondition    :',l_TF_precond) 
 if (SC_cycle) then
   call msg('r' ,'[SC] N (N-1) order mixing             :',SC_cycle_mixing)
   call msg('r' ,'[SC] Imposed accuracy on density      :',SC_rho_threshold)
   call msg('rn','[SC]                  on energies [ev]:',SC_E_threshold*HARTREE)
 endif
 !
 !  Ho (kinetic + potential)
 !---------------------------
 !
 call SC_common_build_Ho(E,Xk,k)
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
 if (RESTART.or.NSC_shot) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID(3))  ! Local potential
   io_V=io_SC_components('V' ,E,ID(3))
 endif
 !
 if (RESTART) then
   !
   if ((1.-SC_cycle_mixing)>1.E-5) call &
&    warning('Short splitted runs with fractional mixing may give different results')
   !
   ! RESTART
   !
   do ik=1,QP_nk
     call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID(2))
     io_WF=io_SC_components('WF',E,ID(2))
     call WF_rotate(ik,WFo,fft_size)
   enddo
   !
   ! ... rho_n and V_hartree_sc
   !
   call el_density(E,k,rho_n,.false.)
   call V_Hartree(rho_n,V_hartree_sc)
   !
   V_hartree_sc(:,n_sp_pol)= V_hartree_sc(:,1)
   !
   V_xc_nm1 = V_xc_sc
   rho_nm1  = rho_n
   !
 else if (NSC_shot) then
   !
   ! Non-SC shot
   !
   it_now=0
   !
 else
   !
   it_now=0
   !
   V_xc_sc  =0.
   V_xc_nm1 =0.
   !
 endif
 !
 ! SCF cycle & NSC shot
 !=====================
 ! 
 if (NSC_shot) call live_timing('[NSC] Single shot loop',QP_nk,SERIAL=.true.)
 !
 do it = 1, SC_iterations
   !
   E_previous(:,:,:) = E%E(:SC_bands,:,:)
   !
   it_now=it_now+1
   !
   if (local_V.and.SC_cycle) then
     !
     V_xc_sc=(0.,0.)
     !
     if (l_sc_exx) call V_exx(X,E,k,q,V_xc_sc)
     !
     call IO_mute("report log io_in io_out")
     !
     if (l_sc_contains_lda) call xc_lda_driver(E,Xk,SC_KIND,SC_xc_functional,1)
     if (l_sc_contains_lda.and..not.l_sc_exx) V_xc_sc(:,:)=V_xc(:,:)
     if (l_sc_contains_lda.and.     l_sc_exx) V_xc_sc(:,:)=V_xc(:,:)+V_xc_sc(:,:)
     !
     call IO_mute(" ")
     !
     ! Mixing 
     !========
     !
     !WARNING:
     !The mixing of the potential has been eliminated for HF and CHOSEX, for
     !V_xc_sc case tests are needed.
     ! 
     if (it>1) V_xc_sc = SC_cycle_mixing*V_xc_sc + (1.-SC_cycle_mixing)*V_xc_nm1 
     !
     V_xc_nm1=V_xc_sc
     !
   else if (SC_cycle) then
     !
     ! Hartee-Fock
     !
     if(l_use_RHO_IO) then
       !     
       G_lesser=(0.,0.)
       forall(i1=1:SC_bands,ik=1:E%nk)
         G_lesser(i1,i1,ik)=(0._SP,1._SP)*E%f(i1,ik,1)
       end forall
       !
       ! Now I build the G_lesser rotating the non interaction one
       ! G_lesser(i1,i1,ik)=(0.,1)*en%f(i1,ik)
       !  
       do ik=1,QP_nk
         call H_rotate(SC_R(:,:,ik,1),G_lesser(:,:,ik),SC_bands,-1)
       enddo
       call SC_extended_oscillators(G_lesser)
       !
     endif
     !
     ! Stop all messagging
     !
     call IO_mute("report io_in io_out")
     !
     if (.not.l_use_RHO_IO) call XCo_driver(E,k,k,q)
     !
     call IO_mute("report io_out")
     !
     ! CHOSEX
     !
     if (l_sc_chosex) call QP_ppa_chosex(X(2),Xk,E,k,q,qp_dummy,Xw(2),-1)
     !
     call IO_mute(" ")
     !
   endif
   !
   ! Hartree potential
   !===================
   !
   if (it>1) then
     !
     if (.not.l_TF_precond) then 
       rho_n = SC_cycle_mixing*rho_n + (1.-SC_cycle_mixing)*rho_nm1
     else
       delta_rho=rho_n-rho_nm1
       !
       call SC_precondition(delta_rho)
       !
       rho_n=rho_nm1+SC_cycle_mixing*delta_rho
     endif
     !
     call V_Hartree(rho_n,V_hartree_sc)
     V_hartree_sc(:,n_sp_pol)=V_hartree_sc(:,1)
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
       H_nl_sc(:,:,:) = Ho(:,:,ik,:)
       call V_real_space_to_H(ik,V_hartree_sc+V_xc_sc,H_nl_sc,WFo)
       !
     else
       !
       H_nl_sc=(0.,0.)
       !
       ! Add Sigma_x and, eventually, Sigma_c (CHOSEX)
       !
       if(.not.l_use_RHO_IO) call V_qp_basis_to_H(ik,QP_Sx,H_nl_sc)
       !        
       if (l_sc_chosex) call V_qp_basis_to_H(ik,QP_Sc,H_nl_sc)
       !
       ! Rotate H_nl_sc in the WFo basis.
       !
       if (it_now>1) then
         do i_spin=1,n_sp_pol
           call H_rotate(SC_R(:,:,ik,i_spin),H_nl_sc(:,:,i_spin),SC_bands,-1)
         end do
       endif
       !
       if(l_use_RHO_IO) call V_qp_basis_to_H(ik,QP_Sx,H_nl_sc)
       !
       ! IO [H_nl_sc]
       !==============
       !
       call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID(3))
       io_V=io_SC_components('V',E,ID(3))
       !
       H_nl_sc = H_nl_sc + Ho(:,:,ik,:)
       !
       ! Add V_Hartree
       !
       call V_real_space_to_H(ik,V_hartree_sc,H_nl_sc,WFo)
       !
     endif
     !
     ! Diagonalize H_nl_sc 
     !=====================
     !
     do i_spin=1,n_sp_pol
       call mat_dia_inv(DIAGO,USE_LK,H_nl_sc(:,:,i_spin),&
                         &E_real=E%E(:SC_bands,ik,i_spin))
       !
       ! Rotation (WFo->wf) matrix
       !
       SC_R(:,:,ik,i_spin)=H_nl_sc(:,:,i_spin)
     enddo
     !
     ! New wave functions 
     !====================
     !
     call WF_rotate(ik,WFo,fft_size)
     !
     if (NSC_shot) call live_timing(steps=1)
     !
   end do
   !
   if (SC_cycle) then
     !
     ! New occupations 
     !=================
     !
     if (E%nbm/=E%nbf) call fermi_level(E,k,Tel,1) 
     !
     ! New density 
     !=============
     !
     rho_nm1 = rho_n 
     !
     call el_density(E,k,rho_n,.false.)
     !
     ! Convergences
     !==============
     !
     E_convergence=0.
     E_conv=0.
     do ik=1,k%nibz
       do i_spin=1,n_sp_pol
         E_conv(i_spin) = max( E_conv(i_spin),&
&                         maxval(abs(E%E(:SC_bands,ik,i_spin)-E_previous(:,ik,i_spin)-&
&                                    E%E(1,ik,i_spin)+E_previous(1,ik,i_spin)       )))
       enddo
       E_convergence=max(E_convergence,maxval(E_conv))
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
&       stop_now(.FALSE.) ) then
#if defined _MAGNETIC
     if(eval_Lz) call MAG_eval_Lz(2)
#endif
     exit
   endif
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
   !
 else
   call msg('r','')
 endif
 !
 if (NSC_shot) call live_timing(steps=1)
 !
1 continue
 !
 ! Before all the DFT energies are shifted the V_xc_sc
 ! must be shifted of max(Ev)-max(E_bare_v)
 ! for more details see A. Fleszar PRB 64, 245204
 !
 do i_spin=1,n_sp_pol
   V_xc_sc_shift(i_spin )= maxval(E_bare(:n_met_bands,:,i_spin))&
                        &-maxval(E%E(:n_met_bands,:,i_spin)) 
   if (local_V) then
     V_xc_sc(:,i_spin)=V_xc_sc(:,i_spin)+V_xc_sc_shift(i_spin)
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(3))
     io_V=io_SC_components('V' ,E,ID(3))
   endif
 enddo
 !
 do i_spin=1,n_sp_pol
   do ik=1,k%nibz
     !
     ! Check for degenrate bands in the bare energies
     !
     call degeneration_finder(E_bare(:,ik,i_spin),SC_bands,first_el,n_of_el,n_deg_grp,0.0001/HARTREE)
     !
     do ib=1,SC_bands
       !
       ! Then search for the max projection   
       ! 
       max_proj=-1.
       do i1=1,SC_bands
         if (abs(SC_R(i1,ib,ik,i_spin))**2.>max_proj) then
           b_of_max_proj(ib,ik,i_spin)=i1
           max_proj=abs(SC_R(i1,ib,ik,i_spin))**2.
         endif
       enddo
       ! 
       ! simmetrize the spreads summing over all possible degenerate
       ! bands
       !
       forall (ibp=1:SC_bands) WS_spread(ibp)=abs(SC_R(ibp,ib,ik,i_spin))**2.*100./HARTREE
       !
       ibp=b_of_max_proj(ib,ik,i_spin)
       spread(ib,ik,i_spin)=WS_spread( ibp )
       !
       do i1=1,n_deg_grp
         if (ibp>=first_el(i1).and.ibp<=first_el(i1)+n_of_el(i1)-1) then
           spread(ib,ik,i_spin)=0.
           do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
             spread(ib,ik,i_spin)=spread(ib,ik,i_spin)+WS_spread(i2)
           enddo
         endif
       enddo
     enddo
     !
     ! end do SC_bands
     !
     dE(:,ik,i_spin)=E%E(:SC_bands,ik,i_spin)-E_bare(:,ik,i_spin)
     !
   enddo
   !
 enddo
 !
 !
 ! Final occupations 
 !====================
 if (SC_cycle) call fermi_level(E,k,Tel,2)  
 !
 dE(:,:,:)=E%E(:SC_bands,:,:)-E_bare(:,:,:)
 !
 call msg('r',' ')
 !
 do ik=1,k%nibz
   call report_energies(E%E(:SC_bands,:,:),k,Xk,SC_bands,(/ik,ik/),'E  ',.TRUE.)
   call report_energies(E_bare,k,Xk,SC_bands,(/ik,ik/),'E_o',.FALSE.)
   call report_energies(dE,k,Xk,SC_bands,(/ik,ik/),'d_E',.FALSE.)
   call report_energies(spread,k,Xk,SC_bands,(/ik,ik/),'Spr',.FALSE.)
   call report_energies(real(b_of_max_proj)/HARTREE,k,Xk,SC_bands,(/ik,ik/),'Bnd',.FALSE.)
 enddo
 !
 deallocate(E_bare)
 call mem_est("E_bare")
 if (SC_cycle.or.NSC_shot) then
   call WF_free()
   call SC_free()
   call SC_local_free()
 endif
 !
 contains
   !
   subroutine SC_local_alloc()
     !
     allocate(rho_nm1(fft_size))
     call mem_est("rho_nm1",(/fft_size/),(/SP/))
     !
     allocate(V_xc_sc(fft_size,n_spin))
     call mem_est("V_xc_sc",(/size(V_xc_sc)/))
     !
     allocate(V_xc_nm1(fft_size,n_spin))
     call mem_est("V_xc_nm1",(/size(V_xc_nm1)/))
     !
     if (l_TF_precond) then
       allocate(delta_rho(fft_size))
       call mem_est("delta_rho",(/fft_size/),(/SP/))
     endif
     !
     allocate(WFo(fft_size,wf_n_states))
     call mem_est("WFo",(/fft_size*wf_n_states/))
     !
     if(l_use_RHO_IO) then
       !
       allocate(G_lesser(SC_bands,SC_bands,QP_nk))
       call mem_est("G_lesser",(/size(G_lesser)/),(/2*SP/))
       !
       allocate(QP_Sx(QP_n_states))
       call mem_est("QP_Sx",(/size(QP_Sx)/),(/2*SP/))
       !
       call oscillators_IO_alloc()
       !
     endif
     !
   end subroutine
   !
   subroutine SC_local_free()
     !
     deallocate(rho_nm1)
     call mem_est("rho_nm1")
     !
     deallocate(V_xc_sc)
     call mem_est("V_xc_sc")
     !
     deallocate(V_xc_nm1)
     call mem_est("V_xc_nm1")
     !
     if (l_TF_precond) deallocate(delta_rho)
     call mem_est("delta_rho")
     !
     deallocate(WFo)
     call mem_est("WFo")
     !
     if(l_use_RHO_IO) then
       !
       deallocate(G_lesser)
       call mem_est("G_lesser")
       !
       deallocate(QP_Sx)
       call mem_est("QP_Sx")
       !
       call oscillators_IO_free()
       !
     endif
     !
   end subroutine
   !
end subroutine SC_driver

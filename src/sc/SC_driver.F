!
! Copyright (C) 2000-2010 A. Marini, M. Gruening and the YAMBO team 
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
 use drivers,        ONLY:l_sc_cohsex,l_collisions_IO,l_sc_contains_lda
 use electrons,      ONLY:levels,nel,n_met_bands,n_sp_pol,n_spin,spin
 use R_lattice,      ONLY:bz_samp
 use QP_m,           ONLY:QP_nk,QP_Sc,QP_n_states,QP_Vnl_xc,SC_E_threshold
 use SC,             ONLY:SC_bands,SC_iterations,rho_n,SC_rho_threshold,it_now,E_convergence, &
&                         rho_convergence,SC_R,V_xc_sc,local_V,H_nl_sc,H_rotate,&
&                         l_SC_cycle,SC_cycle_mixing,V_hartree_sc,Ho,E_bare,SC_alloc,SC_free, &
&                         WF_G_max,WF_Go_indx,SC_kind,SC_xc_functional,G_lesser,l_SC_RESTART,l_NSC_shot,l_TF_precond
 use wave_func,      ONLY:wf,wf_n_states,WF_free
 use memory_m,       ONLY:mem_est
 use FFT_m,          ONLY:fft_size
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK
 use com,            ONLY:msg,warning
 use X_m,            ONLY:X_t
 use timing,         ONLY:live_time_msg
 use stderr,         ONLY:tty_size,intc,real2ch
 use xc_functionals, ONLY:V_xc,E_xc
 use frequency,      ONLY:w_samp
 use IO_m,           ONLY:OP_RD,RD_CL_IF_END,io_control,REP,VERIFY,&
&                         OP_WR,WR_CL_IF_END,NONE,WR,OP_WR_CL,OP_RD_CL
 use vec_operate,    ONLY:degeneration_finder
 use timing,         ONLY:live_timing
 use collisions_IO,  ONLY:collisions_IO_alloc,collisions_IO_free
#if defined _MAGNETIC
 use magnetic,       ONLY:average_el_mag
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
 real(SP)                 :: spread(SC_bands(2),k%nibz,n_sp_pol),&
&                            E_previous(SC_bands(2),k%nibz,n_sp_pol),dE(SC_bands(2),k%nibz,n_sp_pol),&
&                            E_conv(n_sp_pol),E_total,E_tot_Hartree,E_tot_xc,max_overlap(SC_bands(2),k%nibz,n_sp_pol)
 real(SP),    allocatable :: V_xc_nm1(:,:),rho_nm1(:)
 real(SP),    allocatable :: delta_rho(:)
 complex(SP), allocatable :: WFo(:,:)
 ! 
 ! Work Space
 !
 integer                  :: ir,i1,i2,ik,ib,ibp,ID(4),io_E,io_V,io_WF,i_spin,io_err
 real(SP)                 :: max_proj,V_xc_sc_shift(n_spin)
 character(schlen)        :: dumb_ch
 integer, external        :: io_SC_components
 logical, external        :: stop_now
 !
 ! ws for states spread evaluation
 !
 real(SP)                 :: WS_spread(SC_bands(2))
 integer                  :: first_el(SC_bands(2)),n_of_el(SC_bands(2)),n_deg_grp,&
&                            b_of_max_proj(SC_bands(2),k%nibz,n_sp_pol)
 !
 ! Initial Start & Restart 
 !==========================
 ! 
 call SC_start_and_restart(X,E,Xk)
 !
 if (.not.any((/l_SC_RESTART,l_SC_cycle,l_NSC_shot/))) return
 !
 ! Allocation(s)...
 !==================
 !
 call SC_alloc(E)
 call SC_local_alloc()
 !
 ! COLLISIONS
 !============
 !
 if (l_collisions_IO) call COLLISIONS_IO_load(.FALSE.)
 !
 ! Initialize SC_R matrix
 !========================
 !
 if(.not.l_SC_RESTART.and.l_collisions_IO) then
   !      
   SC_R=(0.,0.)
   forall(i1=1:SC_bands(2))
     SC_R(i1,i1,:,:)=1.
   end forall
   !
 else if (.not.l_SC_cycle.and.l_SC_RESTART) then
   !
   ! load SC_R rotation matrices
   !-----------------------------
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2))
   io_WF=io_SC_components('WF',E,ID(2))
   do ik=1,QP_nk
     call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID(2))
     io_WF=io_SC_components('WF',E,ID(2))
   enddo
   call msg('r',' ')
   !
   call SC_report()
   return
   !
 endif
 !
 !  Ho (kinetic + potential)
 !---------------------------
 !
 call msg('r','')
 call SC_bare_Hamiltonian(E,Xk,k)
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
 if (l_SC_RESTART.or.l_NSC_shot) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID(3))  ! Local potential
   io_V=io_SC_components('V' ,E,ID(3))
 endif
 !
 if (l_SC_RESTART) then
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
#if defined _MAGNETIC
   !
   ! and spin density
   !
   if(n_sp_pol>1) call spin_density(E,k)
   !
#endif
   !
   V_hartree_sc(:,n_sp_pol)= V_hartree_sc(:,1)
   !
   V_xc_nm1 = V_xc_sc
   rho_nm1  = rho_n
   !
 else if (l_NSC_shot) then
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
 if (l_NSC_shot) call live_timing('[NSC] Single shot loop',QP_nk,SERIAL=.true.)
 !
 do it = 1, SC_iterations
   !
   E_previous(:,:,:) = E%E(:SC_bands(2),:,:)
   !
   it_now=it_now+1
   !
   ! Exchange & Correlation
   !========================
   !
   if (l_SC_cycle) call SC_add_XC(X,Xw,Xk,E,k,q,it,V_xc_nm1)
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
       ! Add Sigma_x and, eventually, Sigma_c (COHSEX)
       !
       call V_qp_basis_to_H(ik,QP_Vnl_xc,H_nl_sc)
       !
       if(l_sc_cohsex) call V_qp_basis_to_H(ik,QP_Sc,H_nl_sc)
       !
       if (.not.l_collisions_IO.and.it_now>1) then
         !
         ! Rotate H_nl_sc in the WFo basis.
         !
         do i_spin=1,n_sp_pol
           call H_rotate(SC_R(:,:,ik,i_spin),H_nl_sc(:,:,i_spin),SC_bands(2),-1)
         end do
       endif
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
       !
       call mat_dia_inv(DIAGO,USE_LK,H_nl_sc(:,:,i_spin),E_real=E%E(:SC_bands(2),ik,i_spin))
       !
       ! Rotation (WFo->wf) matrix
       !
       SC_R(:,:,ik,i_spin)=H_nl_sc(:,:,i_spin) 
       !
     enddo
     !
     ! New wave functions 
     !====================
     !
     call WF_rotate(ik,WFo,fft_size)
     !
     if (l_NSC_shot) call live_timing(steps=1)
     !
   end do
   !
   if(l_NSC_shot) call OCCUPATIONS_Fermi(E,k,1) 
   !
   if (l_SC_cycle) then
     !
     ! New occupations 
     !=================
     !
     ! 21/01/2010 (Andrea): All bands above SC_bands(2) must be shifted
     ! in order to be consistent with the SC values. Otherwise these
     ! bands could interfere with the SC_bands(2) yielding non physical
     ! occupations.
     !
     forall (ib=SC_bands(2)+1:E%nb) E%E(ib,:,:)=E%E(ib,:,:)+E%E(SC_bands(2),:,:)-E_previous(SC_bands(2),:,:)
     !
     ! The same for the bands below SC_lw_band
     !
     forall (ib=1:SC_bands(1)-1) E%E(ib,:,:)=E%E(ib,:,:)+E%E(SC_bands(1),:,:)-E_previous(SC_bands(1),:,:)
     !
     call OCCUPATIONS_Fermi(E,k,1) 
     !
     ! New density 
     !=============
     !
     rho_nm1 = rho_n 
     !
     call el_density(E,k,rho_n,.false.)
     !
     ! Total Energy 
     !==============
     !
     call SC_Energy(E_total,E_tot_Hartree,E_tot_xc,E,k,rho_n)
     !
#if defined _MAGNETIC
     ! New spin density 
     !===================
     !
     if(n_sp_pol>1) call spin_density(E,k)
#endif
     ! Convergences
     !==============
     !
     E_convergence=0.
     E_conv=0.
     do ik=1,k%nibz
       do i_spin=1,n_sp_pol
         E_conv(i_spin) = max( E_conv(i_spin),&
&                         maxval(abs(E%E(:SC_bands(2),ik,i_spin)-E_previous(:,ik,i_spin)-&
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
     if (l_sc_contains_lda.and. local_V) dumb_ch=trim(dumb_ch)//'  E_tot(Ha)= '//trim(real2ch(E_total))
#if defined _MAGNETIC
     if(n_sp_pol>1) dumb_ch=trim(dumb_ch)//'  (Mz='//trim(real2ch(average_el_mag(3)))//')'
#endif
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
     ! History
     !=========
     !
     call SC_History(it,E)
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
 call SC_History(-1,E)
 !
 ! Hartree and xc Energy 
 !=======================
 !
 call SC_Energy(E_total,E_tot_Hartree,E_tot_xc,E,k,rho_n)
 !
 if (l_SC_cycle) then
   !
   ! exit SCF cycle and report
   !
   call msg('nr','[SC] Iterations               :',it_now)
   call msg('r' ,'[SC] Accuracy on density      :',rho_convergence)
   call msg('r' ,'[SC]          on energies [ev]:',E_convergence*HARTREE)
   if (l_sc_contains_lda.and. local_V) then
     call msg('r' ,'[SC] Total Energy (no Ewald) [Ha/Ry]:',E_total*(/1.,2./))
     call msg('r' ,'[SC] Hartree Energy          [Ha/Ry]:',E_tot_Hartree*(/1.,2./))
     call msg('rn','[SC] xc Energy               [Ha/Ry]:',E_tot_xc*(/1.,2./))
   else
     call msg('r','')
   endif
   !
 else
   call msg('r','')
 endif
 !
 if (l_NSC_shot) call live_timing(steps=1)
 !
 call SC_report()
 !
 deallocate(E_bare)
 call mem_est("E_bare")
 if (l_SC_cycle.or.l_NSC_shot) then
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
     allocate(E_xc(fft_size))
     call mem_est("E_xc",(/fft_size/),(/SP/))
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
     if(l_collisions_IO) then
       !
       allocate(G_lesser(SC_bands(2),SC_bands(2),QP_nk))
       call mem_est("G_lesser",(/size(G_lesser)/),(/2*SP/))
       !
       allocate(QP_Vnl_xc(QP_n_states))
       call mem_est("QP_Vnl_xc",(/size(QP_Vnl_xc)/),(/2*SP/))
       !
       call collisions_IO_alloc()
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
     deallocate(E_xc)
     call mem_est("E_xc")
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
     if(l_collisions_IO) then
       !
       deallocate(G_lesser)
       call mem_est("G_lesser")
       !
       if (allocated(QP_Vnl_xc)) deallocate(QP_Vnl_xc)
       call mem_est("QP_Vnl_xc")
       !
       call collisions_IO_free()
       !
     endif
     !
   end subroutine
   !
   subroutine SC_report()
     !     
     ! Before all the DFT energies are shifted the V_xc_sc
     ! must be shifted of max(Ev)-max(E_bare_v)
     ! for more details see A. Fleszar PRB 64, 245204
     !
     if (l_SC_cycle) then
       do i_spin=1,n_sp_pol
         V_xc_sc_shift(i_spin )= maxval(E_bare(:n_met_bands,:,i_spin))-maxval(E%E(:n_met_bands,:,i_spin)) 
         if (local_V) then
           V_xc_sc(:,i_spin)=V_xc_sc(:,i_spin)+V_xc_sc_shift(i_spin)
           call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(3))
           io_V=io_SC_components('V' ,E,ID(3))
         endif
       enddo
     endif
     !
     do i_spin=1,n_sp_pol
       do ik=1,k%nibz
         !
         ! Check for degenrate bands in the bare energies
         !
         call degeneration_finder(E_bare(:,ik,i_spin),SC_bands(2),first_el,n_of_el,n_deg_grp,0.0001/HARTREE)
         !
         do ib=1,SC_bands(2)
           !
           ! Then search for the max projection   
           ! 
           max_proj=-1.
           do i1=1,SC_bands(2)
             if (abs(SC_R(i1,ib,ik,i_spin))**2.>max_proj) then
               b_of_max_proj(ib,ik,i_spin)=i1
               max_proj=abs(SC_R(i1,ib,ik,i_spin))**2.
             endif
           enddo
           !
           ! Max Overlap
           !
           max_overlap(ib,ik,i_spin)=maxloc(abs(SC_R(ib,1:SC_bands(2),ik,i_spin)),1)
           ! 
           ! simmetrize the spreads summing over all possible degenerate
           ! bands
           !
           forall (ibp=1:SC_bands(2)) WS_spread(ibp)=abs(SC_R(ibp,ib,ik,i_spin))**2.*100./HARTREE
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
         ! end do SC_bands(2)
         !
         dE(:,ik,i_spin)=E%E(:SC_bands(2),ik,i_spin)-E_bare(:,ik,i_spin)
         !
       enddo
       !
     enddo
     !
     ! Final occupations 
     !====================
     if (l_SC_cycle.or.(l_SC_RESTART.and..not.l_SC_cycle).or.l_NSC_shot) call OCCUPATIONS_Fermi(E,k,2)  
     !
     dE(:,:,:)=E%E(:SC_bands(2),:,:)-E_bare(:,:,:)
     !
     call msg('r',' ')
     !
     do ik=1,k%nibz
       call REPORT_Energies(E%E(:SC_bands(2),:,:),k,Xk,SC_bands(2),(/ik,ik/),'E  ',.TRUE.)
#if defined _MAGNETIC
       call REPORT_Energies(E%f(:SC_bands(2),:,:)/HARTREE,k,Xk,SC_bands(2),(/ik,ik/),'occ',.FALSE.)
#else
       call REPORT_Energies(E_bare,k,Xk,SC_bands(2),(/ik,ik/),'E_o',.FALSE.)
       call REPORT_Energies(dE,k,Xk,SC_bands(2),(/ik,ik/),'d_E',.FALSE.)
       call REPORT_Energies(spread,k,Xk,SC_bands(2),(/ik,ik/),'Spr',.FALSE.)
       call REPORT_Energies(max_overlap/HARTREE,k,Xk,SC_bands(2),(/ik,ik/),'MxO',.FALSE.)
       call REPORT_Energies(real(b_of_max_proj)/HARTREE,k,Xk,SC_bands(2),(/ik,ik/),'Bnd',.FALSE.)
#endif
     enddo
     !
   end subroutine SC_report
   !
end subroutine SC_driver

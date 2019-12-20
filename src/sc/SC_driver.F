!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG
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
subroutine SC_driver(X,Xw,Xk,E,k,q,Dip)
 !
 use pars,           ONLY:SP,schlen,cZERO
 use units,          ONLY:HA2EV
 use drivers,        ONLY:l_sc_coh,l_sc_sex,l_sc_is_libDFT,l_eval_collisions,l_sc_hartree,&
&                         l_sc_exx,l_sc_electric,l_sc_fock,l_use_collisions
 use electrons,      ONLY:levels,nel,n_met_bands,n_sp_pol,n_spin,spin,n_spinor
 use R_lattice,      ONLY:bz_samp,nkibz
 use QP_m,           ONLY:QP_nk,QP_Sc,QP_n_states,QP_Vnl_xc,SC_E_threshold
 use SC,             ONLY:SC_bands,SC_iterations,SC_rho_threshold,E_convergence,      &
&                         rho_convergence,magn_convergence,it_now,           &
&                         l_SC_cycle,SC_cycle_mixing,SC_alloc,SC_free,l_SC_nl_mix,    &
&                         l_SC_RESTART,l_Mean_Potential,it_to_start,l_froze_density,  &
&                         l_NSC_shot,SC_preconditioning,V_mean,SC_up_W_iters
 use hamiltonian,    ONLY:rho_n,magn_n,l_sc_V_is_local,H_nl_sc,WF_G_max,WF_Go_indx,   &
&                         V_hartree_sc,Hzero,V_xc_sc,E_reference,l_sc_XC_is_local,            &
&                         H_QP_table_setup,H_rotation
 use wave_func,      ONLY:WF,WAVEs,WF_copy
 use FFT_m,          ONLY:fft_size
 use linear_algebra, ONLY:DIAGO
 use interfaces,     ONLY:el_density_and_current,PARALLEL_global_indexes,WF_load,WF_free, &
&                         PARALLEL_WF_distribute,PARALLEL_WF_index,LINEAR_ALGEBRA_driver,OCCUPATIONS_Fermi
 use H_interfaces,   ONLY:V_real_space_to_H
 use com,            ONLY:msg
 use X_m,            ONLY:X_t,current_iq,X_alloc
 use DIPOLES,        ONLY:DIPOLE_t
 use LIVE_t,         ONLY:LIVE_message
 use stderr,         ONLY:intc,real2ch,log_as_a_file
 use xc_functionals, ONLY:E_xc
 use frequency,      ONLY:w_samp
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_QP,PP_redux_wait,CPU_structure
 use IO_m,           ONLY:OP_RD,RD_CL_IF_END,io_control,REP,VERIFY,io_COLLs,io_RESPONSE,&
&                         OP_WR,WR_CL_IF_END,NONE,WR,OP_WR_CL,OP_RD_CL,IO_and_Messaging_switch
 use vec_operate,    ONLY:degeneration_finder,v_norm
 use LIVE_t,         ONLY:live_timing
 use collision_ext,  ONLY:COH_COLL_element,HXC_COLL_element,COLLISIONS_have_HARTREE
 use electrons,      ONLY:Spin_magn
#if defined _ELECTRIC
 use electric,       ONLY:ELECTRIC_alloc,ELECTRIC_free,W_electric
#endif
 !
#include<memory.h>
 !
 type(levels) ::E
 type(bz_samp)::k,q,Xk
 type(X_t)    ::X(2)  ! Xx Em1s
 type(DIPOLE_t)::Dip
 type(w_samp) ::Xw(2)
 !
 ! Iterations
 !
 integer                  :: it
 real(SP)                 :: spread(SC_bands(2),k%nibz,n_sp_pol),&
&                            E_previous(SC_bands(2),k%nibz,n_sp_pol),dE(SC_bands(2),k%nibz,n_sp_pol),&
&                            E_total,E_tot_Hartree,E_tot_xc,max_overlap(SC_bands(2),k%nibz,n_sp_pol)
 real(SP),    allocatable :: V_xc_nm1(:,:),rho_nm1(:),magn_nm1(:,:)
 real(SP),    allocatable :: delta_rho(:),delta_magn(:,:)
 complex(SP), allocatable :: H_nl_nm1(:,:,:,:)
#if defined _ELECTRIC
 complex(SP), allocatable :: SC_R0(:,:,:,:)
#endif
 !
 type(WAVEs)              :: WFo
 ! 
 ! Work Space
 !
 integer                  :: ir,i1,i2,ik,ib,ibp,ID(4),io_E,io_V,io_WF,i_sp_pol,i_io,io_X_err,io_DIP_err
 real(SP)                 :: max_proj,V_xc_sc_shift(n_spin)
 character(schlen)        :: dumb_ch,dumb_ch2
 integer, external        :: io_SC_components,X_dielectric_matrix
 logical, external        :: STOP_now
 !
#if defined _ELECTRIC
 complex(SP) :: Initial_Pol(3),Pol(3)
#endif
 !
 ! ws for states spread evaluation
 !
 real(SP)                 :: WS_spread(SC_bands(2))
 integer                  :: first_el(SC_bands(2)),n_of_el(SC_bands(2)),n_deg_grp,&
&                            b_of_max_proj(SC_bands(2),k%nibz,n_sp_pol)
 !
 ! SC_bands(1)
 !=============
 !
 if (SC_bands(1)/= 1  ) call error(' Lower bound of band indexes must to be 1')
 if (SC_bands(2)> E%nb) call error(' Upper bound of band indexes cannot to be higher than '//trim(intc(E%nb)))
 !
 call H_QP_table_setup(E)
 !
 ! Parallel Setup
 !================
 if (.not.l_eval_collisions) then
   call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
   call PARALLEL_global_indexes(E,k,q,"Self_Energy") 
   call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_G_b,CLEAN_UP=.TRUE.)
   call PARALLEL_WF_distribute(QP_index=PAR_IND_QP)
   call PARALLEL_WF_index( )
 endif
 !
 ! LA communicators
 !==================
 !
 call PARALLEL_assign_LA_COMMs("SE",DIAGO,CPU_structure(5)%nCPU_lin_algebra_DIAGO)
 !
 ! Initial Start & Restart 
 !==========================
 ! 
 call SC_start_and_restart(X,E,Xk)
 !
 ! DIPOLES
 !=========
 !
 if ((l_sc_exx.and.l_SC_cycle) .or. (SC_up_W_iters>0) ) then
   call DIPOLE_dimensions(E,Dip,SC_bands,(/0._SP,0._SP,0._SP/))
   call DIPOLE_IO(k,E,Dip,'read ',io_DIP_err,"SC")
 endif
 !
 if (.not.any((/l_SC_RESTART,l_SC_cycle,l_NSC_shot/))) return
 !
 ! Allocation(s)...
 !==================
 !
 call SC_alloc(E)
 call SC_local_alloc( )
 !
 ! COLLISIONS
 !============
 !
 if (io_COLLs) call COLLISIONS_load(.FALSE.)
 !
 ! Initialize SC_R matrix
 !========================
 !
 if(.not.l_SC_RESTART.and.l_use_collisions) then
   !      
   H_rotation=cZERO
   forall(i1=1:SC_bands(2))
     H_rotation(i1,i1,:,:)=1._SP
   end forall
   !
 else if (.not.l_SC_cycle.and.l_SC_RESTART) then
   !
   ! load SC_R rotation matrices
   !=============================
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2))
   io_WF=io_SC_components('WF',E,ID(2))
   do i_sp_pol=1,n_sp_pol
     do ik=1,QP_nk
       call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1+(i_sp_pol-1)*QP_nk/),ID=ID(2))
       io_WF=io_SC_components('WF',E,ID(2))
     enddo
   enddo
   call msg('r',' ')
   !
   call SC_report()
   return
   !
 endif
 !
 call msg('r','')
 !
 !  Hzero (kinetic + potential)
 !=============================
 call Bare_Hamiltonian(E,Xk,k)
 !
 ! Reference wavefunctions
 !
 call WF_copy(WF,WFo)
 !
 ! Start-Up setup.  Eventually restart setup.
 !
 ! WFs
 !=====
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
   if ((1._SP-SC_cycle_mixing)>1.E-5) call &
&    warning('Short splitted runs with fractional mixing may give different results')
   !
   ! RESTART
   !
   do i_sp_pol=1,n_sp_pol
     do ik=1,QP_nk
       call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1+(i_sp_pol-1)*QP_nk/),ID=ID(2))
       io_WF=io_SC_components('WF',E,ID(2))
       call WF_rotate(ik,i_sp_pol,WFo,fft_size)
     enddo
   enddo
   !
   ! ... rho_n and V_hartree_sc
   !
   call el_density_and_current(E,k,rho=rho_n)
   if (n_spin>1) call el_magnetization(E,k,magn_n)
   if (l_sc_hartree) call V_Hartree(rho_n,V_hartree_sc)
   !
   V_xc_nm1 = V_xc_sc
   rho_nm1  = rho_n
   if(n_spin>1) magn_nm1  = magn_n
   !
   it_to_start=it_now+1
   !
 else if (l_NSC_shot) then
   !
   ! Non-SC shot
   !
   it_to_start=1
   it_now=0
   !
 else
   !
   it_to_start=1
   it_now=0
   !
   V_xc_sc  =0._SP
   V_xc_nm1 =0._SP
   !
 endif
 !
#if defined _ELECTRIC
 ! Initial Polarization
 ! ====================
 if(l_sc_electric)  call Berry_polarization_NEQ(E,Xk,SC_R(:,1:E%nbf,:,:),Initial_Pol)
#endif
 !
 ! SCF cycle & NSC shot
 !=====================
 ! 
 if (l_NSC_shot) call live_timing('[NSC] Single shot loop',QP_nk)
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
   if (it>1.and.l_sc_hartree.and..not.COLLISIONS_have_HARTREE) then
     !
     if (trim(SC_preconditioning)=="simple") then 
       !
       rho_n = SC_cycle_mixing*rho_n + (1._SP-SC_cycle_mixing)*rho_nm1
       if(n_spin>1) magn_n= SC_cycle_mixing*magn_n+ (1._SP-SC_cycle_mixing)*magn_nm1
       !
     else if (trim(SC_preconditioning)=="thomas-fermi") then 
       !
       delta_rho =rho_n -rho_nm1
       call SC_precondition(delta_rho)
       rho_n =rho_nm1 +SC_cycle_mixing*delta_rho
       !
       if(n_spin>1) then
         delta_magn=magn_n-magn_nm1
         if (n_spinor==2) then
           call SC_precondition(delta_magn(i1,1))
           call SC_precondition(delta_magn(i1,2))
         endif
         call SC_precondition(delta_magn(i1,3))
         magn_n=magn_nm1+SC_cycle_mixing*delta_magn
       endif
       !
     endif
     !
     call V_Hartree(rho_n,V_hartree_sc)
     !
   endif
   !
#if defined _ELECTRIC
   !
   ! Static Electric Field
   ! ========================
   !
   if(l_sc_electric) then
     !      
     call SC_add_efield(SC_R(:,1:E%nbf,:,:),E,Xk,Pol)
     Pol=Pol-Initial_Pol
     !
   endif
   !
#endif
   ! 
   ! IO [V_xc]: this is the only DB that must be written on-fly
   !            Both WFs and Energies are written ay the end of the loop
   !===========
   !
   if (     l_sc_V_is_local) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(3))
   if (.not.l_sc_V_is_local) call io_control(ACTION=OP_WR,   COM=NONE,SEC=(/1/),ID=ID(3))
   io_V=io_SC_components('V' ,E,ID(3))
   !
   do i_sp_pol=1,n_sp_pol
   !
   do ik = 1, QP_nk 
     !
     H_nl_sc=cZERO
     !
     ! Build H_nl_sc
     !===============
     !
     if (l_use_collisions .or. (.not.l_sc_V_is_local) ) then
       !
       ! Without collisions FOCK goes in QP_Vnl_xc and CORR in QP_Sc.
       ! With    collisions FOCK and CORR (with also Hartree) go in QP_Vnl_xc
       !
       if (l_sc_fock.or.l_sc_sex.or.l_use_collisions) call V_qp_basis_to_H(ik,i_sp_pol,QP_Vnl_xc,H_nl_sc)
       !
       if (l_sc_coh.or.l_sc_sex)                      call V_qp_basis_to_H(ik,i_sp_pol,QP_Sc,    H_nl_sc)
       !
       ! Rotate H_nl_sc in the WFo basis.
       !
       if (.not.l_use_collisions.and.it_now>1) call OBS_rotate(H_rotation(:,:,ik,i_sp_pol),H_nl_sc,SC_bands(2)-SC_bands(1)+1,-1)
       ! 
       ! I/O [H_nl_sc]
       !
       call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID(3))
       io_V=io_SC_components('V',E,ID(3))
       !
       ! Mixing for non-local self-energies
       !====================================
       !
       if (l_SC_nl_mix) then
          !
          if(it>1) H_nl_sc = SC_cycle_mixing*H_nl_sc + (1._SP-SC_cycle_mixing)*H_nl_nm1(:,:,ik,i_sp_pol)
          !
          H_nl_nm1(:,:,ik,i_sp_pol)= H_nl_sc
          !
       endif
       !
     endif
     !
#if defined _ELECTRIC
     if(l_sc_electric) H_nl_sc=H_nl_sc+W_electric(:,:,ik,i_sp_pol)
#endif
     ! Add local V_xc
     !
     if (l_sc_XC_is_local.and..not.l_use_collisions)     call V_real_space_to_H(ik,i_sp_pol,H_nl_sc,WFo,'xc ',Vxc=V_xc_sc)
     !
     ! Add V_Hartree
     !
     if  (l_sc_hartree.and..not.COLLISIONS_have_HARTREE) call V_real_space_to_H(ik,i_sp_pol,H_nl_sc,WFo,'def',V=V_hartree_sc)
     !
     call PP_redux_wait(H_nl_sc)
     !
     ! Add Hzero
     !
     H_nl_sc = H_nl_sc + Hzero(:,:,ik,i_sp_pol)
     !
     ! Diagonalize H_nl_sc 
     !=====================
     !
     call LINEAR_ALGEBRA_driver(DIAGO,M=H_nl_sc,E_real=E%E(:SC_bands(2),ik,i_sp_pol))
     !
     ! Rotation (WFo->WF) matrix
     !
     H_rotation(:,:,ik,i_sp_pol)=H_nl_sc(:,:) 
     !
     ! New wave functions 
     !====================
     !
     call WF_rotate(ik,i_sp_pol,WFo,fft_size)
     !
     if (l_NSC_shot) call live_timing(steps=1)
     ! 
     ! Mean Potential 
     !================
     !
     if (l_Mean_Potential) call V_Mean_Potential(ik,i_sp_pol,WFo)
     !
   enddo ! ik
   !
   enddo ! i_sp_pol
   !
   if (l_NSC_shot) call OCCUPATIONS_Fermi(E,k,mode="OCCUPATIONS") 
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
     if(.not.l_sc_electric) then
       !
       forall (ib=SC_bands(2)+1:E%nb) E%E(ib,:,:)=E%E(ib,:,:)+E%E(SC_bands(2),:,:)-E_previous(SC_bands(2),:,:)
       !
       ! The same for the bands below SC_bands(1)
       !
       forall (ib=1:SC_bands(1)-1) E%E(ib,:,:)=E%E(ib,:,:)+E%E(SC_bands(1),:,:)-E_previous(SC_bands(1),:,:)
       !
     endif
     !
     call OCCUPATIONS_Fermi(E,k,mode="OCCUPATIONS") 
     !
     ! New density 
     !=============
     !
     rho_nm1 = rho_n 
     if(n_spin>1) magn_nm1 = magn_n 
     !
     if(.not.l_froze_density) call el_density_and_current(E,k,rho=rho_n)
     if(n_spin>1) call el_magnetization(E,k,magn_n)
     !
     ! Update dipoles and recalcolate the dielectric constant
     ! ======================================================
     !
     if (SC_up_W_iters>0.and.mod(it,SC_up_W_iters)==0) then
       !
       ! Update dipoles
       !
       ! Davide: this should be replaced by the use of covariant dipoles ...
       call DIPOLE_SC_rotate(H_rotation,E,Dip, 1)
       call DIPOLE_SC_rotate(H_rotation,E,Dip,-1)
       !
       ! Recalcolate Xo and X  
       !
       current_iq=0
       call IO_and_Messaging_switch("-report")
       io_X_err=X_dielectric_matrix(E,Xk,q,X(2),Xw(2),Dip,.true.)
       call IO_and_Messaging_switch("+report")
       !
       ! Reload and rotate wave-functions if W is updated
       ! =======================================
       !
       call WF_load(WF,WF_G_max,WF_Go_indx,(/1,max(SC_bands(2),E%nbm)/),(/1,nkibz/),title='-SC',force_WFo=.true.)
       !
       do ik=1,QP_nk
         call WF_rotate(ik,i_sp_pol,WFo,fft_size)
       enddo
       !  
     endif
     !
     ! Total Energy 
     !==============
     !
     call SC_Energy(E_total,E_tot_Hartree,E_tot_xc,E,k,rho_n)
     !
     ! Convergences
     !==============
     !
     E_convergence=0.
     do i_sp_pol=1,n_sp_pol
       do ik=1,k%nibz
         E_convergence = max( E_convergence,&
&                         maxval(abs(E%E(:SC_bands(2),ik,i_sp_pol)-E_previous(:,ik,i_sp_pol)-&
&                                    E%E(1,ik,i_sp_pol)+E_previous(1,ik,i_sp_pol)       )))
       enddo
     enddo
     !
     rho_convergence = 0._SP
     do ir=1,fft_size
       rho_convergence = rho_convergence + abs(rho_n(ir)-rho_nm1(ir))/nel
     enddo
     !
     magn_convergence = 0._SP
     if(n_spin>1) then
       do ir=1,fft_size
         if (n_sp_pol==2) magn_convergence = abs(magn_n(ir,3)-magn_nm1(ir,3))/nel
         if (n_spinor==2) magn_convergence = v_norm(magn_n(ir,:)-magn_nm1(ir,:))/nel
       enddo
     endif
     !
     if(it_now==it_to_start) dumb_ch ='[SC] Iteration'
     dumb_ch2='            '//trim(intc(it_now))//' '
     if (l_sc_V_is_local) then
       if(it==1) dumb_ch= trim(dumb_ch) //'  E_tot(Ha) '
       dumb_ch2=trim(dumb_ch2)//'  '//trim(real2ch(E_total))//'  '
     endif
     if (n_spin==2) then
       if(n_sp_pol==2) dumb_ch=trim(dumb_ch)//'  Mz  [muB] '
       if(n_spinor==2) dumb_ch=trim(dumb_ch)//'  |M| [muB] '
       dumb_ch2=trim(dumb_ch2)//'  '//trim(real2ch(v_norm(Spin_magn(:))))//' '
     endif
     if(it_now==it_to_start) dumb_ch =trim(dumb_ch)//' Acc.  E(ev)  -  rho(%)'
     dumb_ch2=trim(dumb_ch2)//'    '//trim(real2ch(E_convergence*HA2EV)) &
&                      //' - '//trim(real2ch(rho_convergence))//'%'
     if (n_spin==2) then
       if(it_now==it_to_start) dumb_ch =trim(dumb_ch)//'     -  magn(%)    '
       dumb_ch2=trim(dumb_ch2)//'  - '//trim(real2ch(magn_convergence))//'%'
     endif
     !
#if defined _ELECTRIC
     if(l_sc_electric) then
       if(it==1) then
         dumb_ch= trim(dumb_ch) //'  Initial Pol;'
         dumb_ch2=trim(dumb_ch2)//'  '//trim(real2ch(sqrt(sum(real(Initial_Pol(:))**2))))
       else
          dumb_ch= trim(dumb_ch) //'  Polarization;'
          dumb_ch2=trim(dumb_ch2)//'  '//trim(real2ch(sqrt(sum(real(Pol(:))**2))))
       endif
     endif
#endif
     !
     if (.not.log_as_a_file.or.it_now==1) then
       if(it_now==it_to_start) call LIVE_message(dumb_ch ,"n","","%s")
       call LIVE_message(dumb_ch2,"n","","%s")
     else
       if (l_sc_V_is_local) then
         if (.not.log_as_a_file) then
           if(it_now==1) call LIVE_message(dumb_ch ,"r","","%s")
           call LIVE_message(dumb_ch2,"r","","%s")
         endif
         if (     log_as_a_file) then
           if(it_now==1) call LIVE_message(dumb_ch ,"n","","%s")
           call LIVE_message(dumb_ch2,"n","","%s")
         endif
       else
         if(it_now==1) call LIVE_message(dumb_ch ,"n","","%s")
         call LIVE_message(dumb_ch2,"n","","%s")
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
   ! IO [SC_R & Mean Potential]
   !============================
   !
   do i_io=1,2
     !
     if (.not.l_Mean_Potential.and.i_io==2) cycle
     !
     dumb_ch='WF'
     if ( i_io==2 ) dumb_ch='MP'
     !
     if (it==1) call io_control(ACTION=OP_WR,COM=REP, SEC=(/1/),ID=ID(2))
     if (it> 1) call io_control(ACTION=OP_WR,COM=NONE,SEC=(/1/),ID=ID(2))
     io_WF=io_SC_components(trim(dumb_ch),E,ID(2))
     do i_sp_pol = 1, n_sp_pol 
     do ik = 1, QP_nk 
       call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID(2))
       io_WF=io_SC_components(trim(dumb_ch),E,ID(2))
     enddo
     enddo
     !
   enddo
   !
   if ( (E_convergence<SC_E_threshold.and.rho_convergence<SC_rho_threshold.and.magn_convergence<SC_rho_threshold) .or.&
&       STOP_now(.FALSE.) ) exit
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
   call msg('nr','[SC] Iterations               ',it_now)
   call msg('r' ,'[SC] Accuracy on density      ',rho_convergence)
   if(n_spin>1) call msg('r' ,'[SC] Accuracy on magnetization',magn_convergence)
   call msg('r' ,'[SC]          on energies [ev]',E_convergence*HA2EV)
   if (     l_sc_V_is_local)  call msg('r' ,'[SC] Total Energy (no Ewald) [Ha/Ry]',E_total*(/1._SP,2._SP/))
   if (.not.l_sc_V_is_local)  call msg('r','')
   if (l_sc_hartree)   call msg('r' ,'[SC] Hartree Energy          [Ha/Ry]',E_tot_Hartree*(/1._SP,2._SP/))
   if (l_sc_is_libDFT) call msg('rn','[SC] xc Energy               [Ha/Ry]',E_tot_xc*(/1._SP,2._SP/))
   !
 else
   call msg('r','')
 endif
 !
 if (l_NSC_shot) call live_timing(steps=1)
 !
 call SC_report()
 !
 if (l_SC_cycle.or.l_NSC_shot) then
   call WF_free(WF)
   call WF_free(WFo)
   call SC_free()
   call SC_local_free()
 endif
 !
 contains
   !
   subroutine SC_local_alloc()
     !
     YAMBO_ALLOC(rho_nm1,(fft_size))
     rho_nm1=0._SP
     !
     if(n_spin>1) then
       YAMBO_ALLOC(magn_nm1,(fft_size,3))
       magn_nm1=0._SP
     endif
     !
     YAMBO_ALLOC(E_xc,(fft_size))
     !
     YAMBO_ALLOC(V_xc_nm1,(fft_size,n_spin))
     !
     if (trim(SC_preconditioning)=="thomas-fermi") then
       YAMBO_ALLOC(delta_rho,(fft_size))
       if(n_spin>1) then
         YAMBO_ALLOC(delta_magn,(fft_size,3))
       endif
     endif
     !
     if (l_Mean_Potential) then
       YAMBO_ALLOC(V_mean,(fft_size,SC_bands(2)))
     endif
     !
     if(l_use_collisions) then
       !
       YAMBO_ALLOC(QP_Vnl_xc,(QP_n_states))
       !
     endif
     !
     if (l_SC_nl_mix) then
       YAMBO_ALLOC(H_nl_nm1,(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk,n_sp_pol))
       H_nl_nm1=cZERO
     endif
     !
#if defined _ELECTRIC
     if(l_SC_electric) then
       call ELECTRIC_alloc(E)
       allocate(W_electric(SC_bands(2),SC_bands(2),QP_nk,n_sp_pol))
       call mem_est("W_electric",(/size(W_electric)/),(/2*SP/))
     endif
     allocate(SC_R0(SC_bands(2),SC_bands(2),QP_nk,n_sp_pol))
     call mem_est("SC_R0",(/size(QP_Vnl_xc)/),(/2*SP/))
#endif
     !
   end subroutine
   !
   subroutine SC_local_free()
     !
     YAMBO_FREE(rho_nm1)
     !
     if(n_spin>1) then
       YAMBO_FREE(magn_nm1)
     endif
     !
     YAMBO_FREE(E_xc)
     !
     YAMBO_FREE(V_xc_nm1)
     !
     if (trim(SC_preconditioning)=="thomas-fermi") then
       YAMBO_FREE(delta_rho)
       if(n_spin>1) then
         YAMBO_FREE(delta_magn)
       endif
     endif
     !
     if (l_Mean_Potential) then
       YAMBO_FREE(V_mean)
     endif
     !
     if(l_use_collisions) then
       !
       YAMBO_FREE(QP_Vnl_xc)
       !
       if (allocated(COH_COLL_element)) call COLLISIONS_alloc_and_free("COH","v1 table","clean",0)
       if (allocated(HXC_COLL_element)) call COLLISIONS_alloc_and_free("SEX","v1 table","clean",0)
       !
     endif
     !
     if (l_SC_nl_mix) then
       YAMBO_FREE(H_nl_nm1)
     endif
     !
     if (.not.io_RESPONSE) call X_alloc('X')
     !
#if defined _ELECTRIC
     if(l_SC_electric) then
       call ELECTRIC_free()
       deallocate(W_electric)
       call mem_est("W_electric")
     endif
#endif
     !
   end subroutine
   !
   subroutine SC_report()
     !     
     ! Before all the DFT energies are shifted the V_xc_sc
     ! must be shifted of max(Ev)-max(E_reference_v)
     ! for more details see A. Fleszar PRB 64, 245204
     !
     if (l_SC_cycle) then
       do i_sp_pol=1,n_sp_pol
         V_xc_sc_shift(i_sp_pol )= maxval(E_reference%E(:n_met_bands,:,i_sp_pol))-maxval(E%E(:n_met_bands,:,i_sp_pol)) 
         if (l_sc_V_is_local) then
           V_xc_sc(:,i_sp_pol)=V_xc_sc(:,i_sp_pol)+V_xc_sc_shift(i_sp_pol)
           call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(3))
           io_V=io_SC_components('V' ,E,ID(3))
         endif
       enddo
     endif
     !
     do i_sp_pol=1,n_sp_pol
       do ik=1,k%nibz
         !
         ! Check for degenrate bands in the bare energies
         !
         call degeneration_finder(E_reference%E(:SC_bands(2),ik,i_sp_pol),SC_bands(2),first_el,n_of_el,n_deg_grp,0.0001/HA2EV)
         !
         do ib=1,SC_bands(2)
           !
           ! Then search for the max projection   
           ! 
           max_proj=-1._SP
           do i1=1,SC_bands(2)
             if (abs(H_rotation(i1,ib,ik,i_sp_pol))**2>max_proj) then
               b_of_max_proj(ib,ik,i_sp_pol)=i1
               max_proj=abs(H_rotation(i1,ib,ik,i_sp_pol))**2
             endif
           enddo
           !
           ! Max Overlap
           !
           max_overlap(ib,ik,i_sp_pol)=maxloc(abs(H_rotation(ib,1:SC_bands(2),ik,i_sp_pol)),1)
           ! 
           ! simmetrize the spreads summing over all possible degenerate bands
           !
           forall (ibp=1:SC_bands(2)) WS_spread(ibp)=abs(H_rotation(ibp,ib,ik,i_sp_pol))**2*100._SP/HA2EV
           !
           ibp=b_of_max_proj(ib,ik,i_sp_pol)
           spread(ib,ik,i_sp_pol)=WS_spread( ibp )
           !
           do i1=1,n_deg_grp
             if (ibp>=first_el(i1).and.ibp<=first_el(i1)+n_of_el(i1)-1) then
               spread(ib,ik,i_sp_pol)=0._SP
               do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
                 spread(ib,ik,i_sp_pol)=spread(ib,ik,i_sp_pol)+WS_spread(i2)
               enddo
             endif
           enddo
         enddo
         !
         ! end do SC_bands(2)
         !
         dE(:,ik,i_sp_pol)=E%E(:SC_bands(2),ik,i_sp_pol)-E_reference%E(:SC_bands(2),ik,i_sp_pol)
         !
       enddo
       !
     enddo
     !
     ! Final occupations 
     !====================
     if (l_SC_cycle.or.(l_SC_RESTART.and..not.l_SC_cycle).or.l_NSC_shot) call OCCUPATIONS_Fermi(E,k,OBJ="E")
     !
     dE(:,:,:)=E%E(:SC_bands(2),:,:)-E_reference%E(:SC_bands(2),:,:)
     !
     call msg('r',' ')
     !
     do ik=1,k%nibz
       call REPORT_Energies(E%E(:SC_bands(2),:,:),k,Xk,SC_bands(2),(/ik,ik/),'E  ',.TRUE.)
       call REPORT_Energies(E%f(:SC_bands(2),:,:)/HA2EV,k,Xk,SC_bands(2),(/ik,ik/),'occ',.FALSE.)
       call REPORT_Energies(E_reference%E(:SC_bands(2),:,:),k,Xk,SC_bands(2),(/ik,ik/),'E_o',.FALSE.)
       call REPORT_Energies(dE,k,Xk,SC_bands(2),(/ik,ik/),'d_E',.FALSE.)
       call REPORT_Energies(spread,k,Xk,SC_bands(2),(/ik,ik/),'Spr',.FALSE.)
       call REPORT_Energies(max_overlap/HA2EV,k,Xk,SC_bands(2),(/ik,ik/),'MxO',.FALSE.)
       call REPORT_Energies(real(b_of_max_proj)/HA2EV,k,Xk,SC_bands(2),(/ik,ik/),'Bnd',.FALSE.)
     enddo
     !
   end subroutine SC_report
   !
end subroutine SC_driver

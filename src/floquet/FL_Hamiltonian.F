!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG IA
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
! for more details!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine FL_Hamiltonian(E,k,q,FL_V_bands,w0,iter_num)
 !
 ! Build electron-field operator
 ! call FL_Build_W_operator
 !
 ! Add diagonal part
 !                                        
 use pars,           ONLY:SP,cZERO,pi,cONE,cI,rZERO
 use units,          ONLY:SPEED_OF_LIGHT,HA2EV
 use electrons,      ONLY:levels,n_sp_pol,n_spin
 use R_lattice,      ONLY:bz_samp
 use zeros,          ONLY:zero_dfl
 use drivers,        ONLY:l_use_Hxc_collisions
 use hamiltonian,    ONLY:rho_n,H_nl_sc,V_hartree_sc,V_xc_sc
 use QP_m,           ONLY:QP_nk
 use X_m,            ONLY:global_gauge,X_t
 use fields,         ONLY:A_ext,A_tot,Efield
 use real_time,      ONLY:eval_DFT,eval_HARTREE,l_RT_induced_Field,rho_reference,&
&                         Ho_plus_Sigma,Phase_LifeTime,RT_Vnl_xc,RT_nbands         
 use wave_func,      ONLY:WF
 use xc_functionals, ONLY:V_xc,XC_potential_driver
 use nl_optics,      ONLY:Correlation,V_xc_0,full_rho,IPA,E_full,LRC,JGM,LRCW,l_use_DIPOLES,NL_LRC_alpha,   &
&                         NL_initial_P,E_tot,E_ext,E_ks,E_xc_0,I_relax,Alpha_ED,dG,eval_dG, & 
&                         NL_bands,VAL_BANDS,NL_P,NL_damping
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_states,FL_Ho_plus_Sigma,FL_beta,&
&                         FL_Ho_lift_degeneracy,max_extra_mode,n_tot_extra_modes,&
&                         FL_delta_rho,FL_delta_rho_hist,l_FL_conv_Rho,l_FL_conv_Chi,FL_FT_extra_Mm1,&
&                         FL_dG,FL_dG_time,FL_Sigma,l_FL_dephase,l_FL_precond
 use global_XC,      ONLY:WF_xc_functional,WF_kind
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PAR_Xk_ibz_index,myid
 use parallel_m,     ONLY:master_cpu
 use parallel_int,   ONLY:PP_redux_wait
 use wrapper,        ONLY:M_by_V
 use wrapper_omp,    ONLY:M_plus_alpha_M_omp
 use collision_ext,  ONLY:COLLISIONS_have_HARTREE
 use H_interfaces,   ONLY:V_real_space_to_H
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 use FFT_m,         ONLY:fft_size,fft_rot_r
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 complex(SP),   intent(in)    :: FL_V_bands(NL_bands(2),n_tot_fl_modes,maxval(E%nbf),QP_nk,n_sp_pol)
 real(SP),      intent(in)    :: w0
 integer,       intent(in)    :: iter_num
 !
 ! Working Space
 !
 integer      :: ik_bz,i_sp_pol,is,i1,i2,ik_mem,ir,i1_fl,i2_fl,i_t
 integer      :: i_fl,i_fks,i_shifted_fl,i1_fks,i2_fks,i_damping_fl
 integer      :: i1_extra,i_comb_extra,i_shift_comb_extra
 complex(SP)  :: E_vec_pot(3),E_xc_pot(3)
 real(SP)     :: phase(fft_size)
 character(4) :: filename
 character(1) :: density_method
 complex(SP)  :: FL_V_bands_time(NL_bands(2),n_tot_extra_modes,maxval(E%nbf),QP_nk,n_sp_pol)
 complex(SP)  :: FL_V_Hartree(fft_size,n_spin,n_tot_extra_modes)
 complex(SP)  :: FL_V_Hartree_KS(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),n_tot_extra_modes)
 complex(SP)  :: FL_V_Hartree_KS_time(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),n_tot_extra_modes)
 real(SP)     :: FL_rho_at_t(fft_size)
 real(SP)     :: FL_res_rho_at_t(fft_size)
 complex(SP)  :: FL_dG_prev(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk,n_tot_extra_modes)
 complex(SP)  :: FKS_Sigma(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP)  :: FL_Sigma_time(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk,n_tot_extra_modes)
 complex(SP)  :: FL_Sigma_prev(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk,n_tot_extra_modes)
 !
#if defined _TIMING
 call timing('FL Hamiltonian',OPR='start')
#endif
 !
 FKS_Sigma=cZERO
 density_method="T"
 !
 FL_V_Hartree = cZERO
 FL_V_Hartree_KS = cZERO
 FL_V_Hartree_KS_time = cZERO
 !aim137 check - should I only allocate these variables if Corr != IPA ?
 !
 select case(density_method)
 case("F")
   !
   if (iter_num > 1) then ! FL_delta_rho will be 0 in first iteration
     !
     if(.not.l_use_Hxc_collisions.or..not.COLLISIONS_have_HARTREE) then
       !
       if((eval_HARTREE .or. eval_DFT) .and. l_FL_conv_Chi) then
         !
         call FL_el_density(E,k,FL_delta_rho,FL_V_bands,NL_bands(1))
         !
         !print*, '>>>>Calculating rho Hartree in iteration ',iter_num
         !
         FL_delta_rho(:,max_extra_mode+1) = &
        &real(FL_delta_rho(:,max_extra_mode+1),SP) - rho_reference(:)
         !
         FL_delta_rho = FL_delta_rho * FL_beta + &
        &               FL_delta_rho_hist * (1._SP-FL_beta)
         !
         FL_delta_rho_hist = FL_delta_rho
         !
         !aim137 prelim - find appropriate criterio to have Rho converged
         !if (iter_num > 4) l_FL_conv_Rho = .true.
       endif
       !
       if (eval_HARTREE) then
         !
         !print*, '>>>>Calculating V Hartree in iteration ',iter_num
         do i1_extra=1,n_tot_extra_modes
           !
           call V_Hartree(FL_delta_rho(:,i1_extra),FL_V_Hartree(:,:,i1_extra))
           !
         enddo
         !
       endif
       !
     endif
     !
   else
     !
     if(allocated(FL_delta_rho_hist)) FL_delta_rho_hist = cZERO
     if(allocated(FL_delta_rho)) FL_delta_rho = cZERO
     !
   endif
   !
 case("T")
   !
   FL_V_Hartree = cZERO
   FL_V_Hartree_KS = cZERO
   FL_V_Hartree_KS_time = cZERO
   !
   if (iter_num > 1) then
     !
     if(.not.l_use_Hxc_collisions.or..not.COLLISIONS_have_HARTREE) then
       !
       if((eval_HARTREE .or. eval_DFT) )then!.and. l_FL_conv_Chi) then
         !
         call FL_build_valence_bands(E,k,FL_V_bands,FL_V_bands_time)
         !
         !print*, '>>>>Calculating rho Hartree in iteration ',iter_num
         do i_t=1,n_tot_extra_modes
           !
           call NL_build_valence_bands(E,FL_V_bands_time(:,i_t,:,:,:),VAL_BANDS,NL_bands(1))
           !
           call el_density_vbands(E,k,FL_rho_at_t,VAL_BANDS)
           FL_delta_rho(:,i_t) = cZERO
           FL_delta_rho(:,i_t) = FL_rho_at_t(:)
           !
           !if (i_t == 1) print*, sum(real(FL_delta_rho(:,i_t))),sum(rho_reference)
           !
           FL_delta_rho(:,i_t) = FL_delta_rho(:,i_t) - rho_reference(:)
           !
           FL_res_rho_at_t = real(FL_delta_rho(:,i_t),SP) &
                         & - real(FL_delta_rho_hist(:,i_t),SP)
           !
           if (l_FL_precond) call FL_precondition(FL_res_rho_at_t)
           !
           FL_delta_rho(:,i_t) = FL_res_rho_at_t * FL_beta + &
                               & FL_delta_rho_hist(:,i_t)
           !
         enddo
         !
         FL_delta_rho_hist = FL_delta_rho
         !
         !aim137 prelim - find appropriate criterio to have Rho converged
         !if (iter_num > 4) l_FL_conv_Rho = .true.
       endif
       !
       if (eval_HARTREE) then
         !
         do i_t=1,n_tot_extra_modes
           !
           call V_Hartree(FL_delta_rho(:,i_t),FL_V_Hartree(:,:,i_t))
           FL_V_Hartree(:,:,i_t) = cZERO + real(FL_V_Hartree(:,:,i_t),SP)
           !
           !print*, i_t-1, FL_V_Hartree(9,1,i_t), FL_V_Hartree(13,1,i_t), FL_V_Hartree(21,1,i_t)
         enddo
         !
       endif
       !
     endif
     !
   else
     !
     if(allocated(FL_delta_rho_hist)) FL_delta_rho_hist = cZERO
     if(allocated(FL_delta_rho)) FL_delta_rho = cZERO
     !
   endif
   !
 end select
 !
 !
 ! Evaluate the G_lesser if using TD-SEX or TD-HF and than add COLLISIONS
 ! ======================================================
 ! aim137 put this in a loop on time steps.
 if (eval_dG .and. iter_num > 1) then
   !
   !print*, '>>>>Calculating dG in iteration ',iter_num
   call FL_build_valence_bands(E,k,FL_V_bands,FL_V_bands_time)
   do i_t=1,n_tot_extra_modes
     !
     call NL_build_dG_lesser(E,FL_V_bands_time(:,i_t,:,:,:),&
&    FL_dG_time(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),:,i_t))
     !
     !aim137 DEBUG<
     !print*, 'Time step =',i_t,' at kpt =',26
     !do i1=NL_bands(1),NL_bands(2)
     !print*, real(FL_dG_time(i1,:,26,i_t))
     !enddo
     !aim137 DEBUG>
     !
   enddo
   !
   !
 else
   !
   if (allocated(FL_dG)) FL_dG = cZERO
   if (allocated(FL_dG_time)) FL_dG_time = cZERO
   FL_dG_prev = cZERO
   !
 endif
 !
 !aim137 DEBUG<
 !do i_t=1,n_tot_extra_modes
 !print*, 'Extra mode =',i_t,' at kpt =',26
 !do i1=NL_bands(1),NL_bands(2)
 !print*, abs(FL_dG(i1,:,26,i_t))
 !enddo
 !enddo
 !aim137 DEBUG>
 !
 if(l_use_Hxc_collisions .and. iter_num > 1) then
   !
   !
   do i_t=1,n_tot_extra_modes
     !
     call COLLISIONS_compose_nl(FL_dG_time(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),:,i_t))
     !
     FL_Sigma_time(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),:,i_t) = &
   & RT_Vnl_xc(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),:,1)
     !
   enddo
   !
   !aim137 DEBUG<
    !print*, 'I will print Sigma_Hartree in time and KS space'
    !do ik_bz=26,26!1,QP_nk
    !print*, ''
    !print*, 'kpt',ik_bz,' and time step =',1
    !do i1=NL_bands(1),NL_bands(2)
    !print*, abs(FL_sigma_time(i1,:,ik_bz,1))
    !enddo
    !print*, ''
    !enddo
   !aim137 DEBUG>
   FL_Sigma_prev = FL_Sigma
   FL_Sigma = cZERO
   !
   do ik_bz=1,QP_nk
   !aim137 puedo paralelizar en kpoints aca
     do i1=NL_bands(1),NL_bands(2)
       do i2=NL_bands(1),NL_bands(2)
         !
         call M_by_V('N',n_tot_extra_modes,n_tot_extra_modes,cONE,FL_FT_extra_Mm1,&
      &  n_tot_extra_modes,FL_Sigma_time(i1,i2,ik_bz,:),1,cZERO,FL_Sigma(i1,i2,ik_bz,:),1)
         !
       enddo
     enddo
   enddo
   !
   !FL_Sigma = FL_beta * FL_Sigma + (1._SP-FL_beta) * FL_Sigma_prev
   !
 else
   !
   if (allocated(FL_Sigma)) FL_Sigma = cZERO
   FL_Sigma_time = cZERO
   FL_Sigma_prev = cZERO
   !
 endif
 !
 !
 !aim137 DEBUG<
! do ik_bz=1,QP_nk
! print*, 'kpt',ik_bz,'----------------------------'
! do i1=NL_bands(1),NL_bands(2)
! print*, abs(FL_sigma(i1,:,ik_bz,3))
! enddo
! enddo
 !aim137 DEBUG>
 !
 ! Build the Hamiltonian
 ! =====================
 !
 Ho_plus_Sigma=cZERO
 I_relax      =cZERO
 !
 do i_sp_pol=1,n_sp_pol
  do ik_bz=1,QP_nk
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik_bz)) cycle
   !
   ! ik_mem is for FL_Ho_plus_Sigma, which is allocated
   ! to N_kpts/N_pools, instead of N_kpts
   !
   ik_mem=PAR_Xk_ibz_index(ik_bz)
   !
   ! Here I fill H_nl_sc with...
   !
   H_nl_sc=cZERO
   FL_V_Hartree_KS_time=cZERO
   !
   ! Correlation part Hartree + TDDFT
   ! ================================
   !
   if(Correlation/=IPA.and.(.not.l_use_Hxc_collisions.or..not.COLLISIONS_have_HARTREE)) then
     !
     if (eval_Hartree.and.(eval_DFT.or.Correlation==JGM)) then
       call error ('TDDFT not yet implemented in floptics')
     elseif(eval_Hartree.and.(.not.eval_DFT.and..not.Correlation==JGM)) then
        !if (ik_bz==26) print*, 'I will print V_Hartree in time and KS space'
       do i1_extra=1,n_tot_extra_modes
         call V_real_space_to_H(ik_bz,i_sp_pol,&
            & FL_V_Hartree_KS_time(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),i1_extra),&
            & WF,'def',FL_V_Hartree(:,:,i1_extra))
       enddo 
       !
   !aim137 DEBUG<
   !if (ik_bz==26) then
   !print*, 'kpt',ik_bz,' and time step =',1
   !print*, ''
   !do i1=NL_bands(1),NL_bands(2)
   !print*, abs(FL_V_Hartree_KS_time(i1,:,1))
   !enddo
   !print*, ''
   !endif
   !aim137 DEBUG>
       !
       select case(density_method)
       case("F")
         !
         FL_V_Hartree_KS = FL_V_Hartree_KS_time
         !
       case("T")
         !
         FL_V_Hartree_KS = cZERO
         !
         do i1=NL_bands(1),NL_bands(2)
           do i2=NL_bands(1),NL_bands(2)
             !
              call M_by_V('N',n_tot_extra_modes,n_tot_extra_modes,cONE,FL_FT_extra_Mm1,&
           &  n_tot_extra_modes,FL_V_Hartree_KS_time(i1,i2,:),1,cZERO,FL_V_Hartree_KS(i1,i2,:),1)
             !
           enddo
         enddo
         !
       end select
       !
       !
       do i1=NL_bands(1),NL_bands(2)
         do i1_fl=1,n_tot_fl_modes
           i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
           do i2=NL_bands(1),NL_bands(2)
             do i2_fl=1,n_tot_fl_modes
               i2_fks = i2_fl + (i2-1)*n_tot_fl_modes
               i_shift_comb_extra = (i1_fl - (max_fl_mode+1)) &
                                & - (i2_fl - (max_fl_mode+1))
               i_comb_extra = i_shift_comb_extra + (max_extra_mode+1)
               !
               H_nl_sc(i1_fks,i2_fks) = &
             & H_nl_sc(i1_fks,i2_fks) + &
             & FL_V_Hartree_KS(i1,i2,i_comb_extra)
               !
               !aim137 DEBUG<
               if (ik_bz == 26) then
               FKS_Sigma(i1_fks,i2_fks) = FL_V_Hartree_KS(i1,i2,i_comb_extra)
               endif
               !aim137 DEBUG>
             enddo
           enddo
         enddo
       enddo
     endif
     !
   endif
   !
   !aim137 DEBUG <
  !if (iter_num == 3 .and. ik_bz == 1) then
  !do i1_fks=FL_states(1),FL_states(2) 
  !write(*,'(100(1f18.14))') H_nl_sc(i1_fks,:)
  !enddo
  !call error ('>>>>>>>>>>>>>>>>>> stop')
  !endif
   !aim137 DEBUG >
   !
   ! Add Ho = T+V_ion+V_h+V_xc
   !==========================
   !
   ! aim137: omp features commented out for now.
! $omp parallel do default(shared), private(i1)
   do i1=NL_bands(1),NL_bands(2) 
     do i_fl=1,n_tot_fl_modes
       i_fks = i_fl + (i1-1)*n_tot_fl_modes
       i_shifted_fl = i_fl - (max_fl_mode+1)
       i_damping_fl = 1; if (i_shifted_fl == 0) i_damping_fl = 0
       if (l_FL_dephase) i_damping_fl = 0
       !
       H_nl_sc(i_fks,i_fks) = &
&              H_nl_sc(i_fks,i_fks) & 
&            + E_full(i1,ik_bz,i_sp_pol) &
&            - real(i_shifted_fl,SP) * w0 &
&            - cI * real(i_damping_fl,SP) * NL_damping &
&            + cI * FL_Ho_lift_degeneracy(i_fks,ik_bz,i_sp_pol) * NL_damping
       !
     enddo
   enddo
! $omp end parallel do
   !
   if(l_use_DIPOLES.or.trim(global_gauge)=='velocity') then
     !    
     !aim137 no se que es esto
     !call RT_apply_field(ik_bz,i_sp_pol,H_nl_sc,A_tot)
     call error ('Code with dipoles for 1st order only not yet implemented')
     !
   else
     !
     call FL_build_W_operator(E,k,A_tot,ik_bz,i_sp_pol,FL_V_bands(:,:,:E%nbf(i_sp_pol),:,i_sp_pol),H_nl_sc)
     !
   endif
   !
   ! Add Self-energy
   ! ===============
   !
   if (l_use_Hxc_collisions .and. iter_num > 1) then
     !if (ik_bz==26) print*, 'I will print the self-energy'
     do i1=NL_bands(1),NL_bands(2)
       do i1_fl=1,n_tot_fl_modes
         i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
         do i2=NL_bands(1),NL_bands(2)
           do i2_fl=1,n_tot_fl_modes
             i2_fks = i2_fl + (i2-1)*n_tot_fl_modes
             i_shift_comb_extra = (i1_fl - (max_fl_mode+1)) &
                              & - (i2_fl - (max_fl_mode+1))
             i_comb_extra = i_shift_comb_extra + (max_extra_mode+1)
             !
             H_nl_sc(i1_fks,i2_fks) = &
           & H_nl_sc(i1_fks,i2_fks) + &
           & FL_Sigma(i1,i2,ik_bz,i_comb_extra)
             !
             !aim137 DEBUG<
             !if (ik_bz == 26) then
             !FKS_Sigma(i1_fks,i2_fks) = FL_Sigma(i1,i2,26,i_comb_extra)
             !endif
             !aim137 DEBUG>
           enddo
         enddo
       enddo
     enddo
   endif
   !
   if (l_FL_dephase) call FL_dephasing(E,k,ik_bz,i_sp_pol,&
   &           FL_V_bands(:,:,:E%nbf(i_sp_pol),ik_bz,i_sp_pol),H_nl_sc)
   !
   ! Store the Hamiltonian
   ! =====================
!aim137: omp features disabled for now
! $omp parallel do default(shared), private(i1)
   do i1=NL_bands(1),NL_bands(2) 
     do i_fl=1,n_tot_fl_modes
       i_fks = i_fl + (i1-1)*n_tot_fl_modes
       FL_Ho_plus_Sigma(:,i_fks,ik_mem,i_sp_pol)=H_nl_sc(:,i_fks)
     enddo
   enddo
! $omp end parallel do
   !
  enddo  !---- loop on k-point
 enddo
 !
 !if(Phase_LifeTime<zero_dfl) call PP_redux_wait(I_relax,COMM=PAR_COM_Xk_ibz_INDEX%COMM ) 
 !
#if defined _TIMING
 call timing('FL Hamiltonian',OPR='stop')
#endif
 !
!aim137 DEBUG<
!print*, ''
!do i_fks=FL_states(1),FL_states(2)
!write(*,*) abs(FKS_Sigma(i_fks,:))
!enddo
!print*, ''
!aim137 DEBUG>
!
!!aim137 DEBUG<
!print*, 'printing Hamiltonian'
!do ik_bz=1,QP_nk
!write(filename,'(i0)') ik_bz
!open(111,file='FL_Ho_plus_Sigma'//trim(adjustl(filename))//'.dat')
!do i_fks=FL_states(1),FL_states(2)
!write(111,*) FL_Ho_plus_Sigma(i_fks,:,ik_bz,1)
!enddo
!close(111)
!enddo
!!aim137 END DEBUG
!
!aim137 DEBUG <
!if (master_cpu) then
!open(111,file='FL_rho_central_minus_reference.dat')
!write(111,*) 'comparing FL_rho_delta (central component) of first iteration with rho_reference'
!write(111,*) 'print FL_delta_rho(i,max_extra_mode+1)-rho_reference(i)'
!do ir=1,fft_size
!write(111,*) ir, FL_delta_rho(ir,max_extra_mode+1),rho_reference(ir),&
!            &FL_delta_rho(ir,max_extra_mode+1)-rho_reference(ir)
!enddo
!close(111)
!open(112,file='FL_rho_all_modes.dat')
!do ir=1,fft_size
!write(112,*) ir, FL_delta_rho(ir,:)
!enddo
!close(112)
!endif
!aim137 DEBUG >
!
end subroutine FL_Hamiltonian

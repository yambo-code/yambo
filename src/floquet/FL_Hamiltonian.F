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
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_states,FL_Ho_plus_Sigma,&
&                         FL_Ho_lift_degeneracy,FL_delta_rho,FL_delta_rho_hist,max_extra_mode,n_tot_extra_modes
 use global_XC,      ONLY:WF_xc_functional,WF_kind
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PAR_Xk_ibz_index,myid
 use parallel_m,     ONLY:master_cpu
 use parallel_int,   ONLY:PP_redux_wait
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
 complex(SP),   intent(in)    :: FL_V_bands(NL_bands(2),n_tot_fl_modes,E%nbf,QP_nk,n_sp_pol)
 real(SP),      intent(in)    :: w0
 integer,       intent(in)    :: iter_num
 !
 ! Working Space
 !
 integer      :: ik_bz,i_sp_pol,is,i1,i2,ik_mem,ir,i1_fl,i2_fl
 integer      :: i_fl,i_fks,i_shifted_fl,i1_fks,i2_fks,i_damping_fl
 integer      :: i1_extra,i_comb_extra,i_shift_comb_extra
 complex(SP)  :: E_vec_pot(3),E_xc_pot(3)
 real(SP)     :: alpha_,beta
 character(4) :: filename
 real(SP)     :: FL_V_Hartree(fft_size,n_spin,n_tot_extra_modes)
 complex(SP)  :: FL_V_Hartree_KS(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),n_tot_extra_modes)
 !
#if defined _TIMING
 call timing('FL Hamiltonian',OPR='start')
#endif
 !
 beta = 0.2_SP
 if(allocated(FL_delta_rho)) FL_delta_rho = rZERO
 FL_V_Hartree = rZERO
 FL_V_Hartree_KS = cZERO
 !aim137 check - should I only allocate these variables if Corr != IPA ?
 !
 if (iter_num > 1) then ! FL_delta_rho will be 0 in first iteration
   !
   if(.not.l_use_Hxc_collisions.or..not.COLLISIONS_have_HARTREE) then
     !
     if(eval_HARTREE .or.  eval_DFT) then
       !
       call FL_el_density(E,k,FL_delta_rho,FL_V_bands,NL_bands(1))
       !
       FL_delta_rho(:,max_extra_mode+1) = &
      &FL_delta_rho(:,max_extra_mode+1) - rho_reference(:)
       !
       FL_delta_rho = FL_delta_rho * beta + &
      &               FL_delta_rho_hist * (1-beta)
       !
       FL_delta_rho_hist = FL_delta_rho
       !
     endif
     !
     if (eval_HARTREE) then
       !
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
   if(allocated(FL_delta_rho_hist)) FL_delta_rho_hist = rZERO
   !
 endif
 !
 !
 ! Evaluate the external/total field
 !=================================
 !
 call RT_Ext_fields(A_ext,0.41341373336561360) !aim137 critical - see if this has any effect at all
 !
 A_tot=A_ext
 !
 E_ext=-A_ext%vecpot_vel/SPEED_OF_LIGHT
 !
 E_tot=E_ext
 E_ks =E_ext
 !
 ! aim137: deleted more beyond-IPA stuff
 ! aim137: deleted G_lesser
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
   !
   ! Correlation part Hartree + TDDFT
   ! ================================
   !
   !
   if(Correlation/=IPA.and.(.not.l_use_Hxc_collisions.or..not.COLLISIONS_have_HARTREE)) then
     !
     if (eval_Hartree.and.(eval_DFT.or.Correlation==JGM)) then
       call error ('TDDFT not yet implemented in floptics')
     elseif(eval_Hartree.and.(.not.eval_DFT.and..not.Correlation==JGM)) then
       do i1_extra=1,n_tot_extra_modes
         call V_real_space_to_H(ik_bz,i_sp_pol,&
            & FL_V_Hartree_KS(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),i1_extra),&
            & WF,'def',FL_V_hartree(:,:,i1_extra))
       enddo 
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
             enddo
           enddo
         enddo
       enddo
     endif
     !
   endif
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
     call FL_build_W_operator(FL_V_bands,E,k,A_tot,ik_bz,i_sp_pol,H_nl_sc)
     !
   endif                                                         
   !
   ! aim137 deleted more beyond-IPA stuff
   !
   ! aim137: we will not use dephasing
   ! call FL_dephasing(FL_V_bands,E,k,ik_bz,i_sp_pol,H_nl_sc)
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
!!aim137 DEBUG
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

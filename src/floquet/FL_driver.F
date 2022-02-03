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
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine FL_driver(E,X,k,q,Dip)
 !
 use pars,           ONLY:SP,cZERO,cONE
 use drivers,        ONLY:l_use_Hxc_collisions
 use QP_m,           ONLY:QP_ng_SH,QP_ng_Sx 
 use LIVE_t,         ONLY:live_timing 
 use electrons,      ONLY:levels,n_sp_pol
 use X_m,            ONLY:X_t
 use DIPOLES,        ONLY:DIPOLE_t,DIP_alloc,DIP_S
 use wave_func,      ONLY:WF,wf_ng
 use interfaces,     ONLY:WF_load,WF_free
 use hamiltonian,    ONLY:WF_Go_indx,H_QP_table_setup,H_potential
 use R_lattice,      ONLY:bz_samp,qindx_free
 use fields,         ONLY:Efield
 use QP_CTL_m,       ONLY:QP_apply
 use R_lattice,      ONLY:qindx_S
 use nl_optics,      ONLY:n_frequencies,Correlation,IPA,NL_alloc,NL_free,V_bands,eval_dG,NL_J,NL_initial_J, &
&                         NL_estep,NL_er,Integrator,l_use_DIPOLES,NL_P_t,JGM,TDDFT,NL_P, &
&                         E_ext_t,E_tot_t,E_ks_t,E_ks,E_tot,E_ext,freqs_done,n_freqs_todo,l_eval_CURRENT,&
&                         HF,SEX,NL_bands,NL_nbands,WF_sample,V_bands_t,NL_alloc_k_distributed
!aim137: add floquet 
 use fl_optics,      ONLY:max_fl_mode,FL_alloc,FL_alloc_k_distributed,FL_free,FL_V_bands,FL_space_dim
 use real_time,      ONLY:RT_step,NE_steps,NE_i_time,RT_nbands,RT_bands,eval_DFT
 use IO_m,           ONLY:io_COLLs
 use parallel_int,   ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,     ONLY:PAR_IND_freqs,PAR_IND_freqs_ID,PAR_IND_Xk_ibz
 use collision_ext,  ONLY:COLLISIONS_have_HARTREE
 !
#include<memory.h>
 !
 ! Notice that E is never changed!
 !
 type(levels)      :: E
 type(bz_samp)     :: k,q
 type(X_t)         :: X(2)
 type(DIPOLE_t)    :: Dip
 !
 logical, external :: stop_now
 ! 
 ! Work Space
 !
 integer           :: i_f,i1,N_sample_size,wf_ng_save,io_err,i_k_bz,i_sp_pol
 integer           :: FL_iteration,fl_shifted_zero_mode
 logical           :: l_selfconsistency_reached
 real(SP)          :: Time
 complex(SP)       :: All_S_mat_EQ(E%nbm,E%nbm,6,k%nbz,n_sp_pol)
 !
 ! In Non-Linear NL_bands always equal to RT_bands
 ! ==============================================
 !
 NL_nbands=NL_bands(2)-NL_bands(1)+1
 !
 RT_bands =NL_bands
 RT_nbands=NL_nbands
 !
 call section('*','FL Initialization')
 !
 ! Initialize
 !=========
 call FL_initialize(E,k,q)
 !
 ! Switch 
 !========
 call XC_potentials(H_potential)
 call XC_additional_SC_potentials()
 !
 ! Dipoles & WF dimensions
 !=========================
 call WF_and_dipole_dimensions(X,k)
 !
 ! Deallocate qindx_S (used only in previous subroutine)
 ! ========================
 YAMBO_FREE(qindx_S)
 !
 ! QP dimensions
 !================
 call H_QP_table_setup(E)
 !
 ! Non_Linear parallization
 ! ===================================
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.,Dip=Dip,X=X(1))
 call PARALLEL_global_indexes(E,k,q,"Non_Linear",Dip=Dip,X=X(1))
 call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 !Load Dipole after the NL parallelization
 ! ========================================
 call DIPOLE_dimensions(E,Dip,NL_bands,(/0._SP,0._SP,0._SP/))
 call DIPOLE_IO(k,E,Dip,'read ',io_err,'NL')
 if(io_err/=0) call error("Error in DIPOLES! ")
 !
 ! Free memory: qindx_* and unused DIPOLES
 ! =====================================================
 call qindx_free('ALL')
 !
 call DIP_alloc('P_square')
 if(l_use_DIPOLES) then
   call DIP_alloc('DIP_S')
 else
!    call DIP_alloc('DIP_R')
!   if(.not.l_eval_CURRENT) then
     call DIP_alloc('DIP_R')
     call DIP_alloc('DIP_v')
!   endif
 endif
 !
 ! WFs loading
 !=============
 !
 wf_ng_save =wf_ng
 if(eval_DFT) then
   wf_ng      =max(QP_ng_Sx,QP_ng_SH)
 else
   wf_ng      =QP_ng_SH
 endif
 WF_Go_indx =1
 !
 !
 ! QP-corrections 
 !================
 call QP_apply(NL_bands,E,k,"G",msg_fmt='s')
 !
 !
 ! Allocation
 ! ==================
 !aim137: newly written subroutines - under review
  call FL_alloc(E) 
 !aim137: this populated the variable FL_space_dim
  call FL_alloc_k_distributed(E)
 !
 ! Initialization
 ! ==================
 call section('*','FL Start and Restart')
 call FL_start_and_restart(E,k,Dip)
!aim137: newly written subroutine - under review
 !
 ! Check if calculation was already done
 ! =====================================
 if(n_freqs_todo==0) return
 !
 ! Initialize IO
 ! ==================
 !aim137: we'll need to add FL_databases
  call NL_databases_IO(0,'write')
 ! i_k_bz
 ! Start-Up & Timing
 !===================
 !aim137: check this is what we need:
 call live_timing('[FL] Frequencies ',PAR_IND_freqs%n_of_elements(PAR_IND_freqs_ID+1))
 !
 ! aim137 creo que no necesito esto al final. Mis S^0 ya estan en DIP_S
 ! Calculate Overlap matrices in EQ (no inversion nor determinant)
 ! ===============================================================
!do i_sp_pol=1,n_sp_pol
!  do i_k_bz=1,k%nbz
!    call FL_Build_Overlaps_Mat_EQ(E,i_sp_pol,i_k_bz,All_S_mat_EQ(:,:,:,i_k_bz,i_sp_pol))
!  enddo
!enddo
 !
 do i_f=1,n_frequencies
   !
   if(.not.PAR_IND_freqs%element_1D(i_f)) cycle
   !
   if(freqs_done(i_f)) then
     call live_timing(steps=NE_steps)
     cycle
   endif
   !
   ! Re-initialization
   ! ================
   FL_V_bands=cZERO
   do i1=1,E%nbf 
     !
     fl_shifted_zero_mode = max_fl_mode+1
     FL_V_bands(i1,fl_shifted_zero_mode,i1,:,:)=cONE
     !
   enddo
   !aim137 DEBUG test mixing
   do i1=NL_bands(1),E%nbf
     FL_V_bands(i1,fl_shifted_zero_mode+1,i1,:,:)=cONE*0.1
     FL_V_bands(i1,fl_shifted_zero_mode-1,i1,:,:)=cONE*-0.1
     FL_V_bands(i1,fl_shifted_zero_mode+2,i1,:,:)=cONE*-0.1
     FL_V_bands(i1,fl_shifted_zero_mode-2,i1,:,:)=cONE*0.1
   enddo
   !aim137 END DEBUG
   !
   ! Efield
   ! ================
   Efield(1)%frequency=NL_er(1)+real(i_f-1,SP)*NL_estep
   !
   l_selfconsistency_reached=.false.
   FL_iteration = 0
   !
   do while(.not.l_selfconsistency_reached)
     !
     FL_iteration=FL_iteration+1
     !
     ! aim137:
     ! do i_k_bz=1,k%nbz ! I do the k-loops inside Hamiltonian and Diagonalization,
     ! because that is how it's done in NL, but I don't understand the benefits. If
     ! anything, I think it would be worse for memory.
     !
     call FL_Hamiltonian(E,k,q,FL_V_bands,Efield(1)%frequency)
     !
     call FL_diagonalization
     !
     call live_timing(steps=1)
     !
     call FL_polarization
     !
     !aim137: preliminary criterion to avoid infinit loop
     l_selfconsistency_reached= (FL_iteration>=4)
     !
   enddo
   !
   !aim137 this subroutine is taking too much, have a look
   !aim137 it doesn't crash, but it gets stuck here
   call NL_databases_IO(i_f,'write')
   !
 enddo
 !
 call FL_output
 !
 call live_timing()
 !
 ! Deallocation
 ! ==========================
 !
 wf_ng     = wf_ng_save
 !
 call WF_free(WF)
 !it's crashing in the following subroutine
 call FL_free()
 call DIP_alloc('DIP_S')
 call DIP_alloc('DIP_R')
 call DIP_alloc('DIP_v')
 !
end subroutine FL_driver

!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine V_real_space_to_H(ik,i_sp_pol,H,WF,V_kind,V,Vxc)
 !
 use electrons,      ONLY:n_spin,n_spinor,spin
 use pars,           ONLY:SP,cZERO
 use FFT_m,          ONLY:fft_size
 use QP_m,           ONLY:QP_table,QP_n_states
 use wave_func,      ONLY:WAVEs
 use wrapper_omp,    ONLY:Vstar_dot_V_omp
 use matrix_operate, ONLY:UP_matrix_index
 use hamiltonian,    ONLY:H_ref_bands
 use timing_m,       ONLY:timing
#if defined _SC
 use drivers,        ONLY:l_sc_run
 use parallel_m,     ONLY:PAR_IND_QP,HEAD_QP_cpu
#endif
#if defined _RT
 use drivers,        ONLY:l_real_time
 use parallel_m,     ONLY:PAR_IND_B_mat_ordered,PAR_IND_WF_k
#endif
 !
 implicit none
 !
 integer,      intent(in)              :: ik,i_sp_pol
 complex(SP),  intent(inout)           :: H(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 type(WAVEs),  intent(in)              :: WF
 character(*), intent(in)              :: V_kind
 real(SP),     intent(in), optional    :: V(fft_size,n_spin)
 real(SP),     intent(in), optional    :: Vxc(fft_size,n_spin)
 !
 ! Work Space
 !
 integer     :: i_qp,ib,ibp,i_wf,i_wfp,i_spinor,j_spinor,i_spin,j_spin
 complex(SP) :: c_ws(fft_size,n_spinor)
 complex(SP) :: V_mat(fft_size,n_spin,n_spin)
 logical     :: PAR_IND_local(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 !
#if defined _RT
 if (l_real_time.and.(.not.PAR_IND_WF_k%element_1D(ik))) return
#endif
 !
 if(index(V_kind,"xc") >0 .and. .not.present(Vxc)) call error(' V_real_space_to_H called with xc kind but without Vxc')
 if(index(V_kind,"def")>0 .and. .not.present(V)  ) call error(' V_real_space_to_H called with def kind but without V')
 !
 call timing('V_real_space_to_H',OPR='start')
 !
 V_mat=cZERO
 if(index(V_kind,"xc") >0) call Build_V_xc_mat(Vxc,V_mat)
 if(index(V_kind,"def")>0) then
   forall(i_spinor=1:n_spinor) V_mat(:,i_sp_pol+i_spinor-1,i_sp_pol+i_spinor-1)= &
&         V_mat(:,i_sp_pol+i_spinor-1,i_sp_pol+i_spinor-1)+V(:,i_sp_pol+i_spinor-1)
 endif
 !
 PAR_IND_local=.true.
 !
#if defined _SC
 if (l_sc_run) then
   !
   PAR_IND_local=.false.
   !
   do i_qp=1,QP_n_states
     !
     if (.not.HEAD_QP_cpu)                 cycle
     if (     QP_table(i_qp,3) /=ik      ) cycle
     if (spin(QP_table(i_qp,:))/=i_sp_pol) cycle
     if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
     !
     ib =QP_table(i_qp,1)
     ibp=QP_table(i_qp,2)
     !
     PAR_IND_local(ib,ibp)=.TRUE.
     !
   enddo 
   !
 endif
#endif
 !
#if defined _RT
 if (l_real_time) then
   !
   PAR_IND_local=.false.
   !
   do ib=H_ref_bands(1),H_ref_bands(2)
     do ibp=ib,H_ref_bands(2)
       PAR_IND_local(ib,ibp)=PAR_IND_B_mat_ordered%element_1D( UP_matrix_index(ib-H_ref_bands(1)+1,ibp-H_ref_bands(1)+1)-1)
     enddo
   enddo
   !
 endif
#endif
 !
 ! Fill ...
 !
 do ib=H_ref_bands(1),H_ref_bands(2)
   do ibp=ib,H_ref_bands(2)
   !
   if (.not.PAR_IND_local(ib,ibp)) cycle
   !
   ! <i_wf|V_mat|i_wfp>
   !
   i_wf  =WF%index(ib ,ik,i_sp_pol)
   i_wfp =WF%index(ibp,ik,i_sp_pol)
   !
   c_ws=cZERO
   do i_spinor=1,n_spinor
     i_spin=i_sp_pol+i_spinor-1
     do j_spinor=1,n_spinor
       j_spin=i_sp_pol+j_spinor-1
       c_ws(:,i_spinor)=c_ws(:,i_spinor)+V_mat(:,i_spin,j_spin)*WF%c(:,j_spinor,i_wfp)
     enddo
   enddo
   !
   H(ib,ibp) = H(ib,ibp)+Vstar_dot_V_omp(fft_size*n_spinor,WF%c(:,:,i_wf),c_ws(:,:))
   !
   if(ib==ibp) H(ib,ib) =real(H(ib,ib),SP)
   if(ib/=ibp) H(ibp,ib)=conjg(H(ib,ibp))
   !
   enddo
 enddo
 !
 call timing('V_real_space_to_H',OPR='stop')
 !
end subroutine 

!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine V_real_space_to_H(ik,i_sp_pol,V,H,WF,V_kind)
 !
 use electrons,      ONLY:n_spin,n_spinor,spin
 use pars,           ONLY:SP,cZERO
 use FFT_m,          ONLY:fft_size
 use QP_m,           ONLY:QP_nb,QP_table,QP_n_states
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
 use parallel_m,     ONLY:PAR_IND_B_mat_ordered,PAR_IND_WF_k,HEAD_QP_cpu
#endif
 implicit none
 !
 integer,      intent(in)    :: ik,i_sp_pol
 real(SP),     intent(in)    :: V(fft_size,n_spin)
 complex(SP),  intent(inout) :: H(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 character(3), intent(in)    :: V_kind
 type(WAVEs) :: WF
 !
 ! Work Space
 !
 integer     :: ir,i_qp,ib,ibp,i_wf,i_wfp,i_spinor,j_spinor,i_spin,j_spin
 complex(SP) :: c_ws(fft_size,n_spinor)
 complex(SP) :: V_mat(fft_size,n_spin,n_spin)
 logical     :: PAR_IND_local(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 !
#if defined _RT
 if (l_real_time.and.(.not.PAR_IND_WF_k%element_1D(ik).or..not.HEAD_QP_cpu)) return
#endif
 !
 call timing('V_real_space_to_H',OPR='start')
 !
 if(V_kind=="xc ") call Build_V_xc_mat(V,V_mat)
 if(V_kind=="def") then
   V_mat=cZERO
   forall(i_spinor=1:n_spinor) V_mat(:,i_sp_pol+i_spinor-1,i_sp_pol+i_spinor-1)=V(:,i_sp_pol+i_spinor-1)
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
   if (.not.HEAD_QP_cpu) cycle
   if (.not.PAR_IND_local(ib,ibp)) cycle
   !
   ! <i_wf|V_mat|i_wfp>
   !
   !
   i_wf  =WF%index(ib ,ik,i_sp_pol)
   i_wfp =WF%index(ibp,ik,i_sp_pol)
   !
   c_ws=cZERO
   do i_spinor=1,n_spinor
     i_spin=i_sp_pol+i_spinor-1
     do j_spinor=1,n_spinor
       j_spin=i_sp_pol+j_spinor-1
#if defined _OPENMP
!$omp parallel do default(shared), private(ir)
       do ir=1,fft_size
         c_ws(ir,i_spinor)=c_ws(ir,i_spinor)+V_mat(ir,i_spin,j_spin)*WF%c(ir,j_spinor,i_wfp)
       enddo
!$omp end parallel do
#else
       c_ws(:,i_spinor)=c_ws(:,i_spinor)+V_mat(:,i_spin,j_spin)*WF%c(:,j_spinor,i_wfp)
#endif
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

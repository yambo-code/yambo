!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 use pars,           ONLY:SP
 use FFT_m,          ONLY:fft_size
 use QP_m,           ONLY:QP_nb,QP_table,QP_n_states
 use wave_func,      ONLY:WAVEs
 use wrapper,        ONLY:Vstar_dot_V
 use matrix_operate, ONLY:UP_matrix_index
 use hamiltonian,    ONLY:H_ref_bands
#if defined _SC || !defined _RT
 use parallel_m,     ONLY:PAR_IND_QP,HEAD_QP_cpu
#endif
#if defined _RT
 use parallel_m,     ONLY:PAR_IND_B_mat_ordered,PAR_IND_WF_k,HEAD_QP_cpu
#endif
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 implicit none
 !
 integer     :: ik,i_sp_pol
 real(SP)    :: V(fft_size,n_spin)
 complex(SP) :: V_mat(fft_size,n_spin,n_spin)
 complex(SP) :: H(QP_nb,QP_nb)
 character(3):: V_kind
 type(WAVEs) :: WF
 !
 ! Work Space
 !
 integer    :: i_qp,ib,ibp,ifft,ifftp
 complex(SP):: c_ws(fft_size,n_spin)
 !
#if defined _RT
 if (.not.PAR_IND_WF_k%element_1D(ik).or..not.HEAD_QP_cpu) return
#endif
 !
#if defined _TIMING
 call timing('V_real_space_to_H',OPR='start')
#endif
 !
 if(V_kind=="xc ") call Build_V_xc_mat(V,V_mat)
 !
 ! Fill ...
 !
 do i_qp=1,QP_n_states
   !
   if (     QP_table(i_qp,3) /=ik      ) cycle
   if (spin(QP_table(i_qp,:))/=i_sp_pol) cycle
   !
   ib =QP_table(i_qp,1)
   ibp=QP_table(i_qp,2)
   !
   if (ib>ibp) cycle
   !
   if (.not.HEAD_QP_cpu) cycle
#if defined _SC && !defined _RT
   if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
#endif
#if defined _RT
   if (.not.PAR_IND_B_mat_ordered%element_1D( UP_matrix_index(ib-H_ref_bands(1)+1,ibp-H_ref_bands(1)+1)-1 )) cycle
#endif
   !
   ! <ifft|V|ifftp>
   !
   if (n_spinor==1) then
     !
     ifft   =WF%index(ib,ik,i_sp_pol)
     ifftp  =WF%index(ibp,ik,i_sp_pol)
     c_ws(:,1)=V(:,i_sp_pol)*WF%c(:,ifftp)
     !
     H(ib,ibp) = H(ib,ibp)+Vstar_dot_V(fft_size,WF%c(:,ifft),c_ws(:,1))
     !
   elseif (n_spinor==2) then
     !
     if (V_kind=='def') then
       !
       ifft   =WF%index(ib,ik,1)
       ifftp  =WF%index(ibp,ik,1)
       c_ws(:,1)=V(:,1)*WF%c(:,ifftp)
       H(ib,ibp) = H(ib,ibp)+Vstar_dot_V(fft_size,WF%c(:,ifft),c_ws(:,1))
       ifft   =WF%index(ib,ik,2)
       ifftp  =WF%index(ibp,ik,2)
       c_ws(:,2)=V(:,2)*WF%c(:,ifftp)
       H(ib,ibp) = H(ib,ibp)+Vstar_dot_V(fft_size,WF%c(:,ifft),c_ws(:,2))
       !
     elseif(V_kind=='xc ') then
       !
       ifft   =WF%index(ib,ik,1)
       !
       ifftp  =WF%index(ibp,ik,1)
       c_ws(:,1)=V_mat(:,1,1)*WF%c(:,ifftp)
       ifftp  =WF%index(ibp,ik,2)
       c_ws(:,2)=V_mat(:,1,2)*WF%c(:,ifftp)
       H(ib,ibp) = H(ib,ibp)+Vstar_dot_V(fft_size,WF%c(:,ifft),c_ws(:,1))+&
&                                Vstar_dot_V(fft_size,WF%c(:,ifft),c_ws(:,2))
       !
       ifft   =WF%index(ib,ik,2)
       !
       ifftp  =WF%index(ibp,ik,1)
       c_ws(:,1)=V_mat(:,2,1)*WF%c(:,ifftp)
       ifftp  =WF%index(ibp,ik,2)
       c_ws(:,2)=V_mat(:,2,2)*WF%c(:,ifftp)
       H(ib,ibp) = H(ib,ibp)+Vstar_dot_V(fft_size,WF%c(:,ifft),c_ws(:,1))+&
&                                Vstar_dot_V(fft_size,WF%c(:,ifft),c_ws(:,2))
       !
     endif
     !
   endif
   !
 enddo
 !
 ! ... and symmetrize
 !
 do ib=1,QP_nb
   H(ib,ib)=real(H(ib,ib),SP)
   do ibp=1,ib-1
     H(ib,ibp)=conjg(H(ibp,ib))
   enddo
 enddo
 !
#if defined _TIMING
 call timing('V_real_space_to_H',OPR='stop')
#endif
 !
end subroutine 

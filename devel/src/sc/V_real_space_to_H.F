!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team
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
subroutine V_real_space_to_H(ik,V,H,WF)
 !
 use electrons,      ONLY:n_sp_pol,spin
 use pars,           ONLY:SP
 use FFT_m,          ONLY:fft_size
 use QP_m,           ONLY:QP_nb,QP_table,QP_n_states
 use wave_func,      ONLY:WAVEs
 use wrapper,        ONLY:Vstar_dot_V
 use matrix_operate, ONLY:matrix_linear_index
 use SC,             ONLY:SC_bands,HEAD_cpu_in_QP_group
#if defined _SC || !defined _RT
 use parallel_m,     ONLY:PAR_IND_QP
#endif
#if defined _RT
 use parallel_m,     ONLY:PAR_IND_B_mat_ordered,PAR_IND_WF_k
#endif
 implicit none
 !
 integer     :: ik,i_spin
 real(SP)    :: V(fft_size,n_sp_pol)
 complex(SP) :: H(QP_nb,QP_nb,n_sp_pol)
 type(WAVEs) :: WF
 !
 ! Work Space
 !
 integer    :: i1,ib,ibp,ifft,ifftp
 complex(SP):: c_ws(fft_size)
 !
#if defined _RT
 if (.not.PAR_IND_WF_k%element_1D(ik)) return
#endif
 !
 ! Fill ...
 !
 do i1=1,QP_n_states
   !
   if (QP_table(i1,3)/=ik) cycle
   !
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   i_spin=spin(qp_table(i1,:))
   !
   if (ib>ibp) cycle
   !
#if defined _SC && !defined _RT
   if (.not.(PAR_IND_QP%element_1D(i1).and.HEAD_cpu_in_QP_group)) cycle
#endif
#if defined _RT
   if (.not.PAR_IND_B_mat_ordered%element_1D( matrix_linear_index(ib-SC_bands(1)+1,ibp-SC_bands(1)+1)-1 )) cycle
#endif
   !
   ! <ifft|V|ifftp>
   !
   ifft   =WF%index(ib,ik,i_spin)
   ifftp  =WF%index(ibp,ik,i_spin)
   c_ws(:)=V(:,i_spin)*WF%c(:,ifftp)
   !
   H(ib,ibp,i_spin) = H(ib,ibp,i_spin)+Vstar_dot_V(fft_size,WF%c(:,ifft),c_ws(:))
   !
 enddo
 !
 ! ... and symmetrize
 !
 do i_spin=1,n_sp_pol
   do ib=1,QP_nb
     H(ib,ib,i_spin)=real(H(ib,ib,i_spin),SP)
     do ibp=1,ib-1
       H(ib,ibp,i_spin)=conjg(H(ibp,ib,i_spin))
     enddo
   enddo
 enddo
 !
end subroutine 

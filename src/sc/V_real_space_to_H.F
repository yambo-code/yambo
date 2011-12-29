!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team
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
subroutine V_real_space_to_H(ik,V,H,wf)
 !
 use electrons,      ONLY:n_sp_pol,spin
 use pars,           ONLY:SP,DP
 use FFT_m,          ONLY:fft_size
 use QP_m,           ONLY:QP_nb,QP_table,QP_n_states
 use wave_func,      ONLY:wf_state,wf_n_states
 use wrapper,        ONLY:Vstar_dot_V
 implicit none
 !
 integer     :: ik,i_spin
 real(SP)    :: V(fft_size,n_sp_pol)
 complex(SP) :: H(QP_nb,QP_nb,n_sp_pol),wf(fft_size,wf_n_states)
 !
 ! Work Space
 !
 integer    :: i1,ib,ibp,ifft,ifftp
 complex(SP):: c_ws(fft_size)
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
   ! <ifft|V|ifftp>
   !
   ifft =wf_state(ib,ik,i_spin)
   ifftp=wf_state(ibp,ik,i_spin)
   c_ws(:)=V(:,i_spin)*wf(:,ifftp)
   !
   H(ib,ibp,i_spin) = H(ib,ibp,i_spin) + Vstar_dot_V(fft_size,wf(:,ifft),c_ws(:))
   !
 end do
 !
 ! ... and symmetrize
 !
 do ib=1,QP_nb
   H(ib,ib,i_spin)=real(H(ib,ib,i_spin),SP)
   do ibp=1,ib-1
     H(ib,ibp,i_spin)=conjg(H(ibp,ib,i_spin))
   enddo
 enddo
 !
end subroutine 

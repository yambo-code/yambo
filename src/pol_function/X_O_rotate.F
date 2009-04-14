!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine X_O_rotate(X_bands,use_trans_gauge,E)
 !
 use pars,                ONLY:SP
 use units,               ONLY:HARTREE
 use R_lattice,           ONLY:nXkibz
 use electrons,           ONLY:levels
 use SC,                  ONLY:SC_bands,SC_R,load_SC_components,H_nl_sc,H_rotate
 use X_m,                 ONLY:X_q0grad
 use IO_m,                ONLY:NONE,DUMP
 use memory_m,            ONLY:mem_est
 implicit none
 !!!!!!
 !WARNING: The SC variables H_nl_sc,SC_R have been generalized for spin
 !polarized calculation. Here the spin index is set to 1 ...Daniele and Maurizia
 !!!!!!
 integer      :: X_bands
 logical      :: use_trans_gauge
 type(levels) :: E
 !
 ! Work Space
 !
 integer     :: ik,ic,it,ib,ibp
 real(SP)    :: delta_E(X_bands,X_bands),delta_E_tilde(X_bands,X_bands)
 complex(SP) :: D_WS_n(X_bands,X_bands),D_WS_nm1(X_bands,X_bands)
 !
 ! First I read the rotation matrices
 !                  -----------------
 !
 return
 !
 call load_SC_components('WF',COM_=NONE,MODE_=DUMP)
 !
 ! and the header of the H_sc
 !
 call load_SC_components('Hsc',COM_=NONE,MODE_=DUMP,ik=0)
 !
 do ik=1,nXkibz
   !
   call load_SC_components('Hsc',COM_=NONE,MODE_=DUMP,ik=ik)
   !
   ! Rotate H_nl_sc in the NEW basis.
   !
   call H_rotate(SC_R(:,:,ik,1),H_nl_sc(:,:,1),SC_bands,1)
   !
   do ib=1,X_bands
     do ibp=1,X_bands
       !
       delta_E(ib,ibp)=E%E(ib,ik,1)-E%E(ibp,ik,1)
       if (associated(E%Eo)) delta_E(ib,ibp)=E%Eo(ib,ik,1)-E%Eo(ibp,ik,1)
       !
       delta_E_tilde(ib,ibp)=delta_E(ib,ibp)+H_nl_sc(ibp,ibp,1)-H_nl_sc(ib,ib,1)
       !
       if (abs(delta_E(ib,ibp))<=1.E-5_SP) then
         delta_E(ib,ibp)=1.
         delta_E_tilde(ib,ibp)=1.
       endif
       !
     enddo
     !
   enddo
   !
   ! Zero the diagonal of the H_sc
   !
   forall (ib=1:X_bands) H_nl_sc(ib,ib,1)=(0.,0.)
   !
   do ic=1,3
     !
     ! Remember that X_q0grad(n,n') = i <n'|r|n>
     ! 
     forall (ib=1:X_bands,ibp=1:X_bands) D_WS_nm1(ib,ibp)=&
&           X_q0grad(ic,ibp,ib,ik,1)*delta_E(ib,ibp)/delta_E_tilde(ib,ibp) 
     D_WS_n=D_WS_nm1
     !
     do it=1,0
       !
       !  i <i|r|j>_n = (<i|p|j> - i <i|r|k>_n-1 \tilde{H}_kl + 
       !                           i \tilde{H}_ik <k|r|j>_n-1 )/(D_ij-H_ii+H_jj)
       !
       ! where \tilde{H} has zero diagonal terms
       !
       D_WS_n(:,:)=-matmul(D_WS_nm1(:,:),H_nl_sc(:X_bands,:X_bands,1))+&
&                   matmul(H_nl_sc(:X_bands,:X_bands,1),D_WS_nm1(:,:))

       !
       forall (ib=1:X_bands,ibp=1:X_bands) D_WS_n(ib,ibp)=&
&             X_q0grad(ic,ibp,ib,ik,1)*delta_E(ib,ibp)/delta_E_tilde(ib,ibp)+&
&             D_WS_n(ib,ibp)/delta_E_tilde(ib,ibp)
       !
       D_WS_nm1=D_WS_n
       !
     enddo 
     !
     forall (ib=1:X_bands,ibp=1:X_bands) X_q0grad(ic,ib,ibp,ik,1)=D_WS_n(ibp,ib)
     !
   enddo
   !
 enddo
 !
 call load_SC_components('WF_clean')
 call load_SC_components('Hsc_clean')
 !
end subroutine

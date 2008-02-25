!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 use pars,                ONLY:SP,HARTREE
 use R_lattice,           ONLY:nXkibz
 use electrons,           ONLY:levels
 use SC,                  ONLY:SC_bands,SC_R,load_SC_components,H_nl_sc,H_rotate
 use X_m,                 ONLY:X_q0grad
 use IO_m,                ONLY:NONE,DUMP
 use memory_m,            ONLY:mem_est
 implicit none
 !
 integer      :: X_bands
 logical      :: use_trans_gauge
 type(levels) :: E
 !
 ! Work Space
 !
 integer     :: ik,ic,it,ib,ibp
 real(SP)    :: delta_E(X_bands,X_bands)
 complex(SP) :: grad_WS_n(3,X_bands,X_bands),grad_WS_nm1(3,X_bands,X_bands)
 !
 ! First I read the rotation matrices
 !                  -----------------
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
   call H_rotate(SC_R(:,:,ik),H_nl_sc,SC_bands,1)
   !
   grad_WS_nm1(:,:,:)=(0.,0.)
   !
   do ib=1,X_bands
     do ibp=1,X_bands
       !
       delta_E(ib,ibp)=E%E(ib,ik,1)-E%E(ibp,ik,1)
       if (associated(E%Eo)) delta_E(ib,ibp)=E%Eo(ib,ik,1)-E%Eo(ibp,ik,1)
       !
       if (abs(delta_E(ib,ibp))<=1.E-5_SP) cycle
       !
     enddo
     !
   enddo
   !
   ! Remember that X_q0grad(n,n') = i <n'|r|n>
   ! 
   forall (ic=1:3,ib=1:X_bands,ibp=1:X_bands) grad_WS_nm1(ic,ib,ibp)=&
&          X_q0grad(ic,ibp,ib,ik,1)*delta_E(ib,ibp)/(delta_E(ib,ibp)+H_nl_sc(ibp,ibp)-H_nl_sc(ib,ib))
   !
   ! Zero the diagonal of the H_sc and move it in delta_E
   !
   forall (ib=1:X_bands) H_nl_sc(ib,ib)=(0.,0.)
   forall (ib=1:X_bands,ibp=1:X_bands) delta_E(ib,ibp)=delta_E(ib,ibp)+H_nl_sc(ibp,ibp)-H_nl_sc(ib,ib)
   !
   do ic=1,3
     !
     do it=1,0
       !
       !  i <i|r|j>_n = (<i|p|j> - i <i|r|k>_n-1 \tilde{H}_kl + i \tilde{H}_ik <k|r|j>_n-1 )/(D_ij+H_ii-H_jj)
       !
       ! where \tilde{H} has zero diagonal
       !
       grad_WS_n(ic,:,:)=-matmul(grad_WS_nm1(ic,:,:),H_nl_sc(:X_bands,:X_bands))+matmul(H_nl_sc(:X_bands,:X_bands),grad_WS_nm1(ic,:,:))
       !
       forall (ib=1:X_bands,ibp=1:X_bands) grad_WS_n(ic,ib,ibp)=grad_WS_n(ic,ib,ibp)/delta_E(ib,ibp)+grad_WS_nm1(ic,ib,ibp)
       !
     enddo 
     !
     forall (ib=1:X_bands,ibp=1:X_bands) X_q0grad(ic,ib,ibp,ik,1)=grad_WS_nm1(ic,ibp,ib)
     !
   enddo
   !
 enddo
 !
 call load_SC_components('WF_clean')
 call load_SC_components('Hsc_clean')
 !
end subroutine

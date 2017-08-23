!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV
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
subroutine QP_load_SC_xc() 
 !
 ! Load and Rotate non-local Hamiltonian coming from SC calculation 
 !
 use electrons,       ONLY:spin,n_sp_pol
 use QP_m,            ONLY:QP_table,QP_n_states,QP_vnl_xc
 use IO_m,            ONLY:VERIFY,DUMP,NONE
 use R_lattice,       ONLY:nkibz
 use SC,              ONLY:SC_bands,load_SC_components,SC_R,H_rotate,compatible_SC_DB,&
&                          found_SC_DB
 use hamiltonian,     ONLY:H_nl_sc
 use interfaces,      ONLY:LINEAR_ALGEBRA_driver
 use linear_algebra,  ONLY:INV
 !
 implicit none
 !
 ! Work Space
 !
 integer  :: i_qp,i_k,i_sp_pol
 !
 ! Load SC_R rotation matrices
 !
 call load_SC_components('WF',COM_=NONE,MODE_=DUMP)
 !
 do i_sp_pol=1,n_sp_pol
   do i_k=1,nkibz
     !
     if (.not.any(QP_table(:,3)==i_k)  ) cycle 
     !
     call load_SC_components('Hsc',COM_=NONE,MODE_=VERIFY,ik=i_k,i_sp_pol=i_sp_pol)
     !
     if (.not.compatible_SC_DB.or..not.found_SC_DB) return
     !
     call LINEAR_ALGEBRA_driver(INV,M=SC_R(:,:,i_k,i_sp_pol))
     call H_rotate(SC_R(:,:,i_k,i_sp_pol),H_nl_sc,SC_bands(2),-1)
     !
     do i_qp=1,QP_n_states
       if (.not.QP_table(i_qp,3) ==i_k)      cycle
       if (spin(QP_table(i_qp,:))==i_sp_pol) cycle
       QP_vnl_xc(i_qp)=QP_vnl_xc(i_qp)-H_nl_sc(QP_table(i_qp,1),QP_table(i_qp,2))
     enddo
     !
   enddo
 enddo
 !
 ! Deallocations
 !
 call load_SC_components('WF_clean')
 call load_SC_components('Hsc_clean')
 !
end subroutine

!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_update_G_reference(it,Xen,G_lesser_reference)
 !
 ! This subroutine update G_reference in order to converge
 ! to the groud state G_lesser of a given Hamiltonian
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels,spin_occ
 use real_time,      ONLY:H_t,RT_is_dumped,NE_i_time,RT_diagonalization_steps
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK 
 use SC,             ONLY:SC_bands,H_rotate
 use QP_m,           ONLY:QP_nk
 !
 implicit none
 !
 integer,       intent(in)     :: it
 type(levels),  intent(inout)  :: Xen
 complex(SP) ,  intent(out)    :: G_lesser_reference(SC_bands,SC_bands,QP_nk)
 !
 ! Work Space
 !
 integer :: ik,ib
 !
 if(any((/.not.mod(it,RT_diagonalization_steps)==0,.not.RT_is_dumped,NE_i_time>=0/))) return
 !
 do ik=1,Xen%nk
    !
    call mat_dia_inv(DIAGO,USE_LK,H_t(1:SC_bands,1:SC_bands,ik,1),E_real=Xen%E(:SC_bands,ik,1))
    !
    ! It works only at T=0 ! 
    !
    G_lesser_reference(:,:,ik)=(0._SP,0._SP)
    !
    forall(ib=1:Xen%nbm)
      G_lesser_reference(ib,ib,ik)=(0._SP,1._SP)*spin_occ
    end forall   
    !
    call H_rotate(H_t(1:SC_bands,1:SC_bands,ik,1),G_lesser_reference(1:SC_bands,1:SC_bands,ik),SC_bands,-1)
    !
  enddo
  !
end subroutine RT_update_G_reference

!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine QP_load_GFs(bands,qp,E)
 !
 ! Using qp%GreenF/qp%GreenF_W I define here the Green Functions
 !
 use electrons,     ONLY:n_sp_pol,spin,levels
 use QP_m,          ONLY:QP_t,GF_is_causal
 !
#include<memory.h>
 !
 type(levels) ::E
 type(QP_t)   ::qp
 integer      ::bands(2)
 !
 ! Work Space
 !
 integer     :: i_qp,i_band,i_k_pt,i_spin
 !
 E%GreenF_is_causal=GF_is_causal
 E%GreenF_n_E_steps=qp%GreenF_n_steps
 !
 YAMBO_ALLOC(E%GreenF,(bands(2),E%nk,n_sp_pol,E%GreenF_n_E_steps))
 YAMBO_ALLOC(E%GreenF_W,(bands(2),E%nk,n_sp_pol,E%GreenF_n_E_steps))
 !
 do i_qp=1,qp%n_states
   i_band=qp%table(i_qp,1)
   i_k_pt=qp%table(i_qp,3)
   i_spin=spin(qp%table(i_qp,:))
   E%GreenF(i_band,i_k_pt,i_spin,:)=qp%GreenF(i_qp,:)
   E%GreenF_W(i_band,i_k_pt,i_spin,:)=qp%GreenF_W(i_qp,:)
 enddo
 !
end subroutine

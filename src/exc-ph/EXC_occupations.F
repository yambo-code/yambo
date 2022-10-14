!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
! Excitonic occupation, following PRB 99, 081109(R) 2019
!
subroutine EXC_occupations(E_QF,k,q,iq_ibz,EXC_n_states,EXC_indexes,EXC_occ)
 !
 use pars,           ONLY:SP,cZERO,rZERO,cONE
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:qindx_C
 use R_lattice,      ONLY:bz_samp
 use BS,             ONLY:BS_H_dim,BS_bands
 use BS_solvers,     ONLY:BSS_eh_table,BS_mat
 !
#include<memory.h>
 !
 type(levels), intent(in)  :: E_QF
 type(bz_samp),intent(in)  :: k,q
 integer,      intent(in)  :: EXC_n_states,iq_ibz
 integer,      intent(in)  :: EXC_indexes(EXC_n_states)
 real(SP),     intent(out) :: EXC_occ(EXC_n_states)
 !
 ! Work space
 !
 integer                   :: i_H,iv,ic,i_l,i_c,ik_bz,ik_ibz,ik_minus_iq_bz,ik_minus_iq_ibz,iq_bz
 real(SP)                  :: el_occ
 !
 EXC_occ=rZERO
 !
 do i_H=1,BS_H_dim
   !
   ! K = (ik_bz,iv,ic)
   !
   ik_bz  = BSS_eh_table(i_H,1)
   ik_ibz = k%sstar(ik_bz,1)
   !
   iv    = BSS_eh_table(i_H,2)
   ic    = BSS_eh_table(i_H,3)
   !
   ! In the BSE at finite q we connect (v, k-q) ==> (c, k)
   !
   iq_bz = q%k_table(iq_ibz,1)
   ik_minus_iq_bz  = qindx_C(ik_bz,iq_bz,1)      ! K_bz-Q_bz
   ik_minus_iq_ibz = k%sstar(ik_minus_iq_bz,1)
   !
   el_occ=E_qf%f(ic,ik_ibz,1)*(spin_occ-E_qf%f(iv,ik_minus_iq_ibz,1))
   !
   do i_c=1,EXC_n_states
      i_l=EXC_indexes(i_c)
      EXC_occ(i_c)=EXC_occ(i_c)+abs(BS_mat(i_H,i_l))**2*el_occ
   enddo
   !
 enddo
 !
end subroutine

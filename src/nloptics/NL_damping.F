!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA
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
subroutine NL_damping(E,V_bands,ik,i_sp_pol)
 !
 ! Output: I_relax
 !
 use pars,           ONLY:SP,cI,cZERO
 use zeros,          ONLY:zero_dfl
 use electrons,      ONLY:levels,n_sp_pol
 use QP_m,           ONLY:QP_nk
 use real_time,      ONLY:Phase_LifeTime
 use nl_optics,      ONLY:I_relax,NL_bands
 use parallel_m,     ONLY:PAR_Xk_ibz_index
 !
 implicit none
 !                      
 type(levels), intent(in) :: E
 complex(SP),  intent(in) :: V_bands(NL_bands(2),E%nbf,QP_nk,n_sp_pol)
 integer,      intent(in) :: ik,i_sp_pol
 !
 ! Working Space
 !
 integer          :: ib,i1,i2,ik_mem
 !
 ! Damping
 !=========
 !
 ik_mem=PAR_Xk_ibz_index(ik)
 !
 ! Fictitious lifetime equivalent to the broadening
 ! in linear optics
 !
 if(Phase_LifeTime<zero_dfl) return
 !
 ! the |V_b(t)><V_b(t)| term
 !
 forall(ib=1:E%nbf,i1=NL_bands(1):NL_bands(2),i2=NL_bands(1):NL_bands(2))
   I_relax(i1,i2,ib,ik_mem,i_sp_pol) = & 
       & -cI*2._SP/Phase_LifeTime*(conjg(V_bands(i2,ib,ik,i_sp_pol))*V_bands(i1,ib,ik,i_sp_pol))
 end forall
 !
 ! the |V_b(t=0)><V_b(t=0)| term 
 !
 forall(ib=NL_bands(1):E%nbf) &
&     I_relax(ib,ib,ib,ik_mem,i_sp_pol)=I_relax(ib,ib,ib,ik_mem,i_sp_pol)+cI*2._SP/Phase_LifeTime 
 !
end subroutine NL_damping


!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine QP_expand(k,qp,qp_expanded)
 !
 use pars,          ONLY:SP
 use QP_m,          ONLY:QP_t
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:n_sp_pol
#include<memory.h>
 !
 type(bz_samp)    ::k
 type(QP_t)       ::qp,qp_expanded
 !
 ! Work Space
 !
 integer :: i_qp,ik,ib(2),is,ik_bz,n_qp_expanded,i1,max_ik
 !
 qp_expanded%n_descs=qp%n_descs
 qp_expanded%nb=qp%nb
 qp_expanded%description=qp%description
 !
 do i_qp=1,qp_expanded%n_descs
   if (index(qp_expanded%description(i_qp),"QP @ K")>0) then
     qp_expanded%description(i_qp)=trim(qp_expanded%description(i_qp))//" [BZ expanded]"
   endif
 enddo
 !
 call k_ibz2bz(k,'i',.false.)
 !
 do i1=1,2
   !
   n_qp_expanded=0
   max_ik=-1
   !
   do i_qp=1,qp%n_states
     ib=qp%table(i_qp,1:2)
     ik=qp%table(i_qp,3)
     do is=1,k%nstar(ik)
       if (ik>1 ) ik_bz=sum(k%nstar(1:ik-1))+is
       if (ik==1) ik_bz=is
       n_qp_expanded=n_qp_expanded+1
       !
       max_ik=max(ik_bz,max_ik)
       !
       if (i1==2) then
         qp_expanded%k(ik_bz,:)=k%ptbz(ik_bz,:)
         qp_expanded%wk(ik_bz)= 1._SP/real(k%nbz,SP)
         qp_expanded%table(n_qp_expanded,:3)=(/ib,ik_bz/)
         if (n_sp_pol==2) qp_expanded%table(n_qp_expanded,4)=qp%table(i_qp,4)
         qp_expanded%E_bare(n_qp_expanded)=qp%E_bare(i_qp)
         qp_expanded%E(n_qp_expanded)=qp%E(i_qp)
         qp_expanded%Z(n_qp_expanded)=qp%Z(i_qp)
       endif
       !
     enddo
   enddo
   !
   if (i1==1) then
     !
     qp_expanded%nk=max_ik
     qp_expanded%n_states=n_qp_expanded
     !
     YAMBO_ALLOC(qp_expanded%wk,(max_ik))
     YAMBO_ALLOC(qp_expanded%k,(max_ik,3))
     YAMBO_ALLOC(qp_expanded%table,(qp_expanded%n_states,3+n_sp_pol-1))
     YAMBO_ALLOC(qp_expanded%E_bare,(qp_expanded%n_states))
     YAMBO_ALLOC(qp_expanded%E,(qp_expanded%n_states))
     YAMBO_ALLOC(qp_expanded%Z,(qp_expanded%n_states))
     !
   endif
   !
 enddo
 !
end subroutine

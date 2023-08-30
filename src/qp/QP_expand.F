!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QP_expand(k,qp,qp_expanded)
 !
 use pars,          ONLY:SP
 use QP_m,          ONLY:QP_t
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:n_sp_pol
 use descriptors,   ONLY:IO_desc_duplicate
 !
#include<memory.h>
 !
 type(bz_samp)    ::k
 type(QP_t)       ::qp,qp_expanded
 !
 ! Work Space
 !
 integer :: i_qp,ik,ib(2),is,ik_bz,n_qp_expanded,i1,max_ik
 !
 call IO_desc_duplicate(qp%desc,qp_expanded%desc)
 !
 do i_qp=1,qp_expanded%desc%n
   if (index(qp_expanded%desc%str(i_qp),"kind")>0) then
     qp_expanded%desc%kind(i_qp)=" "
     qp_expanded%desc%size(i_qp)=0
   endif
   if (index(qp_expanded%desc%str(i_qp),"QP @ state")>0) then
     qp_expanded%desc%str(i_qp)=trim(qp_expanded%desc%str(i_qp))//" [BZ expanded]"
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

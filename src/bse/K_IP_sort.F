!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine K_IP_sort(what,I,N,El,vC)
 !
 use pars,          ONLY:SP,cZERO
 use parallel_m,    ONLY:PAR_COM_eh_INDEX,PAR_COM_Xk_ibz_INDEX
 use X_m,           ONLY:BS_E_sorted,N_BS_E_sorted
 use BS,            ONLY:BS_H_dim
 use vec_operate,   ONLY:sort,degeneration_finder
 use units,         ONLY:HA2EV
 use parallel_int,  ONLY:PP_redux_wait
 !
#include<memory.h>
 !
 character(*)  ::what
 integer       ::I,N
 complex(SP)   ::El,vC(BS_H_dim,N_BS_E_sorted)
 !
 ! Work space
 !
 integer                  :: EDIM,Ntmp,i_VAR,i_cv,i_grp,n_deg_grp
 real(SP),    allocatable :: vR(:)
 integer,     allocatable :: sort_indx(:),first_el(:),n_of_el(:)
 !
 if (what=="add") then
   N=N+1
   vC(I,N)=El
   return
 endif
 !
 call PP_redux_wait(vC,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(vC,COMM=PAR_COM_eh_INDEX%COMM )
 !
 YAMBO_ALLOC(vR,(BS_H_dim)) 
 YAMBO_ALLOC(sort_indx,(BS_H_dim))
 !
 ! Sort the energies
 vR=real(vC(:,1),SP)
 call sort(arrin=vR,indx=sort_indx)
 !
 EDIM=min(BS_H_dim,10000)
 Ntmp=1
 if(EDIM>1000) Ntmp=10
 allocate(first_el(EDIM)) 
 allocate(n_of_el(EDIM))
 !
 ! Threshold is 1meV
 call degeneration_finder(vR(:EDIM),EDIM,first_el,n_of_el,n_deg_grp,0.001_SP/HA2EV,Include_single_values=.TRUE.)
 !
 do i_VAR=1,n_BS_E_sorted
   BS_E_sorted(:,i_VAR,1)=cZERO
   do i_grp=1,n_deg_grp
     if(i_grp>EDIM/Ntmp) cycle
     if(i_VAR==2) then
       BS_E_sorted(i_grp,i_VAR,1)=real(n_of_el(i_grp),SP)
       cycle
     endif
     do i_cv=first_el(i_grp),first_el(i_grp)+n_of_el(i_grp)-1
       BS_E_sorted(i_grp,i_VAR,1)=BS_E_sorted(i_grp,i_VAR,1)+vC(sort_indx(i_cv),i_VAR)
     enddo
     BS_E_sorted(i_grp,i_VAR,1)=BS_E_sorted(i_grp,i_VAR,1)/real(n_of_el(i_grp),SP)
   enddo
 enddo
 !
 ! Free
 deallocate(first_el) 
 deallocate(n_of_el)
 YAMBO_FREE(vR)
 YAMBO_FREE(sort_indx)
 !
end subroutine

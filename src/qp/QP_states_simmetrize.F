!
! License-Identifier: GPL
!
! Copyright (C) 2007 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
!@brief Enforce symmetry between degenerate states 
!       it may induce errors in case of accidental degeneracies
!
subroutine QP_states_simmetrize(en,what,V_real,V_complex,state_is_2do,warn_me)
 !
 use pars,          ONLY:SP,schlen
 use QP_m,          ONLY:QP_n_states,QP_table
 use units,         ONLY:HA2EV
 use vec_operate,   ONLY:sort,degeneration_finder
 use electrons,     ONLY:levels,spin,n_sp_pol
 use stderr,        ONLY:real2ch,intc
 !
 implicit none
 !
 type(levels)          ::en
 character(*),optional ::what
 real(SP),    optional ::V_real(:)
 complex(SP), optional ::V_complex(:)
 logical,     optional ::state_is_2do(:),warn_me
 !
 ! Work Space
 !
 real(SP)    ::Eo_sorted(QP_n_states),r_sum,max_percentual_diff(QP_n_states),DIFF
 complex(SP) ::c_sum
 integer     ::Eo_sorted_index(QP_n_states),i1,inx,i2,inx_first,&
&              first_el(QP_n_states),n_of_el(QP_n_states),n_deg_grp
 character(schlen)   :: title=" ",states_ch=" "
 logical             :: check_the_diffs=.FALSE.
 real(SP), parameter :: E_deg_treshold=0.0001_SP/HA2EV,V_deg_percentual_treshold=0.01
 !
 if (present(what))    title=what
 if (present(warn_me)) check_the_diffs=warn_me
 !
 ! Degenerate bands average.
 !
 do i1=1,QP_n_states
   Eo_sorted(i1)=en%E(QP_table(i1,1),QP_table(i1,3),spin(QP_table(i1,:)))
 enddo
 !
 call sort(Eo_sorted,indx=Eo_sorted_index)
 call degeneration_finder(Eo_sorted,QP_n_states,first_el,n_of_el,n_deg_grp,E_deg_treshold)
 !
 if (present(state_is_2do)) then
   state_is_2do=.TRUE.
 endif
 !
 do i1=1,n_deg_grp
   !
   r_sum=0._SP
   c_sum=(0._SP,0._SP)
   !
   inx_first =Eo_sorted_index( first_el(i1) )
   !
   states_ch="(n,k)="
   if (n_sp_pol>1) states_ch="(n,k,s)="
   !
   do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
     !
     inx       =Eo_sorted_index(i2)
     !
     if (spin(QP_table(inx_first,:))/=spin(QP_table(inx,:))) cycle
     !
     if (present(state_is_2do)) then
       if (i2>first_el(i1)) state_is_2do(inx)=.FALSE.
     endif
     !
     max_percentual_diff(i1)=0._SP
     !
     states_ch=trim(states_ch)//"("//trim(intc(QP_table(inx,1)))//","//trim(intc(QP_table(inx,3)))
     if (n_sp_pol>1) then
       if (spin(QP_table(inx,:))==1) states_ch=trim(states_ch)//",up"
       if (spin(QP_table(inx,:))==2) states_ch=trim(states_ch)//",dn"
     endif
     states_ch=trim(states_ch)//")"
     if (i2<first_el(i1)+n_of_el(i1)-1) states_ch=trim(states_ch)//"-"
     !
     if (present(V_real)) then
       if (check_the_diffs.and.abs(V_real(inx_first))>0._SP) then 
         DIFF=(V_real(inx)-V_real(inx_first))/real( n_of_el(i1) )/V_real(inx_first)
       endif
       r_sum=r_sum+V_real(inx)/real( n_of_el(i1) )
     endif
     if (present(V_complex)) then
       if (check_the_diffs.and.abs(V_complex(inx_first))>0._SP) then 
         DIFF=abs(V_complex(inx)-V_complex(inx_first))/real( n_of_el(i1) )/abs(V_complex(inx_first))
       endif
       c_sum=c_sum+V_complex(inx)/real( n_of_el(i1) )
     endif
     if (check_the_diffs)  max_percentual_diff(i1)=max_percentual_diff(i1)+DIFF
     !
   enddo
   !
   do i2=first_el(i1),first_el(i1)+n_of_el(i1)-1
     !
     inx   =Eo_sorted_index(i2)
     !
     if (present(V_real)) then
       V_real(inx)=r_sum
     endif
     if (present(V_complex)) then
       V_complex(inx)=c_sum
     endif
     !
   enddo
   !
   if (.not.check_the_diffs) cycle
   !
   if (max_percentual_diff(i1)<V_deg_percentual_treshold) cycle
   !
   call warning("Detected a degeneracy breakdown induced by "//trim(title)//" of "//&
&               trim(real2ch(max_percentual_diff(i1)*100.))//"% in states "//trim(states_ch))
   !
 enddo
 !
end subroutine

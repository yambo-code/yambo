!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QP_apply_the_ACTION(i_ACTION,bands,E,k)
 !
 ! Here I apply the strecth for both QP_action.
 !
 use pars,          ONLY:SP,rZERO,cONE,rONE
 use electrons,     ONLY:levels,n_sp_pol
 use R_lattice,     ONLY:bz_samp
 use QP_CTL_m,      ONLY:QP_action,QP_ctl_xc_STRING,At_least_one_band_corrected
 !
#include<memory.h>
 !
 type(levels)    ::E
 type(bz_samp)   ::k
 integer         ::i_ACTION,bands(2)
 !
 ! Work Space
 !
 integer  :: ib,ik,ik_fg,i_spin
 logical  :: do_E(n_sp_pol),do_W(n_sp_pol),do_Z(n_sp_pol),val
 real(SP) :: E_ref(2),Eo
 complex(SP), external ::QP_eval_the_FIT
 !
 if (.not.QP_action(i_ACTION)%active) return
 !
 E_ref=0._SP
 !
 do i_spin= 1,n_sp_pol
   !
   ! Is there anything to do ?
   !
   do_W(i_spin)=.false.
   do_Z(i_spin)=.false.
   do_E(i_spin)= any(QP_action(i_ACTION)%CTL(i_spin)%E/=(/rZERO,rONE,rZERO,rONE/))
   !
   E_ref=QP_action(i_ACTION)%CTL(i_spin)%E_new_ref
   !
   if (any((/QP_action(i_ACTION)%CTL(i_spin)%W(:)/=rZERO/)).or.&
&      any((/QP_action(i_ACTION)%CTL(i_spin)%W_dos(:)/=rZERO/))) then
     if (.not.allocated(E%W)) then
       YAMBO_ALLOC(E%W,(E%nb,E%nk,n_sp_pol))
       E%W=rZERO
     endif
     do_W(i_spin)=.TRUE.
   endif
   if (QP_action(i_ACTION)%CTL(i_spin)%Z/=cONE) then
     if (.not.allocated(E%Z)) then
       YAMBO_ALLOC(E%Z,(E%nb,E%nk,n_sp_pol))
       E%Z=1._SP
     endif
     do_Z(i_spin)=.TRUE.
   endif
   !
   if (any((/do_E(i_spin),do_W(i_spin),do_Z(i_spin)/))) then
     if (.not.allocated(E%Eo)) then
       YAMBO_ALLOC(E%Eo,(E%nb,E%nk,n_sp_pol))
       E%Eo=E%E
     endif
   endif
   !
   do ik=1,E%nk
     !
     do ib=bands(1),bands(2)
       !
       if (allocated(QP_action(i_ACTION)%STATES_done)) then
          if (QP_action(i_ACTION)%STATES_done(ib,ik,i_spin)==1) cycle
       endif
       !
       if (.not.At_least_one_band_corrected) At_least_one_band_corrected=do_E(i_spin)
       !
       Eo=E%Eo(ib,ik,i_spin)
       val=Eo<=E%E_VBM(i_spin)+1.E-5
       if (do_E(i_spin)) E%E(ib,ik,i_spin)=E%E(ib,ik,i_spin)+QP_eval_the_FIT(i_ACTION,i_spin,Eo,E_ref,bands,E,k,"E",val)
       if (do_W(i_spin)) E%W(ib,ik,i_spin)=E%W(ib,ik,i_spin)+QP_eval_the_FIT(i_ACTION,i_spin,Eo,E_ref,bands,E,k,"W",val)
       if (do_Z(i_spin)) E%Z(ib,ik,i_spin)=                  QP_eval_the_FIT(i_ACTION,i_spin,Eo,E_ref,bands,E,k,"Z",val)
       !
       if(allocated(E%FG%E).and.ib<=E%FG%nb) then
         do ik_fg=k%FGbare%k_range(ik,1),k%FGbare%k_range(ik,2)
           if (do_E(i_spin)) E%FG%E(ib,ik_fg,i_spin)=E%FG%E(ib,ik_fg,i_spin)+&
&                              QP_eval_the_FIT(i_ACTION,i_spin,E%FG%E(ib,ik_fg,i_spin),E_ref,bands,E,k,"E",val)
           if (do_W(i_spin)) E%FG%W(ib,ik_fg,i_spin)=E%FG%W(ib,ik_fg,i_spin)+&
&                              QP_eval_the_FIT(i_ACTION,i_spin,E%FG%E(ib,ik_fg,i_spin),E_ref,bands,E,k,"W",val)
           !DEBUG>
           !if (do_Z(i_spin)) E%Z_FineGd(ib,ik_fg,i_spin)=&
           !&                  QP_eval_the_FIT(i_ACTION,i_spin,E%E_FineGd(ib,ik_fg,i_spin),E_ref,bands,E,k,"Z",val)
           !DEBUG<
         enddo
       endif
       !
     enddo
     !
     if(bands(1)>1) E%E(:bands(1)-1,ik,i_spin)=E%E(:bands(1)-1,ik,i_spin)+ &
     &                        E%E(bands(1),ik,i_spin)-E%Eo(bands(1),ik,i_spin)
     if(bands(2)<E%nb) E%E(bands(2)+1:,ik,i_spin)=E%E(bands(2)+1:,ik,i_spin)+ &
     &                        E%E(bands(2),ik,i_spin)-E%Eo(bands(2),ik,i_spin)
     !
   enddo
   !
 enddo
 !
 ! XC string update
 !
 if (.not.(any(do_E).or.any(do_W).or.any(do_Z))) return 
 !
 if (len_trim(QP_ctl_xc_STRING)>0) QP_ctl_xc_STRING=trim(QP_ctl_xc_STRING)//" +" 
 !
 if (.not.allocated(QP_action(i_ACTION)%STATES_done)) then
   QP_ctl_xc_STRING=trim(QP_ctl_xc_STRING)//" Analyt. " 
 endif
 if (any(do_E)) QP_ctl_xc_STRING=trim(QP_ctl_xc_STRING)//" E" 
 if (any(do_W)) QP_ctl_xc_STRING=trim(QP_ctl_xc_STRING)//" W" 
 if (any(do_Z)) QP_ctl_xc_STRING=trim(QP_ctl_xc_STRING)//" Z" 
 if (allocated(QP_action(i_ACTION)%STATES_done)) then
   if (any(do_E).or.any(do_W).or.any(do_Z)) QP_ctl_xc_STRING=trim(QP_ctl_xc_STRING)//" Fit" 
 endif
 !
end subroutine

!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
complex(SP) function QP_eval_the_FIT(i_ACTION,i_sp_pol,Energy,E_ref_in,bands,E,k,what,is_valence)
 !
 use pars,          ONLY:SP,cZERO
 use units,         ONLY:HA2EV
 use electrons,     ONLY:levels,n_spin,n_spinor
 use R_lattice,     ONLY:bz_samp
 use QP_CTL_m,      ONLY:QP_action
 use interfaces,    ONLY:el_density_of_states
 !
 implicit none
 !
 type(levels)    ::E
 type(bz_samp)   ::k
 integer         ::i_ACTION,i_sp_pol,bands(2)
 real(SP)        ::Energy,E_ref_in(2)
 character(1), intent(in) ::what
 logical,      intent(in) ::is_valence
 !
 ! Work Space
 !
 real(SP)    :: dos_factor(n_spin),M_factor,E_ref
 integer     :: iref,jref,i_p
 !
 ! Valence or conduction
 !-----------------------
 iref=0
 jref=0
 QP_eval_the_FIT=cZERO
 !
 ! Energy
 !========
 if (what=='E') then
   !
   if (is_valence) iref=2
   !
   ! Scissor correction
   QP_eval_the_FIT=QP_action(i_ACTION)%CTL(i_sp_pol)%E(iref+1) 
   !
   ! Stretching correction
   if (.not.is_valence) E_ref=E_ref_in(1)
   if (     is_valence) E_ref=E_ref_in(2)
   !
   M_factor=(QP_action(i_ACTION)%CTL(i_sp_pol)%E(iref+2)-1._SP) 
   QP_eval_the_FIT=QP_eval_the_FIT+M_factor*(Energy-E_ref)
   !
 endif
 !
 ! Lifetimes
 !===========
 if (what=="W") then
   !
   if (is_valence) then
     iref=3
     jref=1
     E_ref=QP_action(i_ACTION)%CTL(i_sp_pol)%W_E_ref(2)
   else
     E_ref=QP_action(i_ACTION)%CTL(i_sp_pol)%W_E_ref(1)
   endif
   !
   ! Constant correction 
   QP_eval_the_FIT=QP_action(i_ACTION)%CTL(i_sp_pol)%W(iref+1)
   !
   ! DOS proportional correction
   M_factor=QP_action(i_ACTION)%CTL(i_sp_pol)%W_dos(jref+1)
   if (abs(M_factor)>0.) then
     call el_density_of_states(k,E,Energy,0.1_SP/HA2EV,bands,dos_factor,USE_the_DbGd=.FALSE.,USE_Eo=.TRUE.)
     QP_eval_the_FIT=QP_eval_the_FIT+M_factor*dos_factor(i_sp_pol)*real(n_spinor,SP)
   endif
   !
   ! Linear/Quadratic correction
   !
   if ( (     is_valence.and.Energy<E_ref) .or. (.not.is_valence.and.Energy>E_ref) ) then
     do i_p=1,2
       M_factor=QP_action(i_ACTION)%CTL(i_sp_pol)%W(iref+1+i_p)
       QP_eval_the_FIT=QP_eval_the_FIT+M_factor*abs(Energy-E_ref)**i_p
     enddo
   endif
   !
   if (.not.is_valence) QP_eval_the_FIT=-abs(QP_eval_the_FIT)
   if (     is_valence) QP_eval_the_FIT= abs(QP_eval_the_FIT)
   !
 endif
 !
 ! Renormalization factors
 !=========================
 if (what=='Z') then
   QP_eval_the_FIT=QP_action(i_ACTION)%CTL(i_sp_pol)%Z
 endif
 !
end function

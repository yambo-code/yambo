!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine External_corrections(en,Xen,Ken,DIPen,Xk,k,X)
 !
 use drivers,       ONLY:l_optics,l_chi,l_em1s,l_em1d,l_acfdt,l_ppa,l_bss,l_gw0,l_bse,l_alda_fxc
 use pars,          ONLY:schlen
 use X_m,           ONLY:X_t
 use electrons,     ONLY:levels,n_bands,E_duplicate,n_sp_pol
 use R_lattice,     ONLY:bz_samp
 use BS,            ONLY:BS_bands
 use QP_CTL_m,      ONLY:QP_apply
 use drivers,       ONLY:l_cohsex,l_real_time,l_ppa
 use RT_control,    ONLY:RT_apply
 !
 implicit none
 !
 type(levels) ::en,Xen,Ken,DIPen
 type(X_t)    ::X(5) 
 type(bz_samp)::k,Xk
 !
 ! Work Space
 ! 
 integer  :: iq,i_sp_pol
 character(schlen) :: db_name
 !
 logical           :: l_apply_RT_corrections
 !
 logical       ::l_apply_QP_corrections,l_dynamical
 integer       ::X_kind,bands_to_correct(2)
 !
 ! 1) Screening
 ! ==============
 l_apply_QP_corrections=(l_optics.and.l_chi).or.l_em1d.or.l_em1s.or.l_acfdt
 !
 l_dynamical=(l_optics.and.l_chi) .or. l_em1d
 !
 if( l_acfdt )                      X_kind=1
 if( l_em1s  )                      X_kind=2
 if( l_dynamical .and.      l_ppa ) X_kind=3
 if( l_dynamical .and. .not.l_ppa ) X_kind=4
 if (l_apply_QP_corrections) call QP_apply(X(X_kind)%ib,Xen,Xk,"X",msg_fmt='rs',main_section=.TRUE.)
 !
 l_apply_RT_corrections=l_apply_QP_corrections.and.        &
&    (     ((l_em1s.or.l_ppa.or.l_gw0).and..not.l_real_time)   &
&      .or.(l_optics.and.l_chi)          )
 if (l_apply_RT_corrections) call RT_apply(X(X_kind)%ib,Xen,Xk,what="X",VERBOSE=.true.)
 !
 ! 2) BSE
 ! ========
 bands_to_correct=BS_bands
 !
 ! If using ALDA enough bands must be loaded to be able to calculate the density
 !          ================================
 if (l_alda_fxc) bands_to_correct=(/1,BS_bands(2)/)
 !
 l_apply_QP_corrections=(l_optics.and.l_bse.and..not.l_ppa).or.l_bss
 if (l_apply_QP_corrections) call QP_apply(BS_bands,Ken,Xk,"K",msg_fmt='rs',main_section=.TRUE.)
 !
 l_apply_RT_corrections=l_apply_QP_corrections.and..not.l_real_time
 if (l_apply_RT_corrections) call RT_apply(BS_bands,Ken,Xk,what="K",VERBOSE=.true.)
 !
 ! 3) Green's Function used to compute QP corrections
 ! ====================================================
 bands_to_correct=(/1,n_bands/)
 if (l_gw0)                  call QP_apply(bands_to_correct,en,k,"G",msg_fmt='rs',main_section=.TRUE.)
 !
 l_apply_RT_corrections=l_cohsex.or.l_ppa.or.l_gw0
 if (l_apply_RT_corrections) call RT_apply(bands_to_correct,en,k,what="G",VERBOSE=.true.)
 !
 call E_duplicate(en,DIPen)
 !
 do i_sp_pol=1,n_sp_pol
   DIPen%nbf(i_sp_pol)=minval((/en%nbf(i_sp_pol),Xen%nbf(i_sp_pol),Ken%nbf(i_sp_pol)/))
   DIPen%nbm(i_sp_pol)=maxval((/en%nbm(i_sp_pol),Xen%nbm(i_sp_pol),Ken%nbm(i_sp_pol)/))
 enddo
 !
 call section('r',' ')
 !
end subroutine

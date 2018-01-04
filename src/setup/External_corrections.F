!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine External_corrections(en,Xen,Ken,DIPen,Xk,k,X)
 !
 use drivers,       ONLY:l_optics,l_chi,l_em1s,l_em1d,l_acfdt,l_ppa,l_bss,l_gw0,l_bse,l_alda_fxc
 use X_m,           ONLY:X_t
 use electrons,     ONLY:levels,n_bands,E_duplicate
 use R_lattice,     ONLY:bz_samp
 use BS,            ONLY:BS_bands
 use QP_CTL_m,      ONLY:QP_apply
 !
 ! GPL_EXCLUDE_START
 !
#if defined _RT
 use drivers,       ONLY:l_cohsex,l_real_time
 use RT_control,    ONLY:RT_apply
#endif
 !
 ! GPL_EXCLUDE_END
 !
#if defined _ELPH
 use drivers,       ONLY:l_elph_corr
 use ELPH,          ONLY:E_k_p_q,elph_nb
#endif
 !
 implicit none
 !
 type(levels) ::en,Xen,Ken,DIPen
 type(X_t)    ::X(4) 
 type(bz_samp)::k,Xk
 !
 ! Work Space
 !
 ! GPL_EXCLUDE_START
 !
#if defined _RT
 logical       ::l_apply_RT_corrections
#endif
 !
 ! GPL_EXCLUDE_END
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
 if (l_apply_QP_corrections) call QP_apply(X(X_kind)%ib,Xen,Xk,"X",msg_fmt='rs')
 !
 ! GPL_EXCLUDE_START
 !
#if defined _RT
 l_apply_RT_corrections=l_apply_QP_corrections.and.        &
&    (     (l_em1s.and..not.l_real_time)   &
&      .or.(l_optics.and.l_chi)          )
 if (l_apply_RT_corrections) call RT_apply(X(X_kind)%ib,Xen,Xk,what="X",VERBOSE=.true.)
#endif
 !
 ! GPL_EXCLUDE_END
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
 if (l_apply_QP_corrections) call QP_apply(BS_bands,Ken,Xk,"K",msg_fmt='rs')
 !
 ! GPL_EXCLUDE_START
 !
#if defined _RT
 l_apply_RT_corrections=l_apply_QP_corrections.and..not.l_real_time
 if (l_apply_RT_corrections) then
   call RT_apply(BS_bands,Ken,Xk,what="K",VERBOSE=.true.)
   call RT_apply(BS_bands,Ken,Xk,what="R",VERBOSE=.true.)
 endif
#endif
 !
 ! GPL_EXCLUDE_END
 !
 ! 3) Green's Function used to compute QP corrections
 ! ====================================================
 bands_to_correct=(/1,n_bands/)
 if (l_gw0)                  call QP_apply(bands_to_correct,en,k,"G",msg_fmt='rs')
#if defined _ELPH
 !    Green's Function (k+q levels loaded for the _ELPH)
 if (l_gw0.and.l_elph_corr) call QP_apply((/1,elph_nb/),E_k_p_q,k,"G",msg_fmt='rs')
#endif
 !
 ! GPL_EXCLUDE_START
 !
#if defined _RT
 l_apply_RT_corrections=l_cohsex
 if (l_apply_RT_corrections) call RT_apply(bands_to_correct,en,k,what="G",VERBOSE=.true.)
#endif
 !
 ! GPL_EXCLUDE_END
 !
 call E_duplicate(en,DIPen)
 DIPen%nbf=minval((/en%nbf,Xen%nbf,Ken%nbf/))
 DIPen%nbm=maxval((/en%nbm,Xen%nbm,Ken%nbm/))
 !
 call section('r',' ')
 !
end subroutine

!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine External_corrections(en,Xen,Ken,Xk,k,X)
 !
 use drivers,       ONLY:l_optics,l_chi,l_em1s,l_em1d,l_acfdt,l_real_time,l_ppa,&
&                        l_cohsex,l_bss,l_gw0,l_elph_corr,l_bse
 use X_m,           ONLY:X_t
 use electrons,     ONLY:levels,n_bands
 use R_lattice,     ONLY:bz_samp
 use BS,            ONLY:BS_bands
 use QP_CTL_m,      ONLY:QP_apply
#if defined _RT
 use RT_CTL,        ONLY:RT_apply
#endif
#if defined _ELPH
 use ELPH,          ONLY:E_k_p_q,elph_nb
#endif
#if defined _SURF
 use ras_module,    ONLY:lras, lreels
#endif
 !
 implicit none
 !
 type(levels) ::en,Xen,Ken
 type(X_t)    ::X(4) 
 type(bz_samp)::k,Xk
 !
 ! Work Space
 !
 logical       ::l_apply_QP_corrections,l_apply_RT_corrections,l_dynamical,VERBOSE
 integer       ::X_kind,QP_bands(2)
 !
 call section('*','External corrections')
 !
 VERBOSE=.TRUE.
 !
 ! 1) Screening
 ! ==============
 l_apply_QP_corrections=(l_optics.and.l_chi).or.l_em1d.or.l_em1s.or.l_acfdt
#if defined _SURF
 l_apply_QP_corrections=l_apply_QP_corrections.or.lras.or.lreels
#endif
 !
 l_dynamical=(l_optics.and.l_chi) .or. l_em1d
#if defined _SURF
 l_dynamical=l_dynamical.or. (lras.or.lreels)
#endif
 !
 if( l_acfdt )                      X_kind=1
 if( l_em1s  )                      X_kind=2
 if( l_dynamical .and.      l_ppa ) X_kind=3
 if( l_dynamical .and. .not.l_ppa ) X_kind=4
 if (l_apply_QP_corrections) call QP_apply(X(X_kind)%ib,Xen,Xk,"X",msg_fmt='s')
#if defined _RT
 l_apply_RT_corrections=l_apply_QP_corrections.and.        &
&    (     (l_em1s.and..not.l_real_time)   &
&      .or.(l_optics.and.l_chi)          )
 if (l_apply_RT_corrections) call RT_apply(X(X_kind)%ib,Xen,Xk,what="X",VERBOSE=VERBOSE)
#endif
 !
 ! 2) BSE
 ! ========
 l_apply_QP_corrections=(l_optics.and.l_bse.and..not.l_ppa).or.l_bss
 if (l_apply_QP_corrections) call QP_apply(BS_bands,Ken,Xk,"K",msg_fmt='s')
#if defined _RT
 l_apply_RT_corrections=l_apply_QP_corrections.and..not.l_real_time
 if (l_apply_RT_corrections) call RT_apply(BS_bands,Ken,Xk,what="K",VERBOSE=VERBOSE)
#endif
 !
 ! 3) Green's Function used to compute QP corrections
 ! ====================================================
 QP_bands=(/1,n_bands/)
 if (l_gw0)                  call QP_apply(QP_bands,en,k,"G",msg_fmt='s')
#if defined _ELPH
 !    Green's Function (k+q levels loaded for the _ELPH)
 if (l_gw0.and.l_elph_corr) call QP_apply((/1,elph_nb/),E_k_p_q,k,"G",msg_fmt='s')
#endif
#if defined _RT
 l_apply_RT_corrections=l_cohsex
 if (l_apply_RT_corrections) call RT_apply(QP_bands,en,Xk,what="G",VERBOSE=VERBOSE)
#endif
 !
end subroutine

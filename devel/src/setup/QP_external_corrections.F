!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine QP_external_corrections(Xen,en,Xk,k,X)
 !
 use drivers
 use X_m,           ONLY:X_t
 use electrons,     ONLY:levels,n_bands
 use R_lattice,     ONLY:bz_samp
 use QP_CTL_m,      ONLY:QP_apply
#if defined _RT
 use rt_ctl,        ONLY:RT_OCC_LIFE_and_REF_IO
 use IO_m,          ONLY:REP
 use QP_m,          ONLY:OCC_T_ref
#endif
#if defined _ELPH
 use ELPH,          ONLY:E_k_p_q,elph_nb
#endif
#if defined _SURF
 use ras_module,    ONLY:lras, lreels
#endif
 implicit none
 !
 type(levels) ::en,Xen
 type(X_t)    ::X(4) 
 type(bz_samp)::k,Xk
 !
 ! Work Space
 !
 logical           ::apply_QP
 !
 ! GPL_EXCLUDE_START
 !
 ! - Xo -
 !
 if (l_acfdt)   call QP_apply(X(1)%ib,Xen,Xk,"X",msg_fmt='s')
 !
 ! GPL_EXCLUDE_END
 !
 ! - Dynamical X -
 !
 apply_QP=(l_optics.and.l_chi).or.(l_em1d.and..not.l_ppa).or.&
#if defined _SURF
        lras.or.lreels.or.&
#endif
        (l_gw0.and..not.l_ppa.and.l_elel_corr).or.l_life
 if (apply_QP)  call QP_apply(X(3)%ib,Xen,Xk,"X",msg_fmt='s')
#if defined _RT
 if (apply_QP)  apply_QP=.not.l_real_time
 if (apply_QP)  call RT_OCC_LIFE_and_REF_IO(Xen,'X',Xk,Time=OCC_T_ref,imposed_COM=REP)
#endif
 !
 ! - Static X -
 !
 apply_QP=l_em1s.or.(l_optics.and.l_bse.and..not.l_ppa).or.l_bss.or.(l_em1d.and.l_cohsex)
 if (apply_QP)  call QP_apply(X(2)%ib,Xen,Xk,"X",msg_fmt='s')
#if defined _RT
 if (apply_QP)  apply_QP=.not.l_real_time
 if (apply_QP)  call RT_OCC_LIFE_and_REF_IO(Xen,'X',Xk,Time=OCC_T_ref,imposed_COM=REP)
#endif
 !
 ! - Plasmon Pole X -
 !
 apply_QP=(l_em1d.and.l_ppa).or.(l_gw0.and.l_ppa.and.l_elel_corr)
 if (apply_QP)  call QP_apply(X(4)%ib,Xen,Xk,"X",msg_fmt='s')
#if defined _RT
 if (apply_QP)  apply_QP=.not.l_real_time
 if (apply_QP)  call RT_OCC_LIFE_and_REF_IO(Xen,'X',Xk,Time=OCC_T_ref,imposed_COM=REP)
#endif
 !
 ! - Green's Function (not the one used in the BSE) -
 !
 if (l_gw0)     call QP_apply((/1,n_bands/),en,k,"G",msg_fmt='s')
#if defined _RT
 if (apply_QP)  apply_QP=.not.l_real_time
 if (apply_QP)  call RT_OCC_LIFE_and_REF_IO(en,'G',k,Time=OCC_T_ref,imposed_COM=REP)
#endif
 !
#if defined _ELPH
 !
 ! - Green's Function (k+q levels loaded for the _ELPH)
 !
 if (l_gw0.and.l_elph_corr) then
   call QP_apply((/1,elph_nb/),E_k_p_q,k,"G",msg_fmt='s')
 endif
 !
#endif
 !
end subroutine

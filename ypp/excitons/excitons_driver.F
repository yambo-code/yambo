!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DV DS
!
! headers
!
#include<y_memory.h>
!
subroutine excitons_driver(k,Xk,en,Xen,q)
 !
 use pars,          ONLY:SP,pi,schlen
 use R_lattice,     ONLY:bz_samp
 use stderr,        ONLY:intc
 use electrons,     ONLY:levels,spin
 use YPP_interfaces,ONLY:excitons_sort_and_report
 use YPPm,          ONLY:l_magnons,l_sort,l_exc_wf,l_spin,l_amplitude,exc_kind,&
 &                       EXCITONS_n_user_states,EXCITONS_user_indexes,BSiq, &
 &                       BS_R_left,BS_R_right,BS_R_left_magn,BS_R_right_magn,&
 &                       BS_E,BS_E_SOC_corr,l_interp,l_dipoles,MAGNON_Res_threshold
 use BS_solvers,    ONLY:BSS_n_eig
#if defined _YPP_ELPH
 use YPP_ELPH,      ONLY:l_eliashberg,l_gkkp,l_ph_ass_dos
#endif
#if defined _YPP_RT
 use YPP_real_time, ONLY:l_RT_abs
#endif
 use y_memory_alloc
 !
 implicit none
 !
 type(bz_samp) ::Xk,k,q
 type(levels)  ::Xen,en
 !
 ! Work Space
 !
 logical              :: l_flag
 integer ,allocatable :: BS_E_degs(:)
 real(SP),allocatable :: Res(:)
 !... I/0
 integer              :: iq,io_err
 !
 l_flag=l_sort .or. l_exc_wf .or.l_amplitude .or. l_spin .or. l_interp .or. l_dipoles
#if defined _YPP_ELPH
 l_flag=l_flag .or. l_eliashberg .or. l_gkkp .or. l_ph_ass_dos
#endif
#if defined _YPP_RT
 l_flag=l_flag .or. l_RT_abs
#endif
 !
 if(.not.l_flag) return
 !
 exc_kind="exciton"
 if (l_magnons) exc_kind="magnon "
 !
 if(.not.l_interp) then
   call section('*',exc_kind//'ic Properties @ Q-index #'//trim(intc(BSiq)))
   iq= BSiq
 else if(l_interp) then
   call section('*',exc_kind//'ic Properties')
   iq = 1
 endif
 !
 call                excitons_read(k,Xk,en,Xen,BSiq,"check",io_err)
 !
 if (io_err==0) call excitons_read(k,Xk,en,Xen,BSiq,"eigenvalues",io_err)
 !
 if (io_err/=0) then
   call warning("BSE databases not found.")
#if defined _YPP_RT
   if (l_RT_abs) call RT_transient_absorption(Xen,Xk,q)
#endif
   return
 endif
 !
 YAMBO_ALLOC(Res,(BSS_n_eig))
 if (l_magnons)       Res(:)=real(BS_R_left_magn(:,1)*BS_R_right_magn(:,1),SP)
 if (.not.l_magnons)  Res(:)=real(BS_R_left(:)*BS_R_right(:),SP)
 !
 ! Sort energies and write to output
 !===================================
 if (l_sort) call excitons_sort_and_report(iq,Res,BS_E,BS_E_SOC_corr=BS_E_SOC_corr)
 !
#if defined _YPP_RT
 if (.not.l_RT_abs) then
#endif
   !
   ! Sort energies to find degenerate states
   !========================================
   YAMBO_ALLOC(BS_E_degs,(BSS_n_eig))
   call excitons_find_degeneracies(BS_E,BS_E_degs,BSS_n_eig)
   !
   ! Define the USER set of excitonic states
   !=========================================
   if(.not.l_sort) call excitons_get_user_states(BS_E_degs,Res,BSS_n_eig,.false.)
   !
#if defined _YPP_RT
 endif
#endif
 !
 ! Exciton dispersion interpolation
 ! =======================================
 if(l_interp) then
   call excitons_bands(k,Xk,en,Xen,q)
   return
 endif
 !
 ! Phonon assisted exciton density of states
 ! =========================================
#if defined _YPP_ELPH
 if(l_ph_ass_dos) then
   call excitons_ph_ass_dos(k,Xk,en,Xen,q)
   return
 endif
#endif
 !
 if(.not.l_sort)   call excitons_read(k,Xk,en,Xen,BSiq,"eigenvectors",io_err)
 ! 
 ! Exciton's spin
 !=====================
 if (l_spin)       call excitons_spin(Xk,Res,BS_E,BS_E_degs)
 !
 ! Exciton's Amplitude
 !=====================
 if (l_amplitude.or.l_dipoles)  call excitons_amplitudes(Xk,Xen,q,BS_E_degs,iq)
 !
 ! Exciton's WFs
 !=====================
 if (l_exc_wf)     call exciton_WFs(Xk,BS_E_degs,iq)
 !
 ! Excitonic Eliashberg function 
 !================================
#if defined _YPP_ELPH
 !
 if (l_eliashberg) call ELPH_general_gFsq(k,en,Xk,Xen,q,BS_E_degs)
 !
 if (l_gkkp)       call ELPH_excitonic_gkkp(Xk)
 !
#endif
 !
#if defined _YPP_RT
 !
 ! Transient absorption 
 !======================
 if (l_RT_abs)     call RT_transient_absorption(Xen,Xk,q)
 !
#endif
 !
 ! CLEAN
 !
 call excitons_read(k,Xk,en,Xen,BSiq,"clean",io_err)
 !
 YAMBO_FREE(EXCITONS_user_indexes)
 !
end subroutine

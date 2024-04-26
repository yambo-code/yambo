! 
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine electrons_driver(Xk,Xen,Xq)
 !
 Use stderr,         ONLY:intc,STRING_same
 use wave_func,      ONLY:WF,wf_ng
 use FFT_m,          ONLY:fft_size,fft_dim
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use QP_m,           ONLY:QP_table,QP_state
 use YPP_interfaces, ONLY:PROJECTION_setup
 use YPPm,           ONLY:l_density,l_mag,v2plot,&
&                         nr,l_sp_wf,DOS_bands,plot_title,l_dos,l_bands,l_current,&
&                         l_angular_momentum,l_position,l_mean_potential,INTERP_input_mode
 use xc_functionals, ONLY:magn
 use interfaces,     ONLY:WF_load,WF_free
 use parallel_int,   ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use interpolate,    ONLY:INTERP_mode
 !
#include<memory.h>
 !
 type(bz_samp) ::Xk,Xq
 type(levels)  ::Xen
 !
 ! Work Space
 !
 integer       ::NB(2),NK(2)
 !
 ! Doing something?
 !==================
 !
 if ( .not. any((/l_density,l_mag,l_sp_wf,l_dos,l_bands,l_current,&
&                 l_mean_potential,l_angular_momentum,l_position/)) ) return
 !
 INTERP_input_mode=INTERP_mode
 !
 ! Projection
 !============
 call PROJECTION_setup("setup")
 !
 ! Plotting setup
 !================
 call plot_check_and_launch(.true.)
 !
 ! States selector
 !=================
 !
 NB=(/1,maxval(Xen%nbm)/)
 NK=(/1,Xk%nibz/)
 !
 if ( any((/l_sp_wf,l_angular_momentum,l_position,l_mean_potential/)) ) then
   !
   call QP_state_table_setup(Xen)
   !
   NB =(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)
   NK =(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)
   !
 else if (l_dos) then
   !
   NB=DOS_bands
   !
 endif
 !
 ! WFs
 !=====
 if( any((/l_mag,l_density,l_sp_wf,l_current,l_mean_potential,l_angular_momentum,l_position/)) ) then
   !
   call PARALLEL_global_indexes(Xen,Xk,Xq," ",RESET=.TRUE.)
   call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
   call PARALLEL_WF_index( )
   !
   call WF_load(WF,wf_ng,1,NB,NK,space='R',title='-WF',impose_free_and_alloc=.TRUE.)
   nr=fft_dim
   YAMBO_ALLOC(v2plot,(fft_size))
   !
 endif
 !
 ! DOS 
 !=====
 if (l_dos)     call electrons_dos(Xk,Xen)
 !
 ! DENSITY
 !=========
 if (l_density) call electrons_density(Xk,Xen)
 !
 ! BANDS interpolation
 !======================
 if (l_bands) call electrons_bands(Xk,Xen)
 !
 ! WAVEFUNCTIONS 
 !===============
 if (l_sp_wf.or.l_mean_potential) call electrons_WFs(Xen)
 !
 ! MAGNETIZATION 
 !===============
 if (l_mag) call electrons_magnetization(Xk,Xen)
 !
#if defined _YPP_SC
 !
 ! Angular momentum 
 !==================
 if (l_angular_momentum) call electrons_angular_momentum(NK,NB)
 !
 ! <Mean> position
 !=================
 if (l_position)         call electrons_position(NK,NB,1.,'cylindrical','2d')
 !
#endif
 !
 ! CURRENT
 !=========
 if (l_current) call electrons_current(Xk,Xen)
 !
 ! Clean-up
 !==========
 call WF_free(WF)
 YAMBO_FREE(v2plot)
 YAMBO_FREE(QP_table)
 YAMBO_FREE(QP_state)
 plot_title=' '
 !
end subroutine

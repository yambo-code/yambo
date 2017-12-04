! 
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine electrons_driver(Xk,Xen,Xq)
 !
 Use stderr,         ONLY:intc
 use wave_func,      ONLY:WF,wf_ng
 use FFT_m,          ONLY:fft_size,fft_dim
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use QP_m,           ONLY:QP_table,QP_state
 use YPP,            ONLY:l_density,l_mag,v2plot,&
&                         nr,l_sp_wf,&
&                         plot_title,l_dos,l_bands,l_current,&
&                         l_angular_momentum,l_position,l_mean_potential
 use functions,      ONLY:Fermi_fnc_derivative
 use xc_functionals, ONLY:magn
 use interfaces,     ONLY:WF_load,WF_free,PARALLEL_WF_index,PARALLEL_global_indexes,PARALLEL_WF_distribute
 !
#include<memory.h>
 !
 type(bz_samp) ::Xk,Xq
 type(levels)  ::Xen
 !
 ! Work Space
 !
 integer               :: nb_to_load(2),nkpt_to_load(2)
 !
 ! Doing something?
 !==================
 !
 if ( .not. any((/l_density,l_mag,l_sp_wf,l_dos,l_bands,l_current,&
&                 l_mean_potential,l_angular_momentum,l_position/)) ) return
 !
 ! Plotting setup
 !================
 call plot_check_and_launch(.true.)
 !
 ! QP indexes loading
 !====================
 !
 nb_to_load=(/1,Xen%nbm/)
 nkpt_to_load=(/1,Xk%nibz/)
 !
 if ( any((/l_sp_wf,l_bands,l_angular_momentum,l_position,  l_mean_potential/)) ) then
   !
   call QP_state_table_setup(Xen)
   nb_to_load   =(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)
   nkpt_to_load =(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)
   !
 endif
 !
 ! WFs
 !=====
 if( .not.any((/l_dos,l_bands,l_angular_momentum,l_position,l_mean_potential/)) ) then
   !
   call PARALLEL_global_indexes(Xen,Xk,Xq," ",RESET=.TRUE.)
   call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
   call PARALLEL_WF_index( )
   !
   call WF_load(WF,wf_ng,1,nb_to_load,nkpt_to_load,space='R',title='-WF',impose_free_and_alloc=.TRUE.)
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
 !
 if (l_bands) call electrons_bands(Xk,Xen,nb_to_load)
 !
 !  !
 !  if ( i_time_rev==1 ) call error(' Bands interpolation does not work with T-rev symmetry')
 !  !
 !endif
 !
 ! WAVEFUNCTIONS 
 !===============
 !
 if (l_sp_wf.or.l_mean_potential) call electrons_WFs(Xen)
 !
 ! MAGNETIZATION 
 !===============
 !
 if (l_mag) call electrons_magnetization(nkpt_to_load,nb_to_load)
 !
 ! GPL_EXCLUDE_START
 !
#if defined _YPP_MAGNETIC
 !
 ! Angular momentum 
 !==================
 if (l_angular_momentum) call electrons_angular_momentum(nkpt_to_load,nb_to_load)
 !
 ! <Mean> position
 !=================
 if (l_position)         call electrons_position(nkpt_to_load,nb_to_load,1.,'cylindrical','2d')
 !
#endif
 !
 ! CURRENT
 !=========
 if (l_current) call electrons_current(Xk,Xen,Xq)
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

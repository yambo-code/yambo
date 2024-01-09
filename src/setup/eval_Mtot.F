!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine eval_Mtot(en,k,q)
 !
 use pars,           ONLY:SP
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:WF
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use com,            ONLY:msg
 use electrons,      ONLY:eval_magn
 use interfaces,     ONLY:WF_load,el_magnetization
 use parallel_int,   ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,&
&                         PARALLEL_WF_index
 !
#include <memory.h>
 !
 type(levels)    ::en
 type(bz_samp)   ::k,q
 !
 ! Work Space
 !
 real(SP), allocatable :: magn_tmp(:,:)
 !
 if (eval_magn) return
 !
 call msg('r','DL Magnetization [cc]')
 !
 call PARALLEL_global_indexes(en,k,q," ",RESET=.TRUE.)
 call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 call WF_load(WF,0,1,(/1,en%nbm/),(/1,k%nibz/),space='R',title='-WF')
 !
 YAMBO_ALLOC(magn_tmp,(fft_size,3))
 call el_magnetization(en,k,magn_tmp)
 YAMBO_FREE(magn_tmp)
 !
 call WF_free(WF)
 !
end subroutine eval_Mtot

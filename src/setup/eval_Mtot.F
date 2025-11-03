!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
! headers
!
#include <y_memory.h>
!
subroutine eval_Mtot(en,k,q)
 !
 use pars,           ONLY:SP
 use parser_m,       ONLY:parser
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:WF
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_spin,Spin_magn,eval_magn
 use com,            ONLY:msg
 use interfaces,     ONLY:WF_load,el_magnetization
 use parallel_int,   ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,&
&                         PARALLEL_WF_index
 use y_memory_alloc
 !
 implicit none
 !
 type(levels)    ::en
 type(bz_samp)   ::k,q
 !
 ! Work Space
 !
 real(SP), allocatable :: magn_tmp(:,:)
 !
 if(n_spin==1) return
 !
 call parser("EvalMagn",eval_magn)
 !
 if(.not.eval_magn) return
 !
 call msg('r','DL Magnetization [cc]')
 !
 call PARALLEL_global_indexes(en,k,q," ",RESET=.TRUE.)
 call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 call fft_setup(0,1,.true.)
 YAMBO_ALLOC(magn_tmp,(fft_size,3))
 call el_magnetization(en,k,magn_tmp)
 YAMBO_FREE(magn_tmp)
 !
 call msg('s','Total magnetization (x,y,z)',real(Spin_magn(:),SP),"[Bohr_magneton]")
 !
end subroutine eval_Mtot

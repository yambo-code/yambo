!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine eval_Mtot(en,k,q)
 !
 use pars,           ONLY:SP,pi,schlen
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:WF
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use com,            ONLY:msg
 use electrons,      ONLY:eval_magn
 use interfaces,     ONLY:WF_load,el_density_and_current,&
&                         PARALLEL_global_indexes,PARALLEL_WF_distribute,&
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

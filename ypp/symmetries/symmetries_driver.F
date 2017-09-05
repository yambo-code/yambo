!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine symmetries_driver(E,Xk)
 !
 use pars,                ONLY:SP
 use xc_functionals,      ONLY:magn
 use electrons,           ONLY:levels,Total_magn,n_spinor
 use FFT_m,               ONLY:fft_size
 use IO_m,                ONLY:mk_dir,rm_file
 use com,                 ONLY:msg,core_io_path,more_io_path
 use R_lattice,           ONLY:bz_samp,ng_closed,ng_vec,bz_samp_reset,bz_samp_duplicate
 use D_lattice,           ONLY:nsym
 use wave_func,           ONLY:wf_ng,WF
 use stderr,              ONLY:intc
 use interfaces,          ONLY:WF_load
 !
#include<memory.h>
 !
 type(levels),intent(in)     :: E
 type(bz_samp),intent(inout) :: Xk
 !
 ! Work Space
 !
 type(bz_samp)         :: Xk_save
 integer,allocatable   :: kpoints_map(:,:)
 ! 
 integer               :: old_nsym
 real(SP)              :: old_dl_sop(3,3,nsym)
 integer               :: old_sop_inv(nsym)
 logical               :: S_contains_TR(nsym)
 !
 integer               :: action_kind,old_nkpt
 !
 if (core_io_path==more_io_path) more_io_path=trim(core_io_path)//"/FixSymm"
 !
 call msg('s','Symmetries-respecting SAVE written to'//trim(more_io_path))
 !
 call mk_dir(more_io_path)
 call mk_dir(trim(more_io_path)//"/SAVE")
 call rm_file((trim(more_io_path))//"/SAVE/ndb.gops")
 call rm_file((trim(more_io_path))//"/SAVE/ndb.kindx")
 !
 ! This is needed to construct grot for all wf_ngs.
 if (wf_ng>ng_closed) then
   ng_closed=ng_vec
   call G_shells_finder()
 endif
 !
 ! Check if the system is magnetic
 ! Magnetization must be considered to derive syms if n_spinor==2
 if(n_spinor==2) then
   call WF_load(WF,0,1,(/1,E%nbm/),(/1,Xk%nibz/),title='-Magn')
   YAMBO_ALLOC(magn,(fft_size,3))
   call el_magnetization(E,Xk,magn)
   call msg('s','Total magnetization (x,y,z) [Bohr_magneton] :',real(Total_magn(:),SP))
   YAMBO_FREE(magn)
   call WF_free(WF)
 endif
 !
 ! Expansion of default k-points
 call k_ibz2bz(Xk,'i',.false.)
 YAMBO_FREE(Xk%pt)
 call k_reduce(Xk,.false.)
 !
 call fix_symmetries(old_nsym,old_dl_sop,old_sop_inv,S_contains_TR,action_kind)
 !
 if(old_nsym==nsym) then
   call msg('s','Symmetries did not change. Program terminaned.')
   return
 endif
 !
 if(old_nsym<nsym) ng_closed=ng_vec
 !
 call section('=',"K-points")
 !===========================
 !
 call bz_samp_reset(Xk_save)
 call bz_samp_duplicate(Xk,Xk_save)
 !
 old_nkpt=Xk%nibz
 YAMBO_FREE(Xk%pt)
 call k_reduce(Xk,.false.)
 call msg('s',trim(intc(old_nkpt))//' k-points have been expanded/reduced to ...'//trim(intc(Xk%nibz)))
 !
 YAMBO_FREE(Xk%sstar)
 YAMBO_FREE(Xk%star)
 YAMBO_FREE(Xk%nstar)
 YAMBO_FREE(Xk%weights)
 call k_expand(Xk)
 !
 YAMBO_ALLOC(kpoints_map,(2,Xk%nibz))
 call k_build_map(Xk,Xk_save,kpoints_map,action_kind)
 !
 ! Energies and Wave-functions
 call fix_WFs_and_E(E,Xk,Xk_save,kpoints_map,old_nsym,S_contains_TR,action_kind)
 !
 ! Rotate KB or Vnl
 call fix_PPs(E,Xk,Xk_save,kpoints_map,old_nsym,old_dl_sop,old_sop_inv,S_contains_TR,action_kind)
 !
 ! CLEAN
 !=======
 call bz_samp_reset(Xk_save)
 YAMBO_FREE(kpoints_map)
 !
end subroutine

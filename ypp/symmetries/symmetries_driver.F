!
!        Copyright (C) 2000-2022 the YAMBO team
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
 use pars,                ONLY:SP,lchlen
 use xc_functionals,      ONLY:magn
 use electrons,           ONLY:levels,Spin_magn,n_spinor
 use FFT_m,               ONLY:fft_size
 use IO_m,                ONLY:rm_file,cp_file,OP_RD_CL,NONE,DUMP
 use IO_int,              ONLY:ver_is_gt_or_eq,io_control
 use com,                 ONLY:msg,core_io_path,more_io_path,jobstr
 use R_lattice,           ONLY:bz_samp,ng_closed,ng_vec,bz_samp_reset,bz_samp_duplicate,nqbz
 use D_lattice,           ONLY:nsym,atom_mass,atoms_map,n_atoms_species_max,n_atomic_species
 use wave_func,           ONLY:wf_ng,WF
 use stderr,              ONLY:intc
 use LIVE_t,              ONLY:live_timing
 use interfaces,          ONLY:WF_load,WF_free,el_magnetization
 use parser_m,            ONLY:parser
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
 logical               :: S_contains_TR(nsym),l_keep_k_grid
 !
 integer               :: action_kind,old_nkpt
 !
#if defined _YPP_ELPH
 character(lchlen)  :: filename,infile,outfile,in_path
 integer, external  :: io_ELPH
 integer            :: iq,ID_gkkp,io_err
#endif
 !
 if (core_io_path==more_io_path) more_io_path=trim(core_io_path)//"/FixSymm"
 !
 call parser('KeepKGrid',l_keep_k_grid)
 !
 call msg('s','Symmetries-respecting SAVE written to'//trim(more_io_path))
 !
 call IO_make_directories("SAVE")
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
   call msg('s','Total magnetization (x,y,z)',real(Spin_magn(:),SP),"[Bohr_magneton]")
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
 !
 call section('=',"K-points")
 !===========================
 !
 call bz_samp_reset(Xk_save)
 call bz_samp_duplicate(Xk,Xk_save)
 !
 if(.not.l_keep_k_grid) then
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
 endif
 !
 YAMBO_ALLOC(kpoints_map,(2,Xk%nibz))
 call k_build_map(Xk,Xk_save,kpoints_map,action_kind)
 !
 ! If I remove symmetries from old DB I need to add the new veriables to the DB1
 !
 if(.not.allocated(atom_mass)) then
   YAMBO_ALLOC(atom_mass,(n_atomic_species))
   YAMBO_ALLOC(atoms_map,(n_atoms_species_max,n_atomic_species))
   atom_mass=0._SP
   atoms_map=1  ! set to 1 to avoid segmentation faults
 endif
 !
 ! Energies and Wave-functions
 call fix_WFs_and_E(E,Xk,Xk_save,kpoints_map,old_nsym,S_contains_TR,action_kind)
 !
 ! Rotate KB or Vnl
 call fix_PPs(E,Xk,Xk_save,kpoints_map,old_nsym,old_dl_sop,old_sop_inv,S_contains_TR,action_kind)
 !
 ! Rotate atomic projections
 call fix_ATOMPROJs(E,Xk,Xk_save,kpoints_map,old_nsym,old_dl_sop,old_sop_inv,S_contains_TR,action_kind)
 !
#if defined _YPP_ELPH
 !
 ! Copy gkkp_expanded if presents
 !
 call section('=',"GKKP-databases")
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded') 
 !
 if(io_err==0) then
   !      
    if (trim(jobstr)/='') then
      in_path =trim(jobstr)//"/"
   else
      in_path='SAVE/'
   endif
   !
   call live_timing('Copying gkkp files',nqbz+1)
   !
   do iq=0,nqbz
     !
     if(iq==0) then
       filename ='ndb.elph_gkkp_expanded'
     else
       filename ='ndb.elph_gkkp_expanded_fragment_'//trim(intc(iq))
     endif
     infile=trim(in_path)//trim(filename)
     !
     outfile="./"//trim(more_io_path)//"/SAVE/"//trim(filename)
     call cp_file(trim(infile),trim(outfile),io_err)
     if(io_err/=0) call error(' Error copying the gkkp_expanded files')
     !
     call live_timing(steps=1)
     !
   enddo
   !
   call live_timing()
   !
 else
   call msg('s','Electron-phonon DBs not present')
 endif
 !
#endif
 !
 ! CLEAN
 !=======
 call bz_samp_reset(Xk_save)
 YAMBO_FREE(kpoints_map)
 !
end subroutine

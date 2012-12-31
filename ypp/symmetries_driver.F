!
! Copyright (C) 2000-2012 C. Attaccalite, A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine symmetries_driver(E,k)
 !
 use pars,                ONLY:SP
 use electrons,           ONLY:levels
 use com,                 ONLY:msg,warning
 use R_lattice,           ONLY:bz_samp,ng_closed,ng_vec,bz_samp_reset,bz_samp_duplicate
 use D_lattice,           ONLY:nsym
 use wave_func,           ONLY:wf_nb_io,wf_ng,WF_load,WF_free
 use stderr,              ONLY:intc
 !
 implicit none
 !
 type(levels),intent(in)     :: E
 type(bz_samp),intent(inout) :: k
 !
 ! Work Space
 !
 type(bz_samp)         :: k_save
 integer               :: old_nsym
 real(SP)              :: old_dl_sop(3,3,nsym)
 integer               :: old_sop_inv(nsym)
 logical               :: S_contains_TR(nsym)
 integer               :: wf_ng_save
 integer               :: wf_ncx_save
 integer               :: action_kind
 integer,allocatable   :: kpoints_map(:,:)
 !
 ! This is needed to construct grot for all wf_ngs.
 if (ng_vec>ng_closed) then
   ng_closed=ng_vec
   call G_shells_finder()
 endif
 !
 ! Expansion of default k-points
 call k_ibz2bz(k,'i',.true.)
 call k_reduce(k)
 !
 call bz_samp_reset(k_save)
 call bz_samp_duplicate(k,k_save)
 !
 call fix_symmetries(old_nsym,old_dl_sop,old_sop_inv,S_contains_TR,action_kind)
 !
 if(old_nsym<nsym) call G_shells_finder()
 !
 call section('=',"K-points and G-shells")
 !===========================
 !
 call msg('s',trim(intc(k%nibz))//' k-points are now expanded/reduced to ...')
 deallocate(k%pt)
 call k_reduce(k)
 call msg('l'," "//trim(intc(k%nibz)))
 !
 allocate(kpoints_map(2,k%nibz))
 !
 ! In case sysm are expanded I have to load WFs here because after wf_nc_k
 ! will be ill defined on the smaller set of kpts. Instead if syms are reduced
 ! wf_nc_k_new = (wf_nc_k_old, something_more) and WF_load_works fine
 if(action_kind==2) call WF_load(wf_ng,1,(/1,E%nb/),(/1,k_save%nibz/),space='C',title='')
 !
 ! Fill kpoints_map and rotate spheres of G-vectors centered on kpts
 call fix_kpts_and_gvec(k,k_save,kpoints_map,wf_ncx_save,wf_ng_save,action_kind)
 !
 ! Energies and Wave-functions
 call fix_WFs_and_E(E,k,k_save,kpoints_map,wf_ncx_save,old_nsym,S_contains_TR,action_kind)
 !
 ! Rotate KB and Vnl
 call fix_PPs(E,k,k_save,kpoints_map,wf_ng_save,old_nsym,old_dl_sop,old_sop_inv,S_contains_TR,action_kind)
 !
 ! CLEAN
 !=======
 if(action_kind==2) call WF_free()
 call bz_samp_reset(k_save)
 deallocate(kpoints_map)
 !
end subroutine

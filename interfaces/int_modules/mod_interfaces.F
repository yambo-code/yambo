!
! Copyright (C) 2000-2005 A. Marini and the SELF team
!         http://www.fisica.uniroma2.it/~self
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
 module interfaces
   !
   use pars
   !
 contains

   subroutine print_interface_dimensions(en,k)
     use pars,                  ONLY : SP, PI, lchlen
     use com,                   ONLY : msg
     use electrons,             ONLY : levels, default_nel, n_spin, n_sp_pol, n_spinor, spin_orbit
     use R_lattice,             ONLY : bz_samp, ng_vec, g_vec
     use D_lattice,             ONLY : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                  DL_vol, a, alat, default_Tel, &
&                                  n_atoms_max,n_atom_species,n_atoms, atom_pos, &
&                                  znucl
     use wave_func,             ONLY : wf_nc_k, wf_ncx, wf_igk, wf_ng
     use xc_functionals,        ONLY : GS_xc_KIND,GS_xc_FUNCTIONAL, xc_string
     implicit none
     type(levels),     intent(in)   :: en     ! Energies
     type(bz_samp),    intent(in)   :: k      ! K/Q points

     call msg('s',' :: Electrons             :',default_nel)
     call msg('s',' :: Temperature       [ev]:',default_Tel)
     call msg('s',' :: Lattice factors [a.u.]:',alat)
     call msg('s',' :: K-points              :',k%nibz)
     call msg('s',' :: Bands                 :',en%nb)
     call msg('s',' :: Spinor components     :',n_spinor)
     call msg('s',' :: Spin polarizations    :',n_sp_pol)
     call msg('s',' :: Spin orbit coupling   : ',spin_orbit)
     call msg('s',' :: Symmetries   [spatial]:',nsym)
     call msg('s',' ::                [T-rev]: ',i_time_rev==1)
     call msg('s',' :: Max WF components     :',wf_ncx)
     call msg('s',' :: RL vectors        (WF):',wf_ng)
     call msg('s',' :: RL vectors    (CHARGE):',ng_vec)
     call msg('s',' :: XC potential          : ',xc_string(GS_xc_KIND,GS_xc_FUNCTIONAL))
     call msg('s',' :: Atomic species        :',n_atom_species)
     call msg('s',' :: Max atoms/species     :',n_atoms_max)
     return
   end subroutine print_interface_dimensions

 end module

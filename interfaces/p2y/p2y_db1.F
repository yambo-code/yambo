!
! Copyright (C) 2000-2013 C. Hogan and the YAMBO team 
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
subroutine p2y_db1(en,k)
 !
 use P2Y
 use com,                   ONLY : msg,warning
 use mod_com2y,             ONLY : print_interface_dimensions,N_G_vectors
 use xc_functionals,        ONLY : xc_string,GS_xc_KIND,GS_xc_FUNCTIONAL
 use R_lattice,             ONLY : ng_vec,g_vec
 use wave_func,             ONLY : wf_ng, wf_nc_k, wf_ncx, wf_igk
 use mod_com2y,             ONLY : force_noWFs
 implicit none
 type(levels),     intent(out)  :: en     ! Energies
 type(bz_samp),    intent(out)  :: k      ! K/Q points
 !---------------------------------------------------------------------*
 !    Read dimensions                                                  *
 !---------------------------------------------------------------------*
 call msg('s','Header/K-points/Energies...')
 !
 call get_dimensions(en,k)
 call msg("l","done")
 !
 !---------------------------------------------------------------------*
 !    Read cell data                                                   *
 !---------------------------------------------------------------------*
 !
 call msg('s','Cell data...')
 !
 call get_cell
 call msg("l","done")
 !
 !---------------------------------------------------------------------*
 !    Read atomic data                                                 *
 !---------------------------------------------------------------------*
 !
 call msg('s','Atomic data...')
 !
 call get_atoms
 call msg("l","done")
 !
 !---------------------------------------------------------------------*
 !    Read symmetries                                                  *
 !---------------------------------------------------------------------*
 !
 call msg('s','Symmetries...')
 !
 call get_symmetries
 !
 !---------------------------------------------------------------------*
 !    Read XC functional                                               *
 !---------------------------------------------------------------------*
 !
 call get_xc
 call msg('s','XC functional...'//xc_string(GS_xc_FUNCTIONAL))
 !
 !---------------------------------------------------------------------*
 !    Read k-point mesh                                                *
 !---------------------------------------------------------------------*
 !
 call msg('s','K-points mesh...')
 !
 call get_k_points(k)
 !
 call msg('l','done')
 !
 !---------------------------------------------------------------------*
 !    Read miscellaneous data                                          *
 !---------------------------------------------------------------------*
 !
 call get_more
 !
 !
 if(.not.force_noWFs) then
   !---------------------------------------------------------------------*
   !    Read G-vectors                                                   *
   !---------------------------------------------------------------------*
   !
   call msg('s','RL vectors...')
   !
   call get_R_vectors
   !
   call msg('l','done')
   !
   !---------------------------------------------------------------------*
   !    Read igk arrays                                                  *
   !---------------------------------------------------------------------*
   call msg('s','IGK arrays...')
   !
   call get_IGK(k)
   !
   if (N_G_vectors>0) ng_vec=max(N_G_vectors,wf_ng)
   !
   call msg('l','done')
   !
 else
   !
   N_G_vectors=1
   wf_ng      =1
   ng_vec     =1
   allocate(g_vec(1,3))
   g_vec(1,:)=(/0,0,0/)
   allocate(wf_nc_k(k%nibz))
   wf_nc_k(:)=1
   wf_ncx    =1
   allocate(wf_igk(1,k%nibz))
   wf_igk(1,:)=1
   !
   call warning('IGK array and RL vectors skipped! ')
   !
 endif
 !---------------------------------------------------------------------*
 !    Read eigenvalues                                                 *
 !---------------------------------------------------------------------*
 !
 call msg('s','Energies...')
 !
 call get_energies(en,k)
 !
 call msg('l','done')
 !
 !---------------------------------------------------------------------*
 !    Report                                                           *
 !---------------------------------------------------------------------*
 call print_interface_dimensions(en,k)
 !
end subroutine p2y_db1

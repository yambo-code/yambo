!
! Copyright (C) 2000-2010 C. Hogan and the YAMBO team 
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
 use com,                   ONLY : msg
 use mod_com2y,             ONLY : print_interface_dimensions
 use xc_functionals,        ONLY : xc_string,GS_xc_KIND,GS_xc_FUNCTIONAL
#if defined _LIBXC
     use xc_functionals,        ONLY : xc_setup_fnctl,xc_fnctl_t
#endif
 implicit none
 type(levels),     intent(out)  :: en     ! Energies
 type(bz_samp),    intent(out)  :: k      ! K/Q points
#if defined _LIBXC
 type(xc_fnctl_t)               :: f(2)   ! xc functional info's (probably better as input)
#endif

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
#if defined _LIBXC
 call xc_setup_fnctl(f,GS_xc_FUNCTIONAL,1)
 if (f(1)%id >0) call msg('s','XC functional...',xc_string(f(1)))
 if (f(2)%id >0) call msg('s','XC functional...',xc_string(f(2)))
 if (all(f%id==0)) call msg('s','XC functional...: None (only Hartree)')
#else
 call msg('s','XC functional...'//xc_string(GS_xc_KIND,GS_xc_FUNCTIONAL))
#endif
 !
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
 !---------------------------------------------------------------------*
 !    Read G-vectors                                                   *
 !---------------------------------------------------------------------*
 !
 call msg('s','RL vectors...')
 !
 call get_R_vectors
 !
 call msg('l','done')
 !---------------------------------------------------------------------*
 !    Read igk arrays                                                  *
 !---------------------------------------------------------------------*
 call msg('s','IGK arrays...')
 !
 call get_IGK(k)
 !
 call msg('l','done')
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

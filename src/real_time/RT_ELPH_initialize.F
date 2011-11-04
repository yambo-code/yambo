!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team 
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
subroutine RT_ELPH_initialize()
 ! 
 use drivers,        ONLY:l_ph_corr 
 use units,          ONLY:HA2EV
 use ELPH,           ONLY:ph_modes,elph_nb,elph_nDBs,elph_nDBs_used,nq_memory,elph_branches,&
&                         elph_global_alloc,ph_freqs_sq
 use SC,             ONLY:SC_bands
 use R_lattice,      ONLY:nqbz
 !
 implicit none
 !
 if (.not.l_ph_corr) return
 !
 ph_modes=1
 elph_nb=SC_bands(2)
 elph_nDBs=nqbz
 elph_nDBs_used=nqbz
 nq_memory=nqbz
 elph_branches=(/1,1/)
 !
 call elph_global_alloc(' ')
 !
 ph_freqs_sq=(5./HA2EV)**2.
 !
end subroutine RT_ELPH_initialize

!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
subroutine ELPH_double_grid(k,E,q)
 !
 use pars,                ONLY:SP
 use electrons,           ONLY:levels,E_reset
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,BZ_FineGd_mode
 use D_lattice,           ONLY:n_atoms
 use YPP_ELPH,            ONLY:ph_modes_file,ph_freqs_file,EkplusQ_mode,ELPH_databases_IO_freqs
 use stderr,              ONLY:STRING_same
 use com,                 ONLY:msg
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 type(levels)  ::E
 !
 ! Work space
 !
 type(bz_samp)         :: q_fine
 type(levels)          :: Ph_E
 integer               :: ph_modes
 real(SP), allocatable :: ph_freqs(:,:)
 logical, external     :: file_exists
 logical               :: l_mix,l_expand,l_unexpand,FineGd_is_expanded
 !
 ph_modes=3*n_atoms
 !
 if(.not.STRING_same(ph_modes_file,'none')) call warning("Double-grid for polarization vectors not implemented yet!")
 !
 if(.not.STRING_same(EkplusQ_mode,'interp')) then
    call warning("Only Fourier interpolation implemented for the E(k+q) energies, EkplusQmode switched to 'interp'")
    EkplusQ_mode='interp'
 endif
 !
 if(STRING_same(ph_freqs_file,'none'))            call error("Phonon frequencies file not specified!")
 !
 call bz_samp_reset(q_fine)
 ! 
 if(.not.file_exists(trim(ph_freqs_file))) call error("Phonon frequencies file not found!")
 !
 call msg('s',"Phonon frequencies read from: "//trim(ph_freqs_file))
 call msg('s',"Number of phonon modes: ",ph_modes)
 !
 call ELPH_databases_IO_freqs(ph_freqs_file=ph_freqs_file,n_q=q_fine%nibz)
 !
 call msg('s',"Number of phonon q-points in the matdyn file: ",q_fine%nibz)
 !
 YAMBO_ALLOC(q_fine%pt,(q_fine%nibz,3))
 !
 call E_reset(Ph_E)
 !
 YAMBO_ALLOC(ph_freqs,(q_fine%nibz,ph_modes))
 !
 call ELPH_databases_IO_freqs(ph_freqs_file=ph_freqs_file,n_q=q_fine%nibz,q_pt=q_fine%pt,ph_freqs=ph_freqs)
 !
 ! I assume q-points in alat units, convert to internal units
 ! ==================================
 !
 ! Expand the Coarse Grid 
 !========================
 call k_build_up_BZ_tables(q)
 call k_ibz2bz(q,'i',.true.)
 !
 l_mix     =STRING_same(BZ_FineGd_mode,"mixed")
 l_expand  =STRING_same(BZ_FineGd_mode,"expanded")
 l_unexpand=STRING_same(BZ_FineGd_mode,"unexpanded")
 !
end subroutine

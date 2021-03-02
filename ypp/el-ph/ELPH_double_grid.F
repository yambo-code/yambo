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
 use pars,                ONLY:SP,pi
 use electrons,           ONLY:levels,E_reset
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,BZ_FineGd_mode
 use D_lattice,           ONLY:n_atoms,alat
 use YPP_ELPH,            ONLY:ph_modes_file,ph_freqs_file,EkplusQ_mode,ELPH_databases_IO_freqs
 use stderr,              ONLY:STRING_same
 use com,                 ONLY:msg
 use vec_operate,         ONLY:c2a
 use parser_m,            ONLY:parser
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 type(levels)  ::E
 !
 ! Work space
 !
 type(bz_samp)         :: q_io,q_fine
 type(levels)          :: Ph_E_io,Ph_E_fine
 integer               :: ph_modes,i_q
 logical, external     :: file_exists
 logical               :: l_mix,l_expand,l_unexpand,FineGd_is_expanded
 logical               :: l_skip_DbGd_border_points
 !
 ph_modes=3*n_atoms
 !
 call parser('SkipBorderPts',l_skip_DbGd_border_points)
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
 call bz_samp_reset(q_io)
 ! 
 if(.not.file_exists(trim(ph_freqs_file))) call error("Phonon frequencies file not found!")
 !
 call msg('s',"Phonon frequencies read from: "//trim(ph_freqs_file))
 call msg('s',"Number of phonon modes: ",ph_modes)
 !
 call ELPH_databases_IO_freqs(ph_freqs_file=ph_freqs_file,n_q=q_io%nibz)
 !
 call msg('s',"Number of phonon q-points in the matdyn file: ",q_io%nibz)
 !
 YAMBO_ALLOC(q_io%pt,(q_io%nibz,3))
 !
 call E_reset(Ph_E_fine)
 call E_reset(Ph_E_io)
 !
 Ph_E_io%nb=ph_modes
 Ph_E_io%nk=q_io%nibz
 YAMBO_ALLOC(Ph_E_io%E,(Ph_E_io%nb,Ph_E_io%nk,1))
 !
 call ELPH_databases_IO_freqs(ph_freqs_file=ph_freqs_file,n_q=q_io%nibz,q_pt=q_io%pt,ph_freqs=Ph_E_io%E(:,:,1))
 !
 ! q-points in matdyn output are alat units, convert to internal units
 ! ==================================
 q_io%pt=q_io%pt*(2.*pi/alat(1))    ! From QuantumEspresso Alat to CC
 !
 ! Converto to iku units
 !
 do i_q=1,q_io%nibz
   call c2a(v_in=q_io%pt(i_q,:),mode="kc2i")
 enddo
 !
 ! Expand the Coarse Grid 
 !========================
 call k_build_up_BZ_tables(q)
 call k_ibz2bz(q,'i',.true.)
 !
 ! Find border points and remove them if required
 ! ===============================================
 call k_find_border_and_merge(Ph_E_fine,q_fine,1,Ph_E_io,q_io,q,l_skip_DbGd_border_points)
 stop
 !
 !
 l_mix     =STRING_same(BZ_FineGd_mode,"mixed")
 l_expand  =STRING_same(BZ_FineGd_mode,"expanded")
 l_unexpand=STRING_same(BZ_FineGd_mode,"unexpanded")
 !
end subroutine

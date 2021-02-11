!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine excitons_ph_ass_dos(k,Xk,en,Xen,q_exc)
 !
 use pars,          ONLY:SP,pi
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use electrons,     ONLY:levels
 use com,           ONLY:msg
 use D_lattice,     ONLY:n_atoms,alat
 use YPP_ELPH,      ONLY:ph_freqs_file,ELPH_databases_IO_freqs
 use vec_operate,   ONLY:c2a
 !
#include<memory.h> 
 !
 type(bz_samp) ::Xk,k,q_exc
 type(levels)  ::Xen,en
 !
 logical, external  :: file_exists
 integer            :: i_q,ph_modes
 type(bz_samp)      :: q_matdyn
 real(SP), allocatable :: ph_freqs(:,:)
 !
 ph_modes=3*n_atoms
 !
 if(trim(ph_freqs_file)=='none')            call error("Phonon frequencies file not specified!")      
 !
 call msg('s',"Alat in yambo                 : ",alat(1))
 !
 call bz_samp_reset(q_matdyn)
 !
 if(.not.file_exists(trim(ph_freqs_file))) call error("Phonon frequencies file not found!")      
 !
 call msg('s',"Phonon frequencies read from: "//trim(ph_freqs_file))
 call ELPH_databases_IO_freqs(ph_freqs_file,q_matdyn%nibz)
 call msg('s',"Number of phonon modes found : ",q_matdyn%nibz)
 !
 YAMBO_ALLOC(q_matdyn%pt,(q_matdyn%nibz,3))
 YAMBO_ALLOC(ph_freqs,(q_matdyn%nibz,ph_modes))
 !
 call ELPH_databases_IO_freqs(ph_freqs_file,q_matdyn%nibz,q_matdyn%pt,ph_freqs)
 !
 q_matdyn%pt=q_matdyn%pt*(2.*pi/alat(1))    ! From QuantumEspresso Alat to CC
 !
 ! Converto to iku units
 !
 do i_q=1,q_matdyn%nibz
   call c2a(v_in=q_matdyn%pt(i_q,:),mode="kc2i")
 enddo
 !
 call k_expand(q_matdyn)
 call msg('s',"Number of q-points in the BZ : ",q_matdyn%nbz)
 !
 ! Interpolate Excitons on the same grid
 ! 
 ! 
 ! Calculate the phonon assisted density of states
 !
 !  ph_ass_dos(w) = \sum_{Q,l,n} Bose(E^exc_{l,Q}, T) * Bose(w_{n,q}, T) \delta { E^exc_{l,Q} - w_{n,q} - w } \delta {Q,q}
 ! 
end subroutine

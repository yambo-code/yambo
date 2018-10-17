! 
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine electrons_dos_project( E , dos_E , el_dos )
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use LIVE_t,         ONLY:live_timing
 use YPP,            ONLY:INTERP_grid,DOS_bands,DOS_E_steps,electrons_dos_elemental
 use electrons,      ONLY:levels,E_reset,n_sp_pol,n_spin
 !
 implicit none
 !
 type(levels)  :: E
 real(SP)      :: el_dos(DOS_E_steps,n_spin,2),dos_E(DOS_E_steps)
 !
 ! Work Space
 !
 integer       :: ik,i_E
 type(bz_samp) :: SLICE_k
 type(levels)  :: SLICE_E
 !
 call section('=','DOS projection')    
 !
 call bz_samp_reset(SLICE_k)
 call E_reset(SLICE_E)
 !
 allocate(SLICE_k%weights(INTERP_grid(1)))
 allocate(SLICE_k%sstar(INTERP_grid(1),2))
 allocate(SLICE_E%E(DOS_bands(1):DOS_bands(2),INTERP_grid(1),n_sp_pol))
 allocate(SLICE_E%f(DOS_bands(1):DOS_bands(2),INTERP_grid(1),n_sp_pol))
 SLICE_k%nibz=INTERP_grid(1)
 SLICE_k%nbz =INTERP_grid(1)
 SLICE_k%weights=1./float(SLICE_k%nbz)
 !
 do ik=1,INTERP_grid(1)
   SLICE_k%sstar(ik,1)=ik
 enddo
 !
 call live_timing('Projected DOS',INTERP_grid(1)+1)
 !
 do ik=1,INTERP_grid(1)+1
   !
   SLICE_E%E=E%E(DOS_bands(1):DOS_bands(2),SLICE_k%nibz*(ik-1)+1:SLICE_k%nibz*ik,:)
   SLICE_E%f=E%f(DOS_bands(1):DOS_bands(2),SLICE_k%nibz*(ik-1)+1:SLICE_k%nibz*ik,:)
   !
   call electrons_dos_elemental(SLICE_k,SLICE_E,DOS_bands,el_dos(:,:,1),dos_E,USE_the_DbGd=.FALSE.,QUIET=.TRUE.)
   !
   do i_E=1,DOS_E_steps
     !
     !write (100+ik,*) dos_E(i_E)*HA2EV,el_dos(i_E,1,1)
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 call bz_samp_reset(SLICE_k)
 call E_reset(SLICE_E)
 stop
 !
end subroutine

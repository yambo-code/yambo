!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine MEM_treshold_estimate( )
 !
 use pars,         ONLY:SP
 use units,        ONLY:Kilobyte
 use electrons,    ONLY:n_spin,n_bands
 use R_lattice,    ONLY:ng_vec,nkibz
 use wave_func,    ONLY:wf_ng
 use memory,       ONLY:MEM_treshold,MEM_SAVE_treshold,MEM_jump_treshold
 !
 implicit none
 !
 ! In Kb!
 !
 MEM_treshold=max(int(n_spin*nkibz*n_bands*wf_ng*2.*SP/Kilobyte/100.),1)
 MEM_SAVE_treshold=10 *MEM_treshold
 MEM_jump_treshold=100*MEM_treshold
 !
end subroutine

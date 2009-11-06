!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team 
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
integer function ioDB1_reload(E,k,DB1_path)
 !
 use pars,             ONLY:lchlen
 use memory_m,         ONLY:mem_est
 use electrons,        ONLY:levels,E_reset
 use D_lattice,        ONLY:dl_sop,n_atoms_species,Z_species,atom_pos
 use R_lattice,        ONLY:g_vec,bz_samp,bz_samp_reset
 use wave_func,        ONLY:wf_nc_k,wf_igk
 use com,              ONLY:core_io_path
 use IO_m,             ONLY:io_control,OP_RD_CL,NONE,DUMP
 implicit none
 type(levels)  :: E
 type(bz_samp) :: k
 character(*)  :: DB1_path
 !
 ! WorkSpace
 !
 integer           :: ID
 integer, external :: ioDB1
 character(lchlen) :: core_io_path_save
 !
 ! Saves
 !
 core_io_path_save=core_io_path
 !
 ! Reset
 !
 call bz_samp_reset(k)
 call E_reset(E)
 deallocate(g_vec,dl_sop,wf_nc_k,wf_igk)
 call mem_est("g_vec dl_sop wf_nc_k wf_igk")
 if (allocated(n_atoms_species)) then
   deallocate(n_atoms_species,Z_species,atom_pos)
   call mem_est("n_atoms_species atom_pos Z_species")
 endif
 !
 ! Path redirection
 !
 core_io_path=DB1_path
 !
 ! DB1
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=ID)
 ioDB1_reload=ioDB1(E,k,ID)
 !
 core_io_path=core_io_path_save
 !
end function ioDB1_reload

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
 integer function ioDB1_reload(E,k,DB1_path)
 !
 use pars,             ONLY:lchlen,SP
 use memory_m,         ONLY:mem_est
 use electrons,        ONLY:levels,E_reset
 use D_lattice,        ONLY:dl_sop,n_atoms,znucl,atom_pos
 use R_lattice,        ONLY:g_vec,bz_samp,bz_samp_reset
 use wave_func,        ONLY:wf_nc_k,wf_igk,wf_ng
 use com,              ONLY:inpath
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
 character(lchlen) :: inpath_save
 !
 ! Saves
 !
 inpath_save=inpath
 !
 ! Reset
 !
 call bz_samp_reset(k)
 call E_reset(E)
 deallocate(g_vec,dl_sop,wf_nc_k,wf_igk)
 call mem_est("g_vec dl_sop wf_nc_k wf_igk")
 if (allocated(n_atoms)) then
   deallocate(n_atoms,znucl,atom_pos)
   call mem_est("n_atoms atom_pos znucl")
 endif
 !
 ! Path redirection
 !
 inpath=DB1_path
 !
 ! DB1
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=ID)
 ioDB1_reload=ioDB1(E,k,ID)
 !
 inpath=inpath_save
 !
 end function ioDB1_reload

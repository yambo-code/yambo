!
!        Copyright (C) 2000-2017 the YAMBO team
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
integer function io_DB1_selective_scan(what,DB1_path,E,k,db1_alat,db1_a)
 !
 ! Do a selective (only variables specified in what) or a 
 ! global I/O of the DB1 database located in the path
 ! provided with DB1_path
 !
 use pars,             ONLY:lchlen,SP
 use electrons,        ONLY:levels,E_reset
 use D_lattice,        ONLY:dl_sop,n_atoms_species,Z_species,atom_pos,nsym,alat
 use R_lattice,        ONLY:g_vec,rl_sop,bz_samp,bz_samp_reset
 use wave_func,        ONLY:wf_nc_k,wf_igk
 use com,              ONLY:core_io_path
 use IO_m,             ONLY:io_control,OP_RD_CL,NONE,DUMP,io_connect,io_disconnect,&
&                           io_elemental,io_bulk
#include<memory.h>
 type(levels)      :: E
 type(bz_samp)     :: k
 character(*)      :: DB1_path,what
 real(SP),optional :: db1_a(3,3)
 real(SP),optional :: db1_alat(3)
 !
 ! WorkSpace
 !
 integer           :: ID,i_io,i1,i2,is
 integer, external :: io_DB1
 character(lchlen) :: core_io_path_save
 !
 ! Saves
 !
 core_io_path_save=core_io_path
 !
 ! Path redirection
 !
 core_io_path=DB1_path
 !
 ! When all the informations in the DB1 need to be reloaded
 ! I can use also a plain fortran database.
 ! 
 if (what/='all') then
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,ID=ID)
   io_DB1_selective_scan =io_connect(desc="db1",type=0,ID=ID)
   if (io_DB1_selective_scan/=0) then 
     call io_disconnect(ID=ID)
     return
   endif
 endif
 !
 if (what=="all") then
   !
   call bz_samp_reset(k)
   call E_reset(E)
   !
   if (allocated(g_vec)) then
     YAMBO_FREE(g_vec)
     YAMBO_FREE(dl_sop)
     YAMBO_FREE(rl_sop)
     YAMBO_FREE(wf_nc_k)
     YAMBO_FREE(wf_igk)
   endif
   if (allocated(n_atoms_species)) then
     YAMBO_FREE(n_atoms_species)
     YAMBO_FREE(Z_species)
     YAMBO_FREE(atom_pos)
   endif
   !
   ! DB1
   !
   call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=ID)
   io_DB1_selective_scan=io_DB1(E,k,ID)
   !
 endif
 if (index(what,'alat')>0) then
   call io_bulk(ID,VAR="LATTICE_PARAMETER",VAR_SZ=(/3/))
   call io_bulk(ID,R1=db1_alat)
 endif
 if (index(what,'lattice_vectors')>0) then
   call io_bulk(ID,VAR="LATTICE_VECTORS",VAR_SZ=(/3,3/))
   call io_bulk(ID,R2=db1_a)
 endif
 if (index(what,'E')>0) then
   call io_bulk(ID, VAR="EIGENVALUES", VAR_SZ=shape(E%E) )
   call io_bulk(ID, R3=E%E )
 endif
 if (index(what,'K')>0) then
   call io_bulk(ID, VAR="K-POINTS", VAR_SZ=shape(k%pt) )
   call io_bulk(ID, R2=k%pt)
 endif
 if (index(what,'nk')>0) then
   call io_elemental(ID, VAR="DIMENSIONS",VAR_SZ=17)
   do i_io=1,7
     call io_elemental(ID,I0=k%nibz)
   enddo
   E%nk=k%nibz
 endif
 if (index(what,'nb')>0) then
   call io_elemental(ID, VAR="DIMENSIONS",VAR_SZ=17)
   do i_io=1,6
     call io_elemental(ID,I0=E%nb)
   enddo
 endif
 !
 if (what/='all') call io_disconnect(ID=ID)
 !
 if (what=='all'.and.io_DB1_selective_scan>=0) then
   YAMBO_ALLOC(rl_sop,(3,3,nsym))
   forall (i1=1:3,i2=1:3,is=1:nsym) rl_sop(i1,i2,is)=dl_sop(i1,i2,is)*alat(i1)/alat(i2) 
 endif
 !
 core_io_path=core_io_path_save
 !
end function io_DB1_selective_scan

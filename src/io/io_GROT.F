!
!        Copyright (C) 2000-2019 the YAMBO team
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
integer function io_GROT(io_db)
 !
 use D_lattice,      ONLY:nsym,inv_index
 use R_lattice,      ONLY:n_g_shells,ng_closed,ng_in_shell,E_of_shell,g_rot,minus_G
#ifdef _CUDA
 use R_lattice,      ONLY:minus_G_d
#endif
 use IO_m,           ONLY:io_connect,io_disconnect,io_elemental,read_is_on,&
&                         io_status,io_bulk,io_header,io_sec,ver_is_gt_or_eq
 use timing_m,       ONLY:timing
#include<memory.h>
 integer :: io_db,nsym_db
 !
 call timing('io_GROT',OPR='start')
 !
 io_GROT=io_connect(desc='gops',type=1,ID=io_db)
 if (io_GROT/=0) goto 1
 !
 io_GROT=io_header(io_db,IMPOSE_SN=.true.)
 if (io_GROT/=0) goto 1
 !
 if (.not.ver_is_gt_or_eq(io_db,revision=15192)) then
   io_GROT=-1 
   goto 1
 endif
 !
 if (any((/io_sec(io_db,:)==1/))) then
   call io_elemental(io_db,VAR="PARS",VAR_SZ=3,MENU=0)
   call io_elemental(io_db,I0=nsym,DB_I0=nsym_db)
   call io_elemental(io_db,I0=ng_closed, CHECK=.true.,OP=(/"<="/))
   call io_elemental(io_db,I0=n_g_shells,CHECK=.true.,OP=(/"<="/))
   call io_elemental(io_db,VAR="",VAR_SZ=0,MENU=0)
   io_GROT=io_status(io_db)
   if (io_GROT/=0) goto 1
 endif
 !
 if (.not.any((/io_sec(io_db,:)==2/))) goto 1
 !
 if (read_is_on(io_db)) then
   YAMBO_ALLOC(ng_in_shell,(n_g_shells))
   YAMBO_ALLOC(g_rot,(ng_closed,nsym_db))
   YAMBO_ALLOC(E_of_shell,(n_g_shells))
   YAMBO_ALLOC(minus_G,(ng_closed))
 endif
 !
 call io_bulk(io_db,VAR="ng_in_shell",VAR_SZ=(/n_g_shells/))
 call io_bulk(io_db,I1=ng_in_shell(:n_g_shells))
 !
 call io_bulk(io_db,VAR="E_of_shell",VAR_SZ=(/n_g_shells/))
 call io_bulk(io_db,R1=E_of_shell(:n_g_shells))
 !
 call io_bulk(io_db,VAR="GROT",VAR_SZ=(/ng_closed,nsym_db/))
 call io_bulk(io_db,I2=g_rot(:ng_closed,:))
 !
 if (ver_is_gt_or_eq(io_db,revision=955).and.inv_index<0) then
   call io_bulk(io_db,VAR="minusG",VAR_SZ=(/ng_closed/))
   call io_bulk(io_db,I1=minus_G(:ng_closed))
 endif
#ifdef _CUDA
 if (read_is_on(io_db).and.inv_index<0) then
   YAMBO_FREE(minus_G_d)
   allocate(minus_G_d,source=minus_G)
 endif
#endif
 !
 call initactivate(1,'Gmode')
 !
1 call io_disconnect(ID=io_db)
 !
 call timing('io_GROT',OPR='stop')
 !
end function

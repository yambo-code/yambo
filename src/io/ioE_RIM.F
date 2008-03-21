!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
integer function ioE_RIM(E,ID)
 !
 use pars,           ONLY:SP,IP
 use electrons,      ONLY:levels,BZ_RIM_nkpt,BZ_RIM_table,BZ_RIM_max_filling,&
&                         BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_sp_pol
 use R_lattice,      ONLY:nXkbz
 use memory_m,       ONLY:mem_est
 use IO_m,           ONLY:io_connect,io_disconnect,io_elemental,read_is_on,&
&                         io_bulk,io_header,io_save_to_try_again,&
&                         io_restore_to_try_again
 implicit none
 !
 type(levels) :: E
 integer      :: ID 
 !
 call io_save_to_try_again(ID)
 ioE_RIM=io_connect(desc='E_RIM',type=2,ID=ID)
 ! 
 ! The DB is searched in the SAVE folder when
 ! it is not found in the JOBNAME folder
 !
 if (read_is_on(ID).and.ioE_RIM/=0) then
   call io_restore_to_try_again(ID)
   ioE_RIM=io_connect(desc='E_RIM',type=1,ID=ID)
 endif
 !
 if (ioE_RIM/=0) goto 1
 !
 ioE_RIM=io_header(ID)
 call io_elemental(ID,VAR="PARS",VAR_SZ=3,MENU=1)
 call io_elemental(ID,I0=BZ_RIM_nbands,    VAR=' Bands                           :')
 call io_elemental(ID,I0=BZ_RIM_tot_nkpts, VAR=' BZ RIM k-points                 :')
 call io_elemental(ID,I0=BZ_RIM_max_filling,VAR=' BZ Blocks max filling           :')
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 if (read_is_on(ID)) then
   allocate(E%E_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_sp_pol))
   allocate(E%f_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_sp_pol))
   allocate(BZ_RIM_nkpt(nXkbz),BZ_RIM_table(nXkbz,BZ_RIM_max_filling))
   call mem_est("E_RIM f_RIM BZ_RIM_nkpt BZ_RIM_table",&
&               (/size(E%E_RIM),size(E%f_RIM),size(BZ_RIM_nkpt),&
&                 size(BZ_RIM_table)/),(/SP,SP,IP,IP/))
 endif
 !
 ! Eigenvalues
 !
 call io_bulk(ID, VAR="RIM_EIGENVALUES", VAR_SZ=shape(E%E_RIM) ) 
 call io_bulk(ID, R3=E%E_RIM )
 !
 ! RIM Block filling
 !
 call io_bulk(ID, VAR="RIM_BLOCK_FILLING", VAR_SZ=shape(BZ_RIM_nkpt) ) 
 call io_bulk(ID, I1=BZ_RIM_nkpt )
 !
 ! RIM Block table
 !
 call io_bulk(ID, VAR="RIM_BLOCK_TABLE", VAR_SZ=shape(BZ_RIM_table) ) 
 call io_bulk(ID, I2=BZ_RIM_table )
 !
1 call io_disconnect(ID=ID)
 !
end function

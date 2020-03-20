!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
integer function io_E_SOC_map(En,kpt,ID)
 !
 use electrons,      ONLY:levels,E_SOC_table,E_SOC_nbands
 use R_lattice,      ONLY:bz_samp
 use IO_m,           ONLY:io_connect,io_disconnect,io_elemental,read_is_on,&
&                         io_bulk,io_header,file_is_present,write_is_on
#include<memory.h>
 !
 type(levels) :: En
 type(bz_samp):: kpt
 integer      :: ID 
 !
 ! Work Space
 !
 io_E_SOC_map=-1
 !
 if (file_is_present('E_SOC_map').or.write_is_on(ID)) io_E_SOC_map=io_connect(desc='E_SOC_map',type=2,ID=ID)
 !
 if (io_E_SOC_map/=0) goto 1
 !
 io_E_SOC_map=io_header(ID)
 !
 if (io_E_SOC_map/=0) goto 1
 !
 call io_elemental(ID,VAR="PARS",VAR_SZ=3,MENU=1)
 call io_elemental(ID,I0=E_SOC_nbands, VAR=' Number of SOC bands       ')
 call io_elemental(ID,I0=En%nb,        VAR=' Number of bands           ')
 call io_elemental(ID,I0=kpt%nibz,     VAR=' Number of points          ')
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 if (read_is_on(ID)) then
   YAMBO_ALLOC(En%E_SOC,(E_SOC_nbands,kpt%nibz))
   YAMBO_ALLOC(E_SOC_table,(2,En%nb,kpt%nibz))
 endif
 !
 ! Eigenvalues
 !
 call io_bulk(ID, VAR="EIGENVALUES", VAR_SZ=shape(En%E_SOC) ) 
 call io_bulk(ID, R2=En%E_SOC )
 !
 call io_bulk(ID, VAR="BLOCK_TABLE", VAR_SZ=shape(E_SOC_table) ) 
 call io_bulk(ID, I3=E_SOC_table )
 !
1 call io_disconnect(ID=ID)
 !
end function io_E_SOC_map

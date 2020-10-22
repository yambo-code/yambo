!
!        Copyright (C) 2000-2020 the YAMBO team
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
integer function ELPH_expanded_databases_check(n_bands)
 ! 
 use IO_m,           ONLY:io_control,REP,OP_RD_CL
 use ELPH,           ONLY:elph_nb
 !
 implicit none
 !
 integer                  :: n_bands
 !
 ! Work Space
 !
 integer             ::ID,io_err
 integer, external   ::io_ELPH
 !
 ! I/O check
 !===========
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_ELPH(ID,'gkkp_expanded')
 !
 ELPH_expanded_databases_check=0
 if (io_err/=0.or.elph_nb<n_bands) ELPH_expanded_databases_check=-1
 !
end function ELPH_expanded_databases_check

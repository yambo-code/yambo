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
SUBROUTINE SLK_communicators_init( idriver, COMM, INDEX_comm, N)
 !
 use parallel_m,     ONLY:MPI_comm,PAR_COM_INV_INDEX,PAR_COM_INV,PAR_COM_DIAGO,PAR_COM_DIAGO_INDEX,COMM_copy
 use linear_algebra, ONLY:INV,DIAGO
 use SLK_m,          ONLY:DIAGO
 use com,            ONLY:msg
 !
 IMPLICIT NONE
 !
 type(MPI_comm),    intent(in)    :: COMM , INDEX_comm
 integer,           intent(in)    :: N , idriver
 !
 if (N==0) then
   if (idriver==INV) then
     call COMM_copy(INDEX_comm,PAR_COM_INV_INDEX)
     call COMM_copy(COMM,      PAR_COM_INV)
   else if (idriver==DIAGO) then
     call COMM_copy(INDEX_comm,PAR_COM_DIAGO_INDEX)
     call COMM_copy(COMM      ,PAR_COM_DIAGO)
   endif
 endif
 !
END SUBROUTINE SLK_communicators_init

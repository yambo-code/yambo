!
!        Copyright (C) 2000-2014 the YAMBO team 
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
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
!
! Copyright (C) 2001-2007 Quantum-ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .

subroutine errore( calling_routine, message, ierr )
 !
 implicit none
 character(len=*), intent(in) :: calling_routine, message
 integer,          intent(in) :: ierr
 !
 ! ... the error message is written un the "*" unit
 !
 WRITE( UNIT = *, FMT = '(/,1X,78("%"))' )
 WRITE( UNIT = *, &
        FMT = '(5X,"from ",A," : error #",I10)' ) TRIM(calling_routine), ierr
 WRITE( UNIT = *, FMT = '(5X,A)' ) message
 WRITE( UNIT = *, FMT = '(1X,78("%"),/)' )
 !
 WRITE( *, '("     stopping ...")' )
 !
 STOP 2
 !
 RETURN
 ! 
end subroutine errore


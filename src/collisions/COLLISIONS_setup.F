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
subroutine COLLISIONS_setup( )
 !
 ! Here I allocate and define some objects of the COLLISIONS_group kind.
 !
 !type COLLISIONS_group
 !  character(schlen)     :: name="none"
 !  integer               :: kind=0
 !  integer               :: N           ! # of collisions (dimension of linked COLLISIONS_element)
 !  integer,     pointer  :: state(:,:)   => null()
 !  integer,     pointer  :: table(:,:)   => null()
 !  integer,     pointer  :: DB_map(:)    => null()
 !  integer,     pointer  :: IO_status(:) => null()
 !end type
 !
 use collision,     ONLY:GW_NEQ_collisions,Fock_collisions,SEX_collisions,P_collisions
 !
 !GW_NEQ_collisions%name=
 !GW_NEQ_collisions%kind=
 !
end subroutine

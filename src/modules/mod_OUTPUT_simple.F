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
module OUTPUT_simple
 !
 use pars,  ONLY:SP,schlen,lchlen,rZERO
 !
 integer, parameter       :: N_MAX_columns=100
 !
 type COLt
   integer            :: POS=0
   character(schlen)  :: title= " "
   real(SP)           :: value=rZERO
 end type
 !
 logical              :: TITLES_ordered=.FALSE.
 logical              :: TITLES_dumped=.FALSE.
 integer              :: N_columns = 0
 integer              :: N_columns_ordered = 0
 character(lchlen)    :: filename = " "
 character(schlen)    :: filename_short= " "
 type(COLt)           :: COL(N_MAX_columns)
 !
 interface
   !
   subroutine OUTPUT_unit_conversion(Ivar,Rvar,DESC,KIND,PARSER_call)
     use pars,  ONLY:SP
     real(SP)    ,optional :: Rvar
     integer     ,optional :: Ivar
     character(*),optional :: DESC
     character(*),optional :: KIND
     logical     ,optional :: PARSER_call
   end subroutine
   !
   subroutine OUTPUT_add_column(what,name,TITLES,VAL_TITLES,VALUES,ORDER,UNIT,KIND)
     use pars,  ONLY:SP
     character(*) , OPTIONAL :: what
     character(*) , OPTIONAL :: name
     character(*) , OPTIONAL :: TITLES(:)
     character(*) , OPTIONAL :: VAL_TITLES(:)
     real(SP)     , OPTIONAL :: VALUES(:)
     character(*) , OPTIONAL :: ORDER(:)
     character(*) , OPTIONAL :: UNIT
     character(*) , OPTIONAL :: KIND
   end subroutine
   !
 end interface
 !
end module

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
 integer, parameter       :: N_MAX_columns=30
 !
 type COLt
   integer            :: POS=0
   character(schlen)  :: title= " " 
   character(1)       :: KIND= " " 
   real(SP)           :: R_value=rZERO
   real(SP)           :: I_value=0
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
   subroutine OUTPUT_add_column(what,name,TITLES,I_VALUES,R_VALUES,ORDER,UNIT,KIND)
     use pars,  ONLY:SP
     character(*) , OPTIONAL :: what
     character(*) , OPTIONAL :: name
     character(*) , OPTIONAL :: TITLES(:)
     integer      , OPTIONAL :: I_VALUES(:)
     real(SP)     , OPTIONAL :: R_VALUES(:)
     character(*) , OPTIONAL :: ORDER(:)
     character(*) , OPTIONAL :: UNIT
     character(*) , OPTIONAL :: KIND
   end subroutine
   !
 end interface
 !
 contains
   !
   subroutine OUTPUT_reset(what)
     character(*) :: what
     integer      :: ic
     if (what=="row".or.what=="reset") then
       do ic=1,N_MAX_columns
         COL(ic)%POS=0
         COL(ic)%title=" "
         COL(ic)%KIND=" "
         COL(ic)%I_value=0
         COL(ic)%R_value=0.
       enddo
       N_columns = 0
       N_columns_ordered = 0
       TITLES_ordered=.FALSE.
     endif
     if (what=="reset") then
       TITLES_dumped=.FALSE.
       filename = " "
       filename_short= " "
     endif
   end subroutine
 !
end module

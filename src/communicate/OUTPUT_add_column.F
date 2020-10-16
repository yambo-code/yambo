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
subroutine OUTPUT_add_column(what,name,TITLES,VAL_TITLES,VALUES,ORDER,UNIT,KIND)
 !
 use pars,                ONLY:SP,schlen
 use OUTPUT_simple,       ONLY:COL,N_columns,filename,filename_short,OUTPUT_unit_conversion,&
&                              TITLES_dumped,N_MAX_columns,TITLES_ordered,N_columns_ordered
 use com,                 ONLY:msg,of_open_close
 use stderr,              ONLY:STRING_same
 !
 implicit none
 !
 character(*) , OPTIONAL :: what
 character(*) , OPTIONAL :: name
 character(*) , OPTIONAL :: TITLES(:)
 character(*) , OPTIONAL :: VAL_TITLES(:)
 real(SP)     , OPTIONAL :: VALUES(:)
 character(*) , OPTIONAL :: ORDER(:)
 character(*) , OPTIONAL :: UNIT
 character(*) , OPTIONAL :: KIND
 !
 ! Work Space
 !
 integer           :: N,i_t,i_o
 real(SP)          :: o_values(N_MAX_columns)
 character(schlen) :: o_titles(N_MAX_columns)
 !
 if (present(name)) then
   filename      =name
   filename_short=name
 endif
 !
 if (present(what)) then
   if (what=="reset".or.what=="close".or.what=="open") then
     N_columns=0
     N_columns_ordered=0
     TITLES_ordered=.FALSE.
     TITLES_dumped=.FALSE.
   endif
   if (what=="close") call of_open_close(filename)
   if (what=="open")  call of_open_close(filename,'ot')
 endif
 !
 if (present(TITLES)) then
   N=size(TITLES)
   do i_t=1,N
     N_columns=N_columns+1
     N_columns_ordered=N_columns_ordered+1
     COL(N_columns)%title=TITLES(i_t)
     COL(N_columns)%POS=N_columns
     if (present(VALUES)) then
       if (present(UNIT)) then
         COL(N_columns)%title=trim(COL(N_columns)%title)//" ["//UNIT//"]"
         if (present(KIND)) then
           call OUTPUT_unit_conversion(rVAR=VALUES(i_t),DESC=UNIT,KIND=KIND)
         else
           call OUTPUT_unit_conversion(rVAR=VALUES(i_t),DESC=UNIT)
         endif
       endif
       COL(N_columns)%value=VALUES(i_t)
     endif
   enddo
 endif
 !
 if (present(VAL_TITLES).and.present(VALUES)) then
   N=size(VAL_TITLES)
   do i_t=1,N
     do i_o=1,N
       if (STRING_same(trim(VAL_TITLES(i_t)),trim(COL(i_o)%title))) then
         if (present(UNIT)) then
           if (present(KIND)) then
             call OUTPUT_unit_conversion(rVAR=VALUES(i_t),DESC=UNIT,KIND=KIND)
           else
             call OUTPUT_unit_conversion(rVAR=VALUES(i_t),DESC=UNIT)
           endif
         endif
         COL(i_o)%value=VALUES(i_t)
       endif
     enddo
   enddo
 endif
 !
 if (present(ORDER)) then
   N=size(ORDER)
   if (.not.TITLES_ordered) then
     COL(:)%POS=0
     N_columns_ordered=0
   endif
   do i_t=1,N_columns
     do i_o=1,N
       if (index(COL(i_t)%title,trim(ORDER(i_o)))==1) COL(i_t)%POS=i_o+N_columns_ordered
     enddo
   enddo
   N_columns_ordered=N_columns_ordered+N
   TITLES_ordered=.TRUE.
 endif
 !
 if (.not.present(what)) return
 if (.not.what=="write") return
 !
 do i_t=1,N_columns
   if (COL(i_t)%POS==0) cycle
   o_titles(COL(i_t)%POS)=COL(i_t)%title
   o_values(COL(i_t)%POS)=COL(i_t)%value
 enddo
 !
 if (.not.TITLES_dumped) then
   call msg('o '//filename_short,'#')
   call msg('o '//filename_short,'#',(o_titles(:N_columns_ordered)),INDENT=0,USE_TABS=.TRUE.)
   call msg('o '//filename_short,'#')
   TITLES_dumped=.TRUE.
 endif
 !
 call msg('o '//filename_short,'',o_values(:N_columns_ordered),INDENT=-2,USE_TABS=.TRUE.)
 !
 N_columns=0
 N_columns_ordered=0
 TITLES_ordered=.FALSE.
 !
end subroutine

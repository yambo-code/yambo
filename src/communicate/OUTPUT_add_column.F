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
subroutine OUTPUT_add_column(what,name,TITLES,I_VALUES,R_VALUES,ORDER,UNIT,KIND)
 !
 use pars,                ONLY:SP,schlen,lchlen
 use OUTPUT_simple,       ONLY:COL,N_columns,filename,filename_short,OUTPUT_unit_conversion,&
&                              TITLES_dumped,N_MAX_columns,TITLES_ordered,N_columns_ordered,&
&                              OUTPUT_reset
 use com,                 ONLY:msg,of_open_close
 use stderr,              ONLY:STRING_same
 !
 implicit none
 !
 character(*) , OPTIONAL :: what
 character(*) , OPTIONAL :: name
 character(*) , OPTIONAL :: TITLES(:)
 integer      , OPTIONAL :: I_VALUES(:)
 real(SP)     , OPTIONAL :: R_VALUES(:)
 character(*) , OPTIONAL :: ORDER(:)
 character(*) , OPTIONAL :: UNIT
 character(*) , OPTIONAL :: KIND
 !
 ! Work Space
 !
 integer           :: N,M,i_t,i_o,i_COL
 logical           :: NEW_COL
 real(SP)          :: o_R_values(N_MAX_columns)
 integer           :: o_I_values(N_MAX_columns)
 character(1)      :: o_KINDS(N_MAX_columns)
 character(schlen) :: o_titles(N_MAX_columns)
 character(lchlen) :: MSG_string(N_MAX_columns)
 !
 if (present(name)) then
   filename      =name
   filename_short=name
 endif
 !
 if (present(what)) then
   if (what=="close") call of_open_close(filename)
   if (what=="open" ) call of_open_close(filename,'ot')
   if (what=="reset".or.what=="close") call OUTPUT_reset("reset")
 endif
 !
 if (present(TITLES)) then
   N=size(TITLES)
   do i_t=1,N
     NEW_COL=.TRUE.
     i_COL=N_columns+1
     do i_o=1,N
       if (STRING_same(trim(TITLES(i_t)),trim(COL(i_o)%title))) then
         i_COL=i_o
         NEW_COL=.FALSE.
       endif
     enddo
     if (NEW_COL) then
       N_columns=N_columns+1
       N_columns_ordered=N_columns_ordered+1
     endif
     COL(i_COL)%title=TITLES(i_t)
     COL(i_COL)%POS=i_COL
     if (present(I_VALUES)) then
       COL(i_COL)%I_value=I_VALUES(i_t)
       COL(i_COL)%KIND="I"
     endif
     if (present(R_VALUES)) then
       if (present(UNIT)) then
         COL(i_COL)%title=trim(COL(i_COL)%title)//" ["//UNIT//"]"
         if (present(KIND)) then
           call OUTPUT_unit_conversion(rVAR=R_VALUES(i_t),DESC=UNIT,KIND=KIND)
         else
           call OUTPUT_unit_conversion(rVAR=R_VALUES(i_t),DESC=UNIT)
         endif
       endif
       COL(i_COL)%R_value=R_VALUES(i_t)
       COL(i_COL)%KIND="R"
     endif
   enddo
 endif
 !
 if (present(ORDER)) then
   N=size(ORDER) 
   M=0
   if (.not.TITLES_ordered) then
     COL(:)%POS=0
     N_columns_ordered=0
   endif
   do i_t=1,N_columns
     do i_o=1,N
       if (index(COL(i_t)%title,trim(ORDER(i_o)))==1) then
         M=M+1
         COL(i_t)%POS=i_o+N_columns_ordered
       endif
     enddo
   enddo
   N_columns_ordered=N_columns_ordered+M
   TITLES_ordered=.TRUE.
 endif
 !
 if (.not.present(what)) return
 if (.not.what=="write") return
 !
 ! TITLES
 !--------
 do i_t=1,N_columns
   if (COL(i_t)%POS==0) cycle
   o_titles(COL(i_t)%POS)=COL(i_t)%title
 enddo
 if (.not.TITLES_dumped) then
   call msg('o '//filename_short,'#')
   call msg('o '//filename_short,'#',(o_titles(:N_columns_ordered)),INDENT=0,USE_TABS=.TRUE.)
   call msg('o '//filename_short,'#')
   TITLES_dumped=.TRUE.
 endif
 !
 ! DATA
 !-------
 MSG_string=" "
 do i_t=1,N_columns
   if (COL(i_t)%POS==0) cycle
   o_KINDS(COL(i_t)%POS)=COL(i_t)%KIND
   if (COL(i_t)%KIND=="R") o_R_values(COL(i_t)%POS)=COL(i_t)%R_value
   if (COL(i_t)%KIND=="I") o_I_values(COL(i_t)%POS)=COL(i_t)%I_value
 enddo
 !
 do i_t=1,N_columns_ordered
   if (o_KINDS(i_t)=="I") call msg('string','',o_I_values(i_t),USE_TABS=.TRUE.,MSG_string=MSG_string(i_t))
   if (o_KINDS(i_t)=="R") call msg('string','',o_R_values(i_t),USE_TABS=.TRUE.,MSG_string=MSG_string(i_t))
 enddo
 call msg('o '//filename_short,' ',MSG_string(:N_columns_ordered),INDENT=-2,USE_TABS=.TRUE.)
 !
 call OUTPUT_reset("row")
 !
end subroutine

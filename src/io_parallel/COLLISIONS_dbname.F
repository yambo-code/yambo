!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine COLLISIONS_dbname(coll_kind,db_name)
 !
 use pars,          ONLY:schlen
 use xc_functionals,ONLY:GS_xc_kind
 use global_XC,     ONLY:QP_SE_GW_NEQ,QP_SE_GW_QED,&
&                        H_SE_FOCK,H_SE_COH,H_SE_SEX,H_SE_HARTREE
 !
 implicit none
 !
 integer,           intent(in)  :: coll_kind
 character(schlen), intent(out) :: db_name
 !
 if(coll_kind==H_SE_COH) then
   db_name = "COLLISIONS_COH"
 elseif(coll_kind==H_SE_HARTREE.or.coll_kind==H_SE_FOCK.or.coll_kind==H_SE_SEX.or.coll_kind==GS_xc_kind.or.&
&       coll_kind>=H_SE_HARTREE) then
   db_name = "COLLISIONS_HXC"
 elseif(coll_kind==QP_SE_GW_NEQ) then
   db_name = "COLLISIONS_GW_NEQ"
 elseif(coll_kind==QP_SE_GW_QED) then
   db_name = "COLLISIONS_P"
 else
   call error("COLLISIONS I/O : Unknown database type ")
 endif
 !
end subroutine COLLISIONS_dbname

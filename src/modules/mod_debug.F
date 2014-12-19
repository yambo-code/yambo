!
!        Copyright (C) 2000-2014 the YAMBO team
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
module debug
 !
 use pars,    ONLY:schlen
 use com,     ONLY:msg
 use stderr,  ONLY:intc
 !
 implicit none
 !
 integer, parameter:: n_debugs_max=100
 character(schlen) :: debug_name(n_debugs_max) 
 integer           :: debug_point(n_debugs_max)
 !
 contains
   !
   subroutine dbg(name)
     !
     character(*) :: name
     integer      :: id
     !
     if (name=="reset") then
       debug_name=" "
       debug_point=0
     endif
     !
     do id=1,n_debugs_max
       if (name==trim(debug_name(id))) then
         debug_point(id)=debug_point(id)+1
         call msg('nln',name//" "//intc(debug_point(id)))
         return
       endif
     enddo
     !
     do id=1,n_debugs_max
       if (len_trim(debug_name(id))==0) then
         debug_name(id)=name
         debug_point(id)=debug_point(id)+1
         call msg('nln',name//" "//intc(debug_point(id)))
         return
       endif
       !
     enddo
     !
   end subroutine
   !
end module 

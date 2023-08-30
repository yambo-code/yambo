!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module debug
 !
 use pars,    ONLY:schlen,n_debugs_max
 use com,     ONLY:msg,secnm
 use stderr,  ONLY:intc
 !
 implicit none
 !
 character(schlen) :: debug_name(n_debugs_max) 
 integer           :: debug_point(n_debugs_max)
 !
 contains
   !
   subroutine dbg(name)
     !
     character(*), optional :: name
     integer                :: id
     character(schlen)      :: db_name
     !
     db_name=trim(secnm)
     if (present(name)) then
       db_name=name
     endif
     !
     if (trim(db_name)=="reset") then
       debug_name=" "
       debug_point=0
     endif
     !
     do id=1,n_debugs_max
       if (trim(db_name)==trim(debug_name(id))) then
         debug_point(id)=debug_point(id)+1
         call msg('nln',trim(db_name)//" "//intc(debug_point(id)))
         return
       endif
     enddo
     !
     do id=1,n_debugs_max
       if (len_trim(debug_name(id))==0) then
         debug_name(id)=trim(db_name)
         debug_point(id)=debug_point(id)+1
         call msg('nln',trim(db_name)//" "//intc(debug_point(id)))
         return
       endif
       !
     enddo
     !
   end subroutine
   !
end module 

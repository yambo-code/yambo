!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, PDA
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
module environ_m
 !
 use pars,      ONLY:SP
#include<memory.h>
 !
 logical  :: l_environ
 !
 integer  :: nr1,nr2,nr3
 integer  :: ng_env
 complex(SP), allocatable :: eps_env_g(:)
 !
contains
 !
 subroutine environ_alloc(ng_env_) 
   implicit none 
   integer :: ng_env_
   ng_env=ng_env_
   YAMBO_ALLOC(eps_env_g,(ng_env))
 end subroutine environ_alloc
 !
 subroutine environ_free() 
   YAMBO_FREE(eps_env_g)
 end subroutine environ_free
 !
end module environ_m


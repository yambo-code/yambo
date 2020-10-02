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
module RT_lifetimes
 !
 use pars,       ONLY:SP
 !
#include<memory.h>
 !
 type RT_lifetime
   ! Zero and Finite temp
   real(SP),    allocatable :: El_emit(:,:)
   real(SP),    allocatable :: Ho_abs(:,:)
   ! Finite temp
   real(SP),    allocatable :: El_abs(:,:)
   real(SP),    allocatable :: Ho_emit(:,:)
   logical                  :: Finite_T
 end type RT_lifetime
 !
 type RT_obj_lifetime
   real(SP),    allocatable :: bare(:)
   real(SP),    allocatable :: El_abs(:)
   real(SP),    allocatable :: Ho_abs(:)
   real(SP),    allocatable :: El_emit(:)
   real(SP),    allocatable :: Ho_emit(:)
   logical                  :: Finite_T
 end type RT_obj_lifetime
 !
 type(RT_lifetime)     :: RT_EE_life,RT_EP_life,RT_EH_life,RT_PE_life
 type(RT_obj_lifetime) :: RT_obj_EE_life,RT_obj_EP_life,RT_obj_EH_life,RT_obj_PE_life
 !
 contains
   !
   subroutine RT_lifetime_alloc(LIFE,Finite_T,Nb,Nk)
     type(RT_lifetime) :: LIFE
     integer :: Nb,Nk
     logical :: Finite_T
     YAMBO_ALLOC(LIFE%El_emit,(Nb(1):Nb(2),Nk))
     YAMBO_ALLOC(LIFE%Ho_abs,(Nb(1):Nb(2),Nk))
     LIFE%Finite_T=Finite_T
     if (.not.Finite_T) return
     YAMBO_ALLOC(LIFE%El_abs,(Nb(1):Nb(2),Nk))
     YAMBO_ALLOC(LIFE%Ho_emit,(Nb(1):Nb(2),Nk))
   end subroutine
   !
   subroutine RT_lifetime_free(LIFE)
     type(RT_lifetime) :: LIFE
     LIFE%Finite_T=.FALSE.
     YAMBO_FREE(LIFE%El_emit)
     YAMBO_FREE(LIFE%Ho_emit)
     YAMBO_FREE(LIFE%El_abs)
     YAMBO_FREE(LIFE%Ho_abs)
   end subroutine
   !
   subroutine RT_obj_lifetime_alloc(LIFE,Finite_T,Ns)
     type(RT_obj_lifetime) :: LIFE
     integer :: Ns
     logical :: Finite_T
     YAMBO_ALLOC(LIFE%El_emit,(Ns))
     YAMBO_ALLOC(LIFE%Ho_abs,(Ns))
     YAMBO_ALLOC(LIFE%bare,(Ns))
     LIFE%Finite_T=Finite_T
     if (.not.Finite_T) return
     YAMBO_ALLOC(LIFE%El_abs,(Ns))
     YAMBO_ALLOC(LIFE%Ho_emit,(Ns))
   end subroutine
   !
   subroutine RT_obj_lifetime_free(LIFE)
     type(RT_obj_lifetime) :: LIFE
     LIFE%Finite_T=.FALSE.
     YAMBO_FREE(LIFE%El_emit)
     YAMBO_FREE(LIFE%Ho_emit)
     YAMBO_FREE(LIFE%El_abs)
     YAMBO_FREE(LIFE%Ho_abs)
     YAMBO_FREE(LIFE%bare)
   end subroutine
   !
end module

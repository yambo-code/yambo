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
module RT_occupations
 !
 use pars,       ONLY:SP,rZERO,schlen
 !
#include<memory.h>
 !
 type RT_occ
   logical                  :: active=.FALSE.
   character(schlen)        :: KIND="none"
   integer                  :: D1(2)
   integer                  :: D2
   real(SP),    allocatable :: N(:,:)
   real(SP),    allocatable :: N_ref(:,:)
   real(SP),    allocatable :: dN(:,:)
 end type RT_occ
 !
 type(RT_occ) :: RT_el_occ,RT_ho_occ,RT_ph_occ
 !
 contains
   !
   subroutine RT_occ_alloc(KIND,OCC,D1,D2)
     character(*) :: KIND
     type(RT_occ) :: OCC
     integer :: D1(2),D2
     OCC%D1=D1
     OCC%D2=D2
     OCC%KIND=KIND
     OCC%active=.true.
     YAMBO_ALLOC(OCC%N,(D1(1):D1(2),D2))
     OCC%N=rZERO
     if (.not.KIND=="El") return
     YAMBO_ALLOC(OCC%dN,(D1(1):D1(2),D2))
     OCC%dN =rZERO
     YAMBO_ALLOC(OCC%N_ref,(D1(1):D1(2),D2))
     OCC%N_ref=rZERO
   end subroutine
   !
   subroutine RT_alloc_free(OCC)
     type(RT_occ) :: OCC
     OCC%D1=0
     OCC%D2=0
     OCC%KIND="none"
     OCC%active=.FALSE.
     YAMBO_FREE(OCC%N)
     YAMBO_FREE(OCC%dN)
     YAMBO_FREE(OCC%N_ref)
   end subroutine
 !
end module

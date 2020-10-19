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
 use pars,           ONLY:SP,rZERO,schlen
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elphoton_scatt,&
&                         l_phel_scatt,l_phel_corr
 !
#include<memory.h>
 !
 type RT_occupation
   character(schlen)        :: KIND
   logical                  :: active
   integer                  :: D1(2)
   integer                  :: D2
   real(SP),    allocatable :: N(:,:)
   real(SP),    allocatable :: N_ref(:,:)
   real(SP),    allocatable :: dN(:,:)
 end type RT_occupation
 !
 type(RT_occupation) :: RT_el_occ,RT_ho_occ,RT_ph_occ,RT_life_occ
 !
 type RT_obj_occupation
   logical                  :: active=.FALSE.
   character(schlen)        :: KIND="none"
   integer                  :: D=0
   real(SP),    allocatable :: N_ref(:)
   real(SP),    allocatable :: dN(:)
 end type RT_obj_occupation
 !
 type(RT_obj_occupation) :: RT_obj_el_occ,RT_obj_ph_occ
 !
 ! Relaxation Time Approx
 !
 logical  :: l_RT_relax_time_approx = .FALSE.
 real(SP) :: RT_RTA_temp(2)
 real(SP) :: RT_RTA_chem(2)
 real(SP), allocatable :: RT_RTA_E_occ_infty(:,:)
 real(SP), allocatable :: RT_RTA_H_occ_infty(:,:)
 !
 contains
   !
   subroutine RT_EL_occupations_alloc(D1,D2)
     integer :: D1(2),D2
     call RT_occupation_alloc("electrons",RT_el_occ,D1,D2)
     call RT_occupation_alloc("holes",RT_ho_occ,D1,D2)
     if (l_elph_scatt.or.l_elel_scatt.or.l_elphoton_scatt) call RT_occupation_alloc("life",RT_life_occ,D1,D2)
   end subroutine
   !
   subroutine RT_PH_occupations_alloc(D1,D2)
     integer :: D1(2),D2
     if (l_phel_scatt.or.l_phel_corr) call RT_occupation_alloc("phonons",RT_ph_occ,D1,D2)
   end subroutine
   !
   subroutine RT_ALL_occupations_free( )
     call RT_occupation_free(RT_el_occ)
     call RT_occupation_free(RT_ho_occ)
     if (l_phel_scatt.or.l_phel_corr) call RT_occupation_free(RT_ph_occ)
     if (l_elph_scatt.or.l_elel_scatt.or.l_elphoton_scatt) call RT_occupation_free(RT_life_occ)
   end subroutine
   !
   subroutine RT_occupation_clean(OCC)
     type(RT_occupation) :: OCC
     if (allocated(OCC%dN)) then
       OCC%dN=rZERO
     endif
     if (allocated(OCC%N)) then
       OCC%N=rZERO
     endif
     if (allocated(OCC%N_ref)) then
       OCC%N_ref=rZERO
     endif
   end subroutine
   !
   subroutine RT_occupation_alloc(KIND,OCC,D1,D2)
     character(*)        :: KIND
     type(RT_occupation) :: OCC
     integer :: D1(2),D2
     OCC%D1=D1
     OCC%D2=D2
     if (KIND=="electrons".or.KIND=="life") then
       YAMBO_ALLOC(OCC%dN,(D1(1):D1(2),D2))
       OCC%dN=rZERO
     endif
     if (KIND=="electrons".or.KIND=="holes".or.KIND=="phonons") then
       YAMBO_ALLOC(OCC%N,(D1(1):D1(2),D2))
       YAMBO_ALLOC(OCC%N_ref,(D1(1):D1(2),D2))
       OCC%N=rZERO
       OCC%N_ref=rZERO
     endif
     OCC%KIND=KIND
     OCC%active=.true.
   end subroutine
   !
   subroutine RT_occupation_free(OCC)
     type(RT_occupation) :: OCC
     OCC%KIND="none"
     OCC%active=.FALSE.
     YAMBO_FREE(OCC%N)
     YAMBO_FREE(OCC%dN)
     YAMBO_FREE(OCC%N_ref)
   end subroutine
   !
   subroutine RT_obj_occupation_alloc(KIND,OCC,Ns)
     character(*)            :: KIND
     type(RT_obj_occupation) :: OCC
     integer :: Ns
     if (.not.OCC%active) return
     YAMBO_ALLOC(OCC%N_ref,(Ns))
     YAMBO_ALLOC(OCC%dN,(Ns))
     OCC%D        =Ns
     OCC%N_ref    =rZERO
     OCC%dN       =rZERO
     OCC%KIND     =kind
     OCC%active   =.TRUE.
   end subroutine
   !
   subroutine RT_obj_occupation_free(OCC)
     type(RT_obj_occupation) :: OCC
     if (.not.OCC%active) return
     OCC%KIND="none"
     OCC%active=.FALSE.
     OCC%D=0
     YAMBO_FREE(OCC%N_ref)
     YAMBO_FREE(OCC%dN)
   end subroutine
   !
end module

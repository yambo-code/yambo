!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG DS
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
module PH_real_time
 !
 use drivers,    ONLY:l_phel_scatt
 use pars,       ONLY:DP,SP,schlen,lchlen,LP
 use units,      ONLY:FS2AUT
 use global_XC,  ONLY:QP_SE_NONE
 !
#include<memory.h>
 !
 ! LOGICALS
 !
 real(SP),    allocatable :: RT_PH_occupations(:,:)
 real(SP),    allocatable :: RT_PH_delta_occupations(:,:)    ! Running values. Updated at each time step.
 real(SP),    allocatable :: RT_PH_occupations_reference(:,:)
 !
 ! PH-EL RT life-times
 !-----------------------------------------
 real(SP),    allocatable :: P_E_sp_res_lifetime(:,:)
 real(SP),    allocatable :: P_E_sp_ares_lifetime(:,:)
 real(SP),    allocatable :: P_E_eh_lifetime(:,:)
 ! ... Q-parallelization  and GKKP
 real(SP),    allocatable :: phel_gkkp_expanded_sq(:,:,:,:,:)
 !
 contains
   !
   subroutine RT_PH_alloc(what)
     !
     use pars,           ONLY:SP,LP,schlen,cZERO
     use drivers,        ONLY:l_phel_scatt
     use ELPH,           ONLY:elph_branches
     use R_lattice,      ONLY:nqbz
     !
     implicit none
     !
     character(*), intent(in) :: what
     !
     logical :: l_phonons,l_all
     !
     l_phonons = trim(what)=="phonons"
     l_all      = trim(what)=="all"
     !
     if (l_phel_scatt) then
       !
       YAMBO_ALLOC(P_E_sp_res_lifetime,(elph_branches(1):elph_branches(2),nqbz))
       YAMBO_ALLOC(P_E_sp_ares_lifetime,(elph_branches(1):elph_branches(2),nqbz))
       YAMBO_ALLOC(P_E_eh_lifetime,(elph_branches(1):elph_branches(2),nqbz))
       P_E_sp_res_lifetime=0._SP
       P_E_sp_ares_lifetime=0._SP
       P_E_eh_lifetime=0._SP
       !
     endif
     !
     if(l_all.or.l_phonons) then
       !
       ! Occ's ...
       YAMBO_ALLOC(RT_PH_occupations,(elph_branches(1):elph_branches(2),nqbz))
       YAMBO_ALLOC(RT_PH_occupations_reference,(elph_branches(1):elph_branches(2),nqbz))
       YAMBO_ALLOC(RT_PH_delta_occupations,(elph_branches(1):elph_branches(2),nqbz))
       RT_PH_occupations_reference=0._SP
       !
     endif
     !
   end subroutine RT_PH_alloc
   !
   subroutine RT_PH_free(what)
     !
     implicit none
     !
     character(*), intent(in) :: what
     !
     logical :: l_phonons,l_all
     !
     l_phonons = trim(what)=="phonons"
     l_all      = trim(what)=="all"
     !
     YAMBO_FREE(P_E_sp_res_lifetime)
     YAMBO_FREE(P_E_sp_ares_lifetime)
     YAMBO_FREE(P_E_eh_lifetime)
     YAMBO_FREE(phel_gkkp_expanded_sq)
     !
     if(l_all.or.l_phonons) then
       !
       YAMBO_FREE(RT_PH_occupations)
       YAMBO_FREE(RT_PH_occupations_reference)
       YAMBO_FREE(RT_PH_delta_occupations)
       !
     endif
     !
   end subroutine RT_PH_free
   !
end module

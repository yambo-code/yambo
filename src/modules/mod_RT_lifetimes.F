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
 use pars,           ONLY:SP,rZERO,schlen
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elphoton_scatt,l_phel_scatt
 !
#include<memory.h>
 !
 type RT_lifetime
   logical                  :: active=.FALSE.
   character(schlen)        :: KIND="none"
   integer                  :: D1(2)=0
   integer                  :: D2=0
   real(SP),    allocatable :: abs(:,:)
   real(SP),    allocatable :: emit(:,:)
   real(SP),    allocatable :: abs_save(:,:,:)
   real(SP),    allocatable :: emit_save(:,:,:)
   real(SP)                 :: prefactor(2)=rZERO ! 1=abs, 2=emit
   real(SP)                 :: mean(2)=rZERO      ! 
   real(SP)                 :: Tfit(2)=rZERO      ! 
 end type RT_lifetime
 !
 type RT_obj_lifetime
   logical                  :: active=.FALSE.
   character(schlen)        :: KIND="none"
   integer                  :: D=0
   real(SP),    allocatable :: abs(:)
   real(SP),    allocatable :: emit(:)
   real(SP),    allocatable :: abs_bare(:)
   real(SP),    allocatable :: emit_bare(:)
 end type RT_obj_lifetime
 !
 ! Lifetimes (updated)
 !-----------
 type(RT_lifetime)     :: RT_EE_life,RT_EP_emit_life,RT_EH_life,RT_PE_life,RT_EP_abs_life
 type(RT_obj_lifetime) :: RT_obj_EE_life,RT_obj_EP_emit_life,RT_obj_EH_life,RT_obj_PE_life,RT_obj_EP_abs_life
 !DEBUG> (FP)
 real(SP), allocatable :: PE_linewidth(:,:)
 !DEBUG<
 !
 ! Lifetimes (REFERENCE)
 !-----------
 type(RT_lifetime)     :: RT_EE_REF_life,RT_EP_emit_REF_life,RT_EP_abs_REF_life,RT_EH_REF_life,RT_PE_REF_life
 ! 
 ! Locks, reference & Logicals
 !-----------------------------
 logical                  :: l_RT_relaxation_update_reference
 logical                  :: l_RT_constant_lifetimes
 logical                  :: l_RT_extrapolate_lifetimes
 logical                  :: l_RT_eval_lifetimes
 logical                  :: l_initial_equilibrium
 !
 ! Memory
 !--------
 integer  :: Life_MEM_steps
 !
 ! Here I control the way the time evolution is sampled for the e-p and e-e
 ! channels. I will see later if this can be somehow extended/duplicated
 ! for the static correlations
 !
 logical  :: l_RT_lifetimes_extrapolation = .FALSE.! If .TRUE. the lifetimes not calculated are extrapolated. Otherwise are kept constant
 real(SP) :: RT_life_extrap_times(2)               ! Lifetimes are evaluated at distance RT_life_extrap_times(2) and in groups long RT_life_extrap_times(1)
 real(SP) :: RT_life_extrap_times_INPUT(2)
 !
 ! Saved RT life-times
 !-----------------------
 real(SP),    allocatable :: SAVED_times(:)
 !
 ! Ouput Lifetimes, ratio fit and temperatures
 !---------------------------------------------
 logical  :: l_RT_lifetimes_Tfit=.FALSE.
 !
 ! Skip Finite (prop to N_ph) components
 !---------------------------------------
 logical  :: l_RT_skip_ph_abs_lifetimes = .FALSE.
 !
 ! ... Q weights calculated using RIM in order to evaluate the ELPH kernel
 real(SP),    allocatable :: q_weight_RT(:,:,:)
 !
 ! Beyond Complete Collision Approx 
 !----------------------------------
 logical     :: l_RT_iterative_ELPH_lifetimes=.FALSE.
 !
 contains
   !
   subroutine RT_EL_lifetimes_alloc(D1,D2)
     use D_lattice, ONLY:Bose_Temp
     integer :: D1(2),D2
     if (l_elph_scatt) then
       call RT_lifetime_alloc("el-ph_emit",RT_EP_emit_life,D1,D2)
       call RT_lifetime_alloc("el-ph_emit-REF",RT_EP_emit_REF_life,D1,D2)
       if (Bose_Temp>0._SP.or.l_phel_scatt) then
         call RT_lifetime_alloc("el-ph_abs",RT_EP_abs_life,D1,D2)
         call RT_lifetime_alloc("el-ph_abs-REF",RT_EP_abs_REF_life,D1,D2)
       endif
     endif
     if (l_elel_scatt) then
       call RT_lifetime_alloc("el-el",RT_EE_life,D1,D2)
       call RT_lifetime_alloc("el-el-REF",RT_EE_REF_life,D1,D2)
     endif
     if (l_elphoton_scatt) then
       call RT_lifetime_alloc("el-photon",RT_EH_life,D1,D2)
       call RT_lifetime_alloc("el-photon-REF",RT_EH_REF_life,D1,D2)
     endif
     YAMBO_ALLOC(SAVED_times,(Life_MEM_steps))
     SAVED_times=rZERO
   end subroutine
   !
   subroutine RT_PH_lifetimes_alloc(D1,D2)
     integer :: D1(2),D2
     if (l_phel_scatt) then
       call RT_lifetime_alloc("ph-el",RT_PE_life,D1,D2)
       call RT_lifetime_alloc("ph-el-REF",RT_PE_REF_life,D1,D2)
     endif
   end subroutine
   !
   subroutine RT_ALL_lifetimes_free( )
     if (l_elph_scatt)     call RT_lifetime_free(RT_EP_emit_life)
     if (l_elph_scatt)     call RT_lifetime_free(RT_EP_abs_life)
     if (l_elel_scatt)     call RT_lifetime_free(RT_EE_life)
     if (l_elphoton_scatt) call RT_lifetime_free(RT_EH_life)
     if (l_elph_scatt)     call RT_lifetime_free(RT_EP_emit_REF_life)
     if (l_elph_scatt)     call RT_lifetime_free(RT_EP_abs_REF_life)
     if (l_elel_scatt)     call RT_lifetime_free(RT_EE_REF_life)
     if (l_elphoton_scatt) call RT_lifetime_free(RT_EH_REF_life)
     YAMBO_FREE(SAVED_times)
   end subroutine
   !
   subroutine RT_lifetime_alloc(KIND,LIFE,D1,D2)
     character(*)      :: KIND
     type(RT_lifetime) :: LIFE
     integer :: D1(2),D2
     LIFE%D1=D1
     LIFE%D2=D2
     YAMBO_ALLOC(LIFE%emit,(D1(1):D1(2),D2))
     YAMBO_ALLOC(LIFE%abs,(D1(1):D1(2),D2))
     YAMBO_ALLOC(LIFE%emit_save,(D1(1):D1(2),D2,Life_MEM_steps))
     YAMBO_ALLOC(LIFE%abs_save,(D1(1):D1(2),D2,Life_MEM_steps))
     LIFE%KIND=KIND
     LIFE%active=.true.
     LIFE%emit=rZERO
     LIFE%abs =rZERO
     LIFE%emit_save=rZERO
     LIFE%abs_save=rZERO
     LIFE%prefactor=rZERO
     LIFE%mean=rZERO
     LIFE%Tfit=rZERO
   end subroutine
   !
   subroutine RT_lifetime_free(LIFE)
     type(RT_lifetime) :: LIFE
     LIFE%KIND="none"
     LIFE%active=.FALSE.
     YAMBO_FREE(LIFE%emit)
     YAMBO_FREE(LIFE%abs)
     YAMBO_FREE(LIFE%abs_save)
     YAMBO_FREE(LIFE%emit_save)
   end subroutine
   !
   subroutine RT_obj_lifetime_alloc(KIND,LIFE,Ns)
     character(*)          :: KIND
     type(RT_obj_lifetime) :: LIFE
     integer :: Ns
     if (.not.LIFE%active) return
     YAMBO_ALLOC(LIFE%emit,(Ns))
     YAMBO_ALLOC(LIFE%abs,(Ns))
     YAMBO_ALLOC(LIFE%abs_bare,(Ns))
     YAMBO_ALLOC(LIFE%emit_bare,(Ns))
     LIFE%D        =Ns
     LIFE%emit     =rZERO
     LIFE%abs      =rZERO
     LIFE%abs_bare =rZERO
     LIFE%emit_bare=rZERO
     LIFE%KIND     =kind
     LIFE%active   =.TRUE.
   end subroutine
   !
   subroutine RT_obj_lifetime_free(LIFE)
     type(RT_obj_lifetime) :: LIFE
     if (.not.LIFE%active) return
     LIFE%KIND="none"
     LIFE%active=.FALSE.
     LIFE%D=0
     YAMBO_FREE(LIFE%emit)
     YAMBO_FREE(LIFE%abs)
     YAMBO_FREE(LIFE%abs_bare)
     YAMBO_FREE(LIFE%emit_bare)
   end subroutine
   !
end module

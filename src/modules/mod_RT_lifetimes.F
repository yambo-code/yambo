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
   ! Spontaneous terms
   real(SP),    allocatable :: abs(:,:)
   real(SP),    allocatable :: emit(:,:)
   real(SP),    allocatable :: save(:,:,:)
   real(SP)                 :: prefactor(2)=rZERO ! 1=valence, 2=conduction
   real(SP)                 :: mean(2)=rZERO      ! 1=EMIT, 2=ABS
   real(SP)                 :: Tfit(2)=rZERO      ! 1=EMIT, 2=ABS
   ! Stimulated terms
   logical                  :: STIMULATED_components=.FALSE.
   real(SP),    allocatable :: STIM_abs(:,:)
   real(SP),    allocatable :: STIM_emit(:,:)
   real(SP),    allocatable :: STIM_save(:,:,:)
   real(SP)                 :: STIM_prefactor(2)=rZERO
   real(SP)                 :: STIM_mean(2)=rZERO
   real(SP)                 :: STIM_Tfit(2)=rZERO
 end type RT_lifetime
 !
 type RT_obj_lifetime
   logical                  :: active=.FALSE.
   character(schlen)        :: KIND="none"
   integer                  :: N=0
   real(SP),    allocatable :: bare(:)
   ! Spontaneous terms
   real(SP),    allocatable :: abs(:)
   real(SP),    allocatable :: emit(:)
   ! Stimulated terms
   logical                  :: STIMULATED_components=.FALSE.
   real(SP),    allocatable :: STIM_abs(:)
   real(SP),    allocatable :: STIM_emit(:)
 end type RT_obj_lifetime
 !
 ! Lifetimes (updated)
 !-----------
 type(RT_lifetime)     :: RT_EE_life,RT_EP_emit_life,RT_EH_life,RT_PE_life
 type(RT_obj_lifetime) :: RT_obj_EE_life,RT_obj_EP_emit_life,RT_obj_EH_life,RT_obj_PE_life
 !
 ! Lifetimes (REFERENCE)
 !-----------
 type(RT_lifetime)     :: RT_EE_REF_life,RT_EP_REF_life,RT_EH_REF_life,RT_PE_REF_life
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
 real(SP),    allocatable :: SAVED_lifetime(:,:,:,:)
 !
 ! Ouput Lifetimes, ratio fit and temperatures
 !---------------------------------------------
 logical                  :: l_lftm_fit_temperatures=.FALSE.
 !
 real(SP)                 :: RT_Tfit_lifetimes(4)=rZERO
 real(SP)                 :: RT_Efit_lifetimes(4)=rZERO
 !
 contains
   !
   subroutine RT_ALL_lifetimes_alloc( )
     !
     use D_lattice, ONLY:Bose_Temp
     use real_time, ONLY:RT_bands,RT_nk
     logical           ::stimulated_components
     !
     if (l_elph_scatt) then
       stimulated_components=.TRUE.!Bose_Temp>0._SP.or.l_phel_scatt
       call RT_lifetime_alloc("El-Ph",RT_EP_emit_life,stimulated_components,RT_bands,RT_nk)
       call RT_lifetime_alloc("El-Ph-REF",RT_EP_REF_life,stimulated_components,RT_bands,RT_nk)
     endif
     if (l_phel_scatt) then
       !call RT_lifetime_alloc("Ph-El",RT_PE_life,.TRUE.,RT_bands,RT_nk)
       !call RT_lifetime_alloc("Ph-El",RT_PE_REF_life,.TRUE.,RT_bands,RT_nk)
     endif
     if (l_elel_scatt) then
       call RT_lifetime_alloc("El-El",RT_EE_life,.FALSE.,RT_bands,RT_nk)
       call RT_lifetime_alloc("El-El-REF",RT_EE_REF_life,.FALSE.,RT_bands,RT_nk)
     endif
     if (l_elphoton_scatt) then
       call RT_lifetime_alloc("El-Photon",RT_EH_life,.FALSE.,RT_bands,RT_nk)
       call RT_lifetime_alloc("El-Photon-REF",RT_EH_REF_life,.FALSE.,RT_bands,RT_nk)
     endif
     !
     if (l_elel_scatt.or.l_elph_scatt.or.l_elphoton_scatt) then
       if (Life_MEM_steps>0) then
         YAMBO_ALLOC(SAVED_lifetime,(RT_bands(1):RT_bands(2),RT_nk,8,Life_MEM_steps))!e-eh,e-ee,e-ph,e-pe,e-ph(abs),e-pe(abs),e-gh,e-g)
         YAMBO_ALLOC(SAVED_times,(Life_MEM_steps))
         SAVED_lifetime=rZERO
         SAVED_times=rZERO
       endif
     endif
   end subroutine
   !
   subroutine RT_ALL_lifetimes_free( )
     if (l_elph_scatt)     call RT_lifetime_free(RT_EP_emit_life)
     if (l_elel_scatt)     call RT_lifetime_free(RT_EE_life)
     if (l_elphoton_scatt) call RT_lifetime_free(RT_EH_life)
     if (l_elph_scatt)     call RT_lifetime_free(RT_EP_REF_life)
     if (l_elel_scatt)     call RT_lifetime_free(RT_EE_REF_life)
     if (l_elphoton_scatt) call RT_lifetime_free(RT_EH_REF_life)
     YAMBO_FREE(SAVED_lifetime)
   end subroutine
   !
   subroutine RT_lifetime_alloc(KIND,LIFE,STIMULATED_components,Nb,Nk)
     character(*)      :: KIND
     type(RT_lifetime) :: LIFE
     integer :: Nb(2),Nk
     logical :: STIMULATED_components
     LIFE%D1=Nb
     LIFE%D2=Nk
     YAMBO_ALLOC(LIFE%emit,(Nb(1):Nb(2),Nk))
     YAMBO_ALLOC(LIFE%abs,(Nb(1):Nb(2),Nk))
     LIFE%STIMULATED_components=STIMULATED_components
     LIFE%KIND=KIND
     LIFE%active=.true.
     LIFE%emit=rZERO
     LIFE%abs =rZERO
     if (.not.STIMULATED_components) return
     YAMBO_ALLOC(LIFE%STIM_abs,(Nb(1):Nb(2),Nk))
     YAMBO_ALLOC(LIFE%STIM_emit,(Nb(1):Nb(2),Nk))
     LIFE%STIM_abs =rZERO
     LIFE%STIM_emit=rZERO
   end subroutine
   !
   subroutine RT_lifetime_free(LIFE)
     type(RT_lifetime) :: LIFE
     LIFE%KIND="none"
     LIFE%active=.FALSE.
     LIFE%STIMULATED_components=.FALSE.
     LIFE%D1=0
     LIFE%D2=0
     YAMBO_FREE(LIFE%emit)
     YAMBO_FREE(LIFE%STIM_emit)
     YAMBO_FREE(LIFE%STIM_abs)
     YAMBO_FREE(LIFE%abs)
   end subroutine
   !
   subroutine RT_obj_lifetime_alloc(KIND,LIFE,STIMULATED_components,Ns)
     character(*)          :: KIND
     type(RT_obj_lifetime) :: LIFE
     integer :: Ns
     logical :: STIMULATED_components
     YAMBO_ALLOC(LIFE%emit,(Ns))
     YAMBO_ALLOC(LIFE%abs,(Ns))
     YAMBO_ALLOC(LIFE%bare,(Ns))
     LIFE%emit=rZERO
     LIFE%abs =rZERO
     LIFE%bare   =rZERO
     LIFE%KIND=kind
     LIFE%active=.TRUE.
     LIFE%STIMULATED_components=STIMULATED_components
     if (.not.STIMULATED_components) return
     YAMBO_ALLOC(LIFE%STIM_abs,(Ns))
     YAMBO_ALLOC(LIFE%STIM_emit,(Ns))
     LIFE%STIM_abs =rZERO
     LIFE%STIM_emit=rZERO
   end subroutine
   !
   subroutine RT_obj_lifetime_free(LIFE)
     type(RT_obj_lifetime) :: LIFE
     LIFE%KIND="none"
     LIFE%active=.FALSE.
     LIFE%STIMULATED_components=.FALSE.
     YAMBO_FREE(LIFE%emit)
     YAMBO_FREE(LIFE%STIM_emit)
     YAMBO_FREE(LIFE%STIM_abs)
     YAMBO_FREE(LIFE%abs)
     YAMBO_FREE(LIFE%bare)
   end subroutine
   !
   subroutine RT_lifetime_copy(LIFEin,LIFEout)
     type(RT_lifetime) :: LIFEin,LIFEout
     !
     if (.not.LIFEin%active) return
     !
     LIFEout%STIMULATED_components=LIFEin%STIMULATED_components
     LIFEout%KIND=LIFEin%KIND
     LIFEout%active=LIFEin%active
     LIFEout%D1=LIFEin%D1
     LIFEout%D2=LIFEin%D2
     LIFEout%prefactor=LIFEin%prefactor
     LIFEout%STIM_prefactor =LIFEin%STIM_prefactor
     !
     if (.not.allocated(LIFEout%emit)) then
       YAMBO_ALLOC(LIFEout%emit,(LIFEout%D1(1):LIFEout%D1(2),LIFEout%D2))
       YAMBO_ALLOC(LIFEout%abs,(LIFEout%D1(1):LIFEout%D1(2),LIFEout%D2))
     endif
     LIFEout%abs=LIFEin%abs
     LIFEout%emit=LIFEin%emit
     !
     if (.not.LIFEin%STIMULATED_components) return
     !
     if (.not.allocated(LIFEout%STIM_emit)) then
       YAMBO_ALLOC(LIFEout%STIM_abs,(LIFEout%D1(1):LIFEout%D1(2),LIFEout%D2))
       YAMBO_ALLOC(LIFEout%STIM_emit,(LIFEout%D1(1):LIFEout%D1(2),LIFEout%D2))
     endif
     LIFEout%STIM_abs=LIFEin%STIM_abs
     LIFEout%STIM_emit=LIFEin%STIM_emit
     !
   end subroutine
   !
end module

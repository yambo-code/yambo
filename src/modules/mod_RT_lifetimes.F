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
 use pars,           ONLY:SP,rZERO
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elphoton_scatt
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
   logical                  :: Finite_T_components
 end type RT_lifetime
 !
 type RT_obj_lifetime
   real(SP),    allocatable :: bare(:)
   real(SP),    allocatable :: El_abs(:)
   real(SP),    allocatable :: Ho_abs(:)
   real(SP),    allocatable :: El_emit(:)
   real(SP),    allocatable :: Ho_emit(:)
   logical                  :: Finite_T_components
 end type RT_obj_lifetime
 !
 ! Lifetimes
 !-----------
 type(RT_lifetime)     :: RT_EE_life,RT_EP_life,RT_EH_life,RT_PE_life
 type(RT_obj_lifetime) :: RT_obj_EE_life,RT_obj_EP_life,RT_obj_EH_life,RT_obj_PE_life
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
 real(SP),    allocatable :: REF_lifetime(:,:,:)
 real(SP),    allocatable :: SAVED_lifetime(:,:,:,:)
 !
 ! Ouput Lifetimes, ratio fit and temperatures
 !---------------------------------------------
 logical                  :: l_lftm_fit_stable(2)=.FALSE.
 logical                  :: l_lftm_fit_temperatures=.FALSE.
 integer                  :: Nfitted_lifetimes(2)=0
 real(SP)                 :: RT_mean_lifetimes(8)=rZERO ! 1-2 (e-e), 3-6 (e-p), 7-8 (e-photon)
 real(SP)                 :: RT_Tfit_lifetimes(4)=rZERO
 real(SP)                 :: RT_Efit_lifetimes(4)=rZERO
 !
 real(SP)                 :: EE_prefactor(2)    =rZERO      ! 1=valence, 2=conduction
 real(SP)                 :: RAD_prefactor(2)   =rZERO
 real(SP)                 :: EP_prefactor(2)    =rZERO
 real(SP)                 :: EP_abs_prefactor(2)=rZERO
 !
 contains
   !
   subroutine RT_ALL_lifetimes_alloc( )
     use real_time, ONLY:RT_bands,RT_nk
     if (l_elph_scatt)     call RT_lifetime_alloc(RT_EP_life,.TRUE.,RT_bands,RT_nk)
     if (l_elel_scatt)     call RT_lifetime_alloc(RT_EE_life,.FALSE.,RT_bands,RT_nk)
     if (l_elphoton_scatt) call RT_lifetime_alloc(RT_EH_life,.FALSE.,RT_bands,RT_nk)
     if (l_elel_scatt.or.l_elph_scatt.or.l_elphoton_scatt) then
       YAMBO_ALLOC(REF_lifetime,(RT_bands(1):RT_bands(2),RT_nk,8))
       REF_lifetime=rZERO
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
     if (l_elph_scatt)     call RT_lifetime_free(RT_EP_life)
     if (l_elel_scatt)     call RT_lifetime_free(RT_EE_life)
     if (l_elphoton_scatt) call RT_lifetime_free(RT_EH_life)
     YAMBO_FREE(REF_lifetime)
     YAMBO_FREE(SAVED_lifetime)
   end subroutine
   !
   subroutine RT_lifetime_alloc(LIFE,Finite_T_components,Nb,Nk)
     type(RT_lifetime) :: LIFE
     integer :: Nb(2),Nk
     logical :: Finite_T_components
     YAMBO_ALLOC(LIFE%El_emit,(Nb(1):Nb(2),Nk))
     YAMBO_ALLOC(LIFE%Ho_abs,(Nb(1):Nb(2),Nk))
     LIFE%Finite_T_components=Finite_T_components
     LIFE%El_emit=rZERO
     LIFE%Ho_abs =rZERO
     if (.not.Finite_T_components) return
     YAMBO_ALLOC(LIFE%El_abs,(Nb(1):Nb(2),Nk))
     YAMBO_ALLOC(LIFE%Ho_emit,(Nb(1):Nb(2),Nk))
     LIFE%El_abs =rZERO
     LIFE%Ho_emit=rZERO
   end subroutine
   !
   subroutine RT_lifetime_free(LIFE)
     type(RT_lifetime) :: LIFE
     LIFE%Finite_T_components=.FALSE.
     YAMBO_FREE(LIFE%El_emit)
     YAMBO_FREE(LIFE%Ho_emit)
     YAMBO_FREE(LIFE%El_abs)
     YAMBO_FREE(LIFE%Ho_abs)
   end subroutine
   !
   subroutine RT_obj_lifetime_alloc(LIFE,Finite_T_components,Ns)
     type(RT_obj_lifetime) :: LIFE
     integer :: Ns
     logical :: Finite_T_components
     YAMBO_ALLOC(LIFE%El_emit,(Ns))
     YAMBO_ALLOC(LIFE%Ho_abs,(Ns))
     YAMBO_ALLOC(LIFE%bare,(Ns))
     LIFE%El_emit=rZERO
     LIFE%Ho_abs =rZERO
     LIFE%bare   =rZERO
     LIFE%Finite_T_components=Finite_T_components
     if (.not.Finite_T_components) return
     YAMBO_ALLOC(LIFE%El_abs,(Ns))
     YAMBO_ALLOC(LIFE%Ho_emit,(Ns))
     LIFE%El_abs =rZERO
     LIFE%Ho_emit=rZERO
   end subroutine
   !
   subroutine RT_obj_lifetime_free(LIFE)
     type(RT_obj_lifetime) :: LIFE
     LIFE%Finite_T_components=.FALSE.
     YAMBO_FREE(LIFE%El_emit)
     YAMBO_FREE(LIFE%Ho_emit)
     YAMBO_FREE(LIFE%El_abs)
     YAMBO_FREE(LIFE%Ho_abs)
     YAMBO_FREE(LIFE%bare)
   end subroutine
   !
end module

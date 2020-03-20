!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
module YPP_real_time
 !
 use pars,     ONLY:schlen,SP,DP,rZERO
 !
#include<memory.h>
 !
 ! RT
 !
 logical  :: l_RealTime
 logical  :: l_RT_DBs
 !
 ! RT DBs (what)
 !
 logical  :: l_RTpump_energy
 logical  :: l_RTpump_kspace
 logical  :: l_RTpump_Fermi
 !
 ! RT Post-processing (what)
 !
 logical  :: l_RT_occupations
 logical  :: l_RT_lifetimes
 logical  :: l_RT_density
 logical  :: l_RT_X
 logical  :: l_NL_X
 logical  :: l_NL_exc
 logical  :: l_RT_abs
 !
 ! RT post-processing (kind)
 !
 logical  :: l_RT_bands
 logical  :: l_RT_time
 logical  :: l_RT_energy
 logical  :: l_RT_dos
 !
 ! (RT) Retarded Green's functions
 !
 integer  :: Ret_GF_bands(2)      ! Bands Range
 real(SP) :: Gr_E_step
 real(SP) :: GF_T_step
 !
 ! Manual pump
 !
 real(SP)    :: Eh_pumped_Nel
 real(SP)    :: Eh_pumped_pair_energy(2)
 !
 real(SP)    :: Eh_pumped_pair_deltaE
 real(SP)    :: Eh_pumped_pair_BZ_regions(3,6)   
 !
 real(SP)    :: Eh_pumped_pair_width               ! energy with
 real(SP)    :: Eh_pumped_pair_BZ_width            ! k width
 integer     :: Eh_pumped_pair_n_BZ_regions
 !
 ! Fermi function pump
 !
 real(SP) :: Eh_mu(2)            ! chemical potential for holes, electrons
 real(SP) :: Eh_temp(2)          ! effective temperature for holes, electrons
 real(SP) :: h_mu_autotune_thr   ! threshold for matching number of pumped holes / electrons
 !
 ! (RT) optics
 !
 logical  :: Use_FFT
 logical  :: l_skip_OBS_IO
 logical  :: l_force_min_damp
 logical  :: l_RT_X_inversion
 integer  :: N_probes
 integer  :: N_pumps
 integer  :: N_probe_frequencies
 integer  :: X_order
 character(schlen) :: Probe_Keyword
 character(schlen) :: Pump_Keyword
 integer, parameter:: max_n_of_paths=500
 character(schlen) :: Pump_path
 character(schlen) :: Probe_path(max_n_of_paths)
 character(3) :: RT_p_format   
 !
 ! (RT) carriers
 !
 real(SP), allocatable     :: delta_f(:,:,:)
 real(SP), allocatable     :: integrated_Nel(:,:)
 real(SP), allocatable     :: integrated_Nhole(:,:)
 !
 ! (RT) common
 !
 real(SP),allocatable     :: RT_time(:)
 !
 type RT_ctl
   real(SP) :: delta_W             =0._SP   ! delta W
   real(SP) :: X_W_range(2)        =0._SP   ! Response energy range 
   real(SP) :: Carriers_W_range(2) =0._SP   ! Carriers energy range 
   integer  :: W_n_steps           =0       ! number of steps in energy mesh
   real(SP) :: delta_T             =0._SP   ! delta T
   real(SP) :: T_range(2)          =0._SP   ! time windows for the transform
   integer  :: T_n_steps           =0       ! number of steps in Time mesh
   real(SP) :: damp_factor         =0._SP   ! factor used in damping
   real(SP) :: Field_damp_factor   =0._SP   ! factor used deduced from the Field 
   character(schlen) :: damping    ="NONE"  ! Damp type (none, lorentian or gaussian)
 end type RT_ctl
 !
 type(RT_ctl), save :: RT_conf 
 !
 type OBS_el
   character(schlen)        :: title
   integer                  :: N
   complex(DP), allocatable :: F_t(:,:)
   complex(DP), allocatable :: F_w(:,:)
   complex(SP), allocatable :: W(:)
 end type OBS_el
 !
 type(OBS_el) :: Pol 
 type(OBS_el) :: Curr 
 type(OBS_el) :: ElField 
 !
 interface
   !
   subroutine  RT_OBSERVABLES_IO(what,IO_error,first_sec_only,G_lesser_only,COM_,mesg)
     use electrons,     ONLY:levels
     implicit none
     character,    optional :: what
     logical,      optional :: IO_error,first_sec_only,G_lesser_only
     integer,      optional :: COM_
     character(*), optional :: mesg
   end subroutine
   !
   subroutine RT_IO_carriers(Time,imposed_COM)
     use pars,       ONLY:SP
     real(SP), optional, intent(inout) :: Time
     integer , optional, intent(in)    :: imposed_COM
   end subroutine RT_IO_carriers
   !
 end interface
   !
   contains 
   !
   subroutine FT_free(FT)
     type(OBS_el) :: FT
     YAMBO_FREE(FT%F_w)
     YAMBO_FREE(FT%F_t)
     YAMBO_FREE(FT%W)
   end subroutine
   !
end module YPP_real_time

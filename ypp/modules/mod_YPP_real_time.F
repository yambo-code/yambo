!
!        Copyright (C) 2000-2018 the YAMBO team
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
 use pars,     ONLY:schlen,SP,DP
 !
 implicit none
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
 logical  :: l_RTpump_manual
 !
 ! RT Post-processing (what)
 !
 logical  :: l_RT_occupations
 logical  :: l_RT_lifetimes
 logical  :: l_RT_density
 logical  :: l_RT_X
 logical  :: l_NL_X
 logical  :: l_RT_abs
 !
 ! RT post-processing (kind)
 !
 logical  :: l_RT_bands
 logical  :: l_RT_time
 logical  :: l_RT_energy
 logical  :: l_RT_2D_dos
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
 real(SP)    :: Eh_pumped_pair_BZ_reagions(3,6)   
 !
 real(SP)    :: Eh_pumped_pair_width               ! energy with
 real(SP)    :: Eh_pumped_pair_BZ_width            ! k width
 integer     :: n_regions
 !
 ! (RT) optics
 !
 logical  :: Use_FFT
 logical  :: l_skip_pol_and_curr_IO
 logical  :: l_force_min_damp
 logical  :: l_RT_X_inversion
 integer  :: N_probes
 integer  :: N_pumps
 integer  :: N_probe_frequencies
 integer  :: X_order
 character(schlen) :: Probe_Keyword
 character(schlen) :: Pump_Keyword
 integer, parameter:: max_n_of_paths=10000
 character(schlen) :: Pump_path
 character(schlen) :: Probe_path(max_n_of_paths)
 !
 complex(DP),    allocatable :: RT_J_t(:,:)
 complex(DP),    allocatable :: RT_P_t(:,:)
 !
 character(3) :: RT_p_format   
 !
 ! (RT) carriers
 !
 real(SP),allocatable     :: delta_f(:,:,:)
 !
 ! (RT) common
 !
 real(SP),allocatable     :: RT_time(:)
 !
 type RT_ctl
   real(SP) :: delta_T           =0._SP   ! delta T
   real(SP) :: T_range(2)        =0._SP   ! time windows for the transform
   integer  :: T_n_steps         =0       ! number of steps in Time mesh
   real(SP) :: W_range(2)        =0._SP   ! energy range 
   integer  :: W_n_steps         =0       ! number of steps in energy mesh
   real(SP) :: damp_factor       =0._SP   ! factor used in damping
   real(SP) :: Field_damp_factor =0._SP   ! factor used deduced from the Field 
   character(schlen) :: damping  ="NONE"  ! Damp type (none, lorentian or gaussian)
 end type RT_ctl
 !
 type(RT_ctl), save :: RT_conf 
 !
 interface
   !
   subroutine  RT_IO_J_and_P(IO_error,first_sec_only,G_lesser_only,COM_,mesg)
     use electrons,     ONLY:levels
     implicit none
     logical, optional  :: IO_error,first_sec_only,G_lesser_only
     integer, optional  :: COM_
     character(*), optional  :: mesg
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
end module YPP_real_time

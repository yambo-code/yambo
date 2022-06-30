!
!        Copyright (C) 2000-2022 the YAMBO team
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
 use units,    ONLY:HA2EV
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
 logical  :: l_RTpump_Fermi
 !
 ! RT Post-processing (what)
 !
 logical  :: l_RT_occupations
 logical  :: l_RT_lifetimes
 logical  :: l_RT_density
 logical  :: l_RT_G_two_times
 logical  :: l_RT_X
 logical  :: l_NL_X
 logical  :: l_NL_exc
 logical  :: l_RT_abs
 logical  :: l_RT_pol
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
 ! Manual pump ...
 !
 real(SP)    :: Eh_pumped_Nel
 !
 ! ... Energy
 !
 real(SP)    :: Eh_pumped_pair_energy(2)
 !
 ! ... K-space
 !
 real(SP)    :: Eh_pumped_pair_deltaE
 real(SP)    :: Eh_pumped_pair_BZ_width         
 integer     :: Eh_pumped_pair_n_BZ_regions
 real(SP), allocatable :: Eh_pumped_pair_BZ_regions(:,:)   
 !
 ! ... Energy and K-space
 !
 real(SP)    :: Eh_pumped_pair_width               ! energy width
 !
 ! ... Fermi function 
 !
 real(SP) :: Eh_mu(2)            ! chemical potential for holes, electrons
 real(SP) :: Eh_temp(2)          ! effective temperature for holes, electrons
 real(SP) :: h_mu_autotune_thr   ! threshold for matching number of pumped holes / electrons
 !
 ! (RT) X
 !
 logical  :: Use_FFT
 logical  :: l_skip_OBS_IO
 logical  :: l_force_min_damp
 logical  :: l_RT_X_inversion
 integer  :: N_probes
 integer  :: N_pumps
 integer  :: N_probe_frequencies
 integer  :: X_order
 character(schlen) :: X_kind
 character(schlen) :: Probe_Keyword
 character(schlen) :: Pump_Keyword
 integer, parameter:: max_n_of_paths=500
 character(schlen) :: Pump_path
 character(schlen) :: Probe_path(max_n_of_paths)
 character(3) :: RT_p_format   
 !
 ! (RT) TRabs
 !
 real(SP)          :: TRabsDIP_dir(3)=0.
 real(SP)          :: TRabs_Eeh_treshold=1.E-5_SP/HA2EV
 character(schlen) :: TRabsWHAT="abs"
 character(schlen) :: TRabsDIP_plane="none"
 character(schlen) :: TRabsMODE="cv"
 logical           :: TRabs_lEPS(3,3)=.FALSE.
 real(SP),    allocatable :: TRabs_E(:)
 complex(SP), allocatable :: TRabs_RES_left(:,:,:)  ! I_trans, DIP_dir, 1 ( RES_0 ) - 2 ( dRES)
 complex(SP), allocatable :: TRabs_RES_right(:,:,:)
 complex(SP), allocatable :: Epsilon_ij(:,:,:)
 complex(SP), allocatable :: B_Hall(:,:)
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
 ! (RT) G<(t,t')
 !
 real(SP)                 :: Rho_deph
 !
 type RT_ctl
   real(SP) :: delta_W             =0._SP   ! delta W
   real(SP) :: X_W_range(2)        =0._SP   ! Response energy range 
   real(SP) :: Carriers_W_range(2) =0._SP   ! Carriers energy range 
   integer  :: W_n_steps           =0       ! number of steps in energy mesh
   real(SP) :: delta_T             =0._SP   ! delta T
   real(SP) :: T_range(2)          =0._SP   ! time windows for the transform
   integer  :: T_n_range(2)        =0       ! lower and upper value of time range
   integer  :: T_n_steps           =0       ! number of steps in Time mesh
   real(SP) :: damp_factor         =0._SP   ! factor used in damping
   real(SP) :: Field_damp_factor   =0._SP   ! factor used deduced from the Field 
   character(schlen) :: damping    ="NONE"  ! Damp type (none, lorentian or gaussian)
 end type RT_ctl
 !
 type(RT_ctl), save :: RT_conf,RT_conf_save
 !
 type OBS_el
   character(schlen)        :: title
   integer                  :: N
   complex(DP), allocatable :: F_t(:,:)
   complex(DP), allocatable :: F_w(:,:)
   complex(SP), allocatable :: W(:)
 end type OBS_el
 !
 type(OBS_el), allocatable :: Pol(:)
 type(OBS_el), allocatable :: Curr(:)
 type(OBS_el) :: ElField
 type(OBS_el) :: GreenF_T_and_W
 !
 interface
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
   subroutine RT_conf_duplicate(Cin,Cout)
     type(RT_ctl) :: Cin,Cout
     Cout%delta_W=Cin%delta_W
     Cout%X_W_range=Cin%X_W_range
     Cout%Carriers_W_range=Cin%Carriers_W_range 
     Cout%W_n_steps=Cin%W_n_steps   
     Cout%delta_T=Cin%delta_T   
     Cout%T_range=Cin%T_range 
     Cout%T_n_range=Cin%T_n_range 
     Cout%T_n_steps=Cin%T_n_steps   
     Cout%damp_factor=Cin%damp_factor   
     Cout%Field_damp_factor=Cin%Field_damp_factor   
     Cout%damping=Cin%damping   
   end subroutine
   !
   subroutine OBS_el_free(FT)
     type(OBS_el) :: FT
     YAMBO_FREE(FT%F_w)
     YAMBO_FREE(FT%F_t)
     YAMBO_FREE(FT%W)
   end subroutine
   !
   function TRabs_EPS_label(i,j)
     character(2) :: TRabs_EPS_label
     integer      :: i,j
     if (i==1) TRabs_EPS_label="x" 
     if (i==2) TRabs_EPS_label="y" 
     if (i==3) TRabs_EPS_label="z" 
     if (j==1) TRabs_EPS_label=TRabs_EPS_label(1:1)//"x" 
     if (j==2) TRabs_EPS_label=TRabs_EPS_label(1:1)//"y" 
     if (j==3) TRabs_EPS_label=TRabs_EPS_label(1:1)//"z" 
   end function
   !
   subroutine gimme_a_G_Time_Point(i_T)
     use real_time,  ONLY:G_MEM_steps
     use RT_control, ONLY:Gless_RT_IO_t
     integer            ::i_T,i_Tp,Nt
     real(SP)           ::TIME_diff,REF_diff
     !
     Nt=size(Gless_RT_IO_t%Time)
     !
     REF_diff=Gless_RT_IO_t%Time(Nt)-Gless_RT_IO_t%Time(Nt-G_MEM_steps)
     !
     do i_Tp=1,Nt
       TIME_diff= abs ( Gless_RT_IO_t%Time(i_Tp) - RT_time(i_T) )
       if (abs(TIME_diff)<REF_diff ) then 
         REF_diff=TIME_diff
         Gless_RT_IO_t%N=i_Tp
       endif
     enddo
   end subroutine
   !
end module YPP_real_time

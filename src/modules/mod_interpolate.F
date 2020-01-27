!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
module interpolate
 !
 use pars,       ONLY:SP,DP,schlen
 use R_lattice,  ONLY:bz_samp
 !
 ! Interpolation according to PRB 38 p2721
 ! Code copied from BolzTraP 
 !
 implicit none
 !
 integer               :: nshells
 real(SP), allocatable :: int_sop(:,:,:)
 real(SP), pointer     :: lattice_vectors(:,:)
 real(SP) :: INTERP_shell_factor
 real(SP) :: metric(3,3)
 !
 character(schlen) :: INTERP_mode = "NN" ! NN/BOLTZ
 character(schlen) :: INTERPOLATION_name = " "
 logical           :: INTERPOLATE_initialized=.FALSE.
 logical           :: INTERPOLATE_is_quiet=.FALSE.
 logical           :: INTERPOLATE_is_serial=.FALSE.
 character(schlen) :: INTERPOLATE_msg_fmt="rs"
 !
 ! NN method
 !
 integer :: NN_nk_exact=0
 integer :: NN_n_of_nearest=1
 integer :: NN_MAX_real_nearest=0
 !
 ! What
 !
 logical :: l_INTERP_f
 logical :: l_INTERP_E
 logical :: l_INTERP_dE
 logical :: l_INTERP_Z
 logical :: l_INTERP_W
 logical :: l_integral_respect=.TRUE. ! In the case of f/df keep the same integral of the SEED and of the interpolated
 !
 ! ID's
 !
 integer :: INTERP_E_id =0
 integer :: INTERP_dE_id=0
 integer :: INTERP_f_id=0
 integer :: INTERP_RT_id=0
 !
 ! K-grids
 !
 type(bz_samp):: GRID_k
 !
 ! Interpolation type
 !
 character(schlen), parameter :: REAL1D   = "REAL1D"
 character(schlen), parameter :: REAL2D   = "REAL2D"
 character(schlen), parameter :: CMPLX1D  = "CMPLX1D"
 character(schlen), parameter :: CMPLX2D  = "CMPLX2D"
 ! 
 type INTERP_coeff
   character(schlen)        :: interp_type = " "
   complex(DP), allocatable :: engre(:,:)
   integer                  :: ndim = 0
 end type INTERP_coeff
 ! 
 type INTERP_element
   integer                  :: nb(2)=0
   integer                  :: nk=0
   integer                  :: D=0  ! 3rd dimension of SEED
   integer                  :: ID=0
   logical                  :: in_use=.FALSE.
   logical                  :: use_NN=.FALSE.
   logical                  :: has_fineGd=.FALSE.
   character(schlen)        :: what=" "
   !
   real(SP),    allocatable :: SEED(:,:,:)
   real(SP),    allocatable :: k(:,:)
   real(SP),    allocatable :: weights(:)
   !
   real(SP),    allocatable :: INTERP(:,:,:)
   real(SP),    allocatable :: INTERP_FineGd(:,:,:)
 end type INTERP_element
 !
 integer, parameter :: max_interpls=10
 !
 type(INTERP_coeff)   :: interpls(max_interpls)
 type(INTERP_element) :: INTERP_obj(max_interpls)
 !
 interface
   !
   subroutine INTERPOLATION_nearest_k(K_in,V_in,K_out,V_out)
     use pars,           ONLY:SP
     real(SP)                :: K_in(:,:)
     real(SP)                :: K_out(:,:)
     real(SP)                :: V_in(:,:,:)
     real(SP)                :: V_out(:,:,:)
   end subroutine
   !
   subroutine INTERPOLATION_coefficients(R1D,R2D,C1D,C2D,E,k,Nk,ID,ID_obj)
     use pars,       ONLY:SP
     use R_lattice,  ONLY:bz_samp
     use electrons,  ONLY:levels
     implicit none
     type(bz_samp), intent(in)           :: k
     real(SP),      intent(in), optional :: R1D(:,:),R2D(:,:,:)
     complex(SP),   intent(in), optional :: C1D(:,:),C2D(:,:,:)
     type(levels),  intent(in), optional :: E
     integer,       intent(in), optional :: Nk
     integer,       intent(in)           :: ID_obj
     integer,       intent(out)          :: ID
   end subroutine  INTERPOLATION_coefficients
   !
   subroutine INTERPOLATION_BZ(K,NK,ID,R1D,R2D,C1D,C2D,E)
     use pars,       ONLY:SP
     use R_lattice,  ONLY:bz_samp
     use electrons,  ONLY:levels
     implicit none
     type(bz_samp), intent(in)            :: K
     integer,       intent(in)            :: ID,NK
     real(SP),      intent(out), optional :: R1D(:,:),R2D(:,:,:)
     complex(SP),   intent(out), optional :: C1D(:,:),C2D(:,:,:)
     type(levels),  intent(out), optional :: E
   end subroutine INTERPOLATION_BZ
   !
   subroutine electrons_bands_interpolate(Xen,Xk,BANDS_range,ID_bands,CIRCUIT_E,GRID_E,CIRCUIT_dE)
     use pars,       ONLY:SP
     use electrons,  ONLY:levels
     use R_lattice,  ONLY:bz_samp
     implicit none
     type(bz_samp), intent(in)  :: Xk
     type(levels),  intent(in)  :: Xen
     integer     ,  intent(in)  :: BANDS_range(2)
     integer     ,  intent(out) :: ID_bands(2)
     type(levels),  intent(out) :: CIRCUIT_E
     type(levels),  intent(out), optional  :: GRID_E
     type(levels),  intent(out), optional  :: CIRCUIT_dE
   end subroutine electrons_bands_interpolate
   !
   subroutine INTERPOLATION_driver_seed(operation,ID,dims,SEED_k,SEED_E,SEED_RT,SEED_QP,SEED_R3,&
&                                       NEAREST,QUIET,SERIAL)
     use pars,        ONLY:SP
     use real_time,   ONLY:RT_t
     use R_lattice,   ONLY:bz_samp
     use electrons,   ONLY:levels
     use QP_m,        ONLY:QP_t
     implicit none
     character(*),  intent(in)              :: operation
     integer,       intent(out)             :: ID
     integer,       intent(in)              :: dims(2)
     type(bz_samp), intent(in)              :: SEED_k
     real(SP),      intent(in),    optional :: SEED_R3(:,:,:)
     type(RT_t),    intent(in),    optional :: SEED_RT
     type(QP_t),    intent(in),    optional :: SEED_QP
     type(levels),  intent(in),    optional :: SEED_E
     logical,       intent(in),    optional :: NEAREST
     logical,       intent(in),    optional :: QUIET
     logical,       intent(in),    optional :: SERIAL
   end subroutine INTERPOLATION_driver_seed
   !
   subroutine INTERPOLATION_driver_do(operation,ID,OUT_k,OUT_E)
     use pars,        ONLY:SP
     use real_time,   ONLY:RT_t
     use R_lattice,   ONLY:bz_samp
     use electrons,   ONLY:levels
     use QP_m,        ONLY:QP_t
     implicit none
     character(*),  intent(in)              :: operation
     integer,       intent(in)              :: ID
     type(bz_samp), intent(inout)           :: OUT_k
     type(levels),  intent(inout), optional :: OUT_E
   end subroutine INTERPOLATION_driver_do
   !
 end interface  
 !
end module interpolate

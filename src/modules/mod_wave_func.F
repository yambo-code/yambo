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
module wave_func
 !
 use pars,           ONLY:SP
#if defined _CUDA
 use cudafor
#endif
#include <memory.h>
 !
 type WAVEs 
   integer              :: b(2)      ! band range 
   integer              :: k(2)      ! k range
   integer              :: sp_pol(2) ! sp_pol_range
   integer              :: N         ! states 
   character(1)         :: space
   complex(SP), allocatable :: c(:,:,:)
   integer    , allocatable :: index(:,:,:)
   logical    , allocatable :: state(:,:,:)
   logical                  :: to_load = .TRUE.
#if defined _CUDA
   complex(SP), allocatable, device :: c_d(:,:,:)
#endif
 end type WAVEs
 !
 ! Buffered WFs
 !
 type (WAVEs), save :: WF_buffer
 logical            :: WF_buffered_IO
 !
 ! Actual WFs
 !
 type (WAVEs), save, target :: WF
 !
 ! Real components ?
 !
 logical                :: real_wavefunctions
 !
 ! Bands block size
 !
 integer                :: wf_nb_io
 integer                :: wf_nb_io_groups
 !
 integer                :: wf_nb_to_load
 integer, allocatable   :: wf_b_indx(:)            ! given ib_to_load=1,wf_nb_to_load returns the band index
 !
 ! Max Num. of COMPONENTS
 !
 integer                :: wf_ncx
 !
 ! Max Num. of G-VECTORS (>= wf_ncx)
 !
 integer                :: wf_ng
 !
 ! Wave function derivatives
 !
 complex(SP), allocatable :: wf_x(:,:,:)
 complex(SP), allocatable :: wf_y(:,:,:)
 complex(SP), allocatable :: wf_z(:,:,:)
 !
 ! Parallel wave-function distribution
 !
 logical    ,allocatable:: states_to_load(:,:,:) 
 !
 ! Num. of COMPONENTS at each k
 !
 integer    ,allocatable:: wf_nc_k(:)
 !
 ! Table correspondance G-vec <-> Components: G_ic = wf_igk(ic,ik)
 !
 integer    ,allocatable:: wf_igk(:,:)
#ifdef _CUDA
 integer    ,allocatable, device :: wf_igk_d(:,:)
#endif
 logical                :: wf_norm_test
 !
 ! Allocation/deallocation messaging verbosity
 !
 logical                :: QUIET_free
 logical                :: QUIET_alloc
 !
 interface
   !
   integer function io_WF_old(ID,wf_old)
     use pars,       ONLY: SP
     integer            :: ID
     real(SP), optional :: wf_old(:,:,:,:)
   end function
   !
   integer function io_WF(ID,wf,wf_d)
     use pars,       ONLY: SP
     integer            :: ID
     complex(SP), optional :: wf(:,:,:)
     complex(SP), optional :: wf_d(:,:,:)
#ifdef _CUDA
     attributes(device) :: wf_d
#endif
   end function
   !
   function WF_symm(ifft,isc)
     use pars,        ONLY: SP
     use electrons,   ONLY: n_spinor
     integer             :: ifft,isc(4)
     complex(SP),dimension(n_spinor) :: WF_symm
   end function
   !
#if defined _CUDA
   function WF_symm_gpu(ifft,isc)
     use pars,        ONLY: SP
     use electrons,   ONLY: n_spinor
     integer             :: ifft,isc(4)
     complex(SP),dimension(n_spinor), device :: WF_symm_gpu
   end function
#endif
   !
 end interface 
 !
 contains
   !
   subroutine WF_copy(WF_in,WF_out)
     use electrons,  ONLY: n_spinor
     use FFT_m,      ONLY: fft_size
     type(WAVEs):: WF_in,WF_out
     YAMBO_ALLOC(WF_out%c,(size(WF_in%c(:,1,1)),n_spinor,WF_in%N))
     YAMBO_ALLOC(WF_out%index,(WF_in%b(1):WF_in%b(2),WF_in%k(1):WF_in%k(2),WF_in%sp_pol(1):WF_in%sp_pol(2)))
     WF_out%b      = WF_in%b
     WF_out%k      = WF_in%k
     WF_out%sp_pol = WF_in%sp_pol
     WF_out%N      = WF_in%N
     WF_out%space  = WF_in%space
     WF_out%to_load= WF_in%to_load
     WF_out%c      = WF_in%c
     WF_out%index  = WF_in%index
#if defined _CUDA
     if (allocated(WF_in%c_d)) allocate(WF_out%c_d,source=WF_in%c)
#endif
   end subroutine
   !
   subroutine WF_derivative_alloc(cart_direction)
     use pars,     ONLY : cZERO
     use FFT_m,    ONLY : fft_size
     use electrons,ONLY : n_spinor
     logical  :: cart_direction(3)
     !
     if(cart_direction(1)) then
       YAMBO_ALLOC(wf_x,(fft_size,n_spinor,WF%N))
       wf_x=cZERO
     endif
     if(cart_direction(2)) then
       YAMBO_ALLOC(wf_y,(fft_size,n_spinor,WF%N))
       wf_y=cZERO
     endif
     if(cart_direction(3)) then
       YAMBO_ALLOC(wf_z,(fft_size,n_spinor,WF%N))
       wf_z=cZERO
     endif
   end subroutine
   !
   subroutine WF_derivative_free()
     !
     YAMBO_FREE(wf_x)
     YAMBO_FREE(wf_y)
     YAMBO_FREE(wf_z)
     !
   end subroutine
   !
end module

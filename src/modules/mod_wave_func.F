!
!        Copyright (C) 2000-2017 the YAMBO team
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
 implicit none
 !
 type WAVEs 
   integer              :: b(2) ! band range 
   integer              :: k(2) ! k range
   integer              :: spin(2)
   integer              :: N ! states 
   character(1)         :: space
   complex(SP), pointer :: c(:,:)        => null()
   integer    , pointer :: index(:,:,:)  => null()
   logical    , pointer :: state(:,:,:)  => null()
   logical              :: to_load = .TRUE.
 end type WAVEs
 !
 ! Buffered WFs
 !
 type (WAVEs), save :: WF_buffer
 logical            :: WF_buffered_IO
 !
 ! Actual WFs
 !
 type (WAVEs), save :: WF
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
 complex(SP), allocatable :: wf_x(:,:)
 complex(SP), allocatable :: wf_y(:,:)
 complex(SP), allocatable :: wf_z(:,:)
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
 logical                :: wf_norm_test
 !
 ! Allocation/deallocation messaging verbosity
 !
 logical                :: QUIET_free
 logical                :: QUIET_alloc
 !
 interface
   !
   integer function io_WF(ID,wf)
     use pars,       ONLY: SP
     integer            :: ID
     real(SP), optional :: wf(:,:,:,:)
   end function
   !
   function WF_symm(ifft,isc)
     use pars,        ONLY: SP
     use electrons,   ONLY: n_spinor
     integer             :: ifft,isc(4)
     complex(SP),dimension(n_spinor) :: WF_symm
   end function
   !
 end interface 
 !
 contains
   !
   subroutine WF_copy(WF_in,WF_out)
     use FFT_m, ONLY:fft_size
     type(WAVEs):: WF_in,WF_out
     WF_out%b=WF_in%b
     WF_out%k=WF_in%k
     WF_out%spin=WF_in%spin
     WF_out%N=WF_in%N
     WF_out%space=WF_in%space
     WF_out%to_load=WF_in%to_load
     if (WF_out%space=="R") allocate(WF_out%c(fft_size,WF_out%N))
     if (WF_out%space=="G") allocate(WF_out%c(wf_ng,WF_out%N))
     if (WF_out%space=="C".or.WF_out%space=="B") allocate(WF_out%c(wf_ncx,WF_out%N))
     allocate(WF_out%index(WF_out%b(2),WF_out%k(2),WF_out%spin(2)))
     WF_out%c=WF_in%c
     WF_out%index=WF_in%index
   end subroutine
   !
   subroutine WF_derivative_alloc(cart_direction)
     use memory_m, ONLY : mem_est
     use FFT_m,    ONLY : fft_size
     logical  :: cart_direction(3)
     !
     if(cart_direction(1)) then
       allocate(wf_x(fft_size,WF%N))
       call mem_est("WF_x",(/fft_size*WF%N/),quiet=QUIET_alloc)
       wf_x=(0.,0.)
     endif
     if(cart_direction(2)) then
       allocate(wf_y(fft_size,WF%N))
       call mem_est("WF_y",(/fft_size*WF%N/),quiet=QUIET_alloc)
       wf_y=(0.,0.)
     endif
     if(cart_direction(3)) then
       allocate(wf_z(fft_size,WF%N))
       call mem_est("WF_z",(/fft_size*WF%N/),quiet=QUIET_alloc)
       wf_z=(0.,0.)
     endif
   end subroutine
   !
   subroutine WF_derivative_free()
     use memory_m, ONLY : mem_est
       !
       if(allocated(wf_x)) deallocate(wf_x)
       if(allocated(wf_y)) deallocate(wf_y)
       if(allocated(wf_z)) deallocate(wf_z)
       call mem_est("WF_x WF_y WF_z",quiet=QUIET_free)
       !
   end subroutine
   !
end module

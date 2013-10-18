!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
 end type WAVEs
 !
 ! Buffered WFs
 !
 type (WAVEs) :: WF_buffer
 !
 ! Actual WFs
 !
 type (WAVEs) :: WF
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
   integer function ioWF(ID,wf)
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
   subroutine WF_load(iG_max,iGo_max,bands_to_load,kpts_to_load,&
&                     spins_to_load,space,title,impose_free_and_alloc,force_WFo)
     integer :: iG_max,iGo_max,bands_to_load(2),kpts_to_load(2)
     integer,     optional :: spins_to_load(2)
     character(*),optional :: space
     character(*),optional :: title
     logical     ,optional :: impose_free_and_alloc
     logical     ,optional :: force_WFo
   end subroutine
   !
 end interface 
 !
 contains
   !
   subroutine WF_alloc()
     use memory_m, ONLY : mem_est
     use FFT_m,    ONLY : fft_size,fftw_plan,fft_dim
     !
     ! Work Space
     !
     integer :: err
     integer :: ik,ib,is
     !
     ! Distributed allocation
     !
     if(allocated(states_to_load)) then
       !
       WF%N=0
       do ib=WF%b(1),WF%b(2)
         do ik=WF%k(1),WF%k(2)
           do is=WF%spin(1),WF%spin(2)
             if(states_to_load(ib,ik,is)) WF%N=WF%N+1
           enddo
         enddo
       enddo
       !
     else
       !
       WF%N=(WF%b(2)-WF%b(1)+1)*(WF%k(2)-WF%k(1)+1)*(WF%spin(2)-WF%spin(1)+1)
       !
     endif
     !
     if (WF%space=="R") allocate(WF%c(fft_size,WF%N),stat=err)
     if (WF%space=="G") allocate(WF%c(wf_ng,WF%N),stat=err)
     if (WF%space=="C") allocate(WF%c(wf_ncx,WF%N),stat=err)
     !
     if (err==0) allocate(WF%index(WF%b(2),WF%k(2),WF%spin(2)),stat=err)
     !
     call mem_est('WF',(/size(WF%c)/),errors=(/err/))
     !
     WF%c=(0.,0.)
     WF%index=0
#if defined _FFTW
     fftw_plan=0
#endif
   end subroutine
   !
   subroutine WF_free()
     use memory_m, ONLY : mem_est
     use FFT_m,    ONLY : fft_size,fft_dim,fft_dim_loaded,fft_g_table,&
  &                       fft_rot_r,fft_rot_r_inv,fftw_plan,fft_multiplier
     if (.not.associated(WF%c)) return
     !
     call mem_est('WF',quiet=QUIET_free)
     !
     deallocate(WF%c,WF%state)
     if (WF%space=="R") then
       fft_size=0
       fft_dim=0
       fft_dim_loaded=0
       fft_multiplier=1
       deallocate(fft_g_table,fft_rot_r,fft_rot_r_inv)
       call mem_est('FFT_g_tab FFT_rot')
#if defined _FFTW
       call dfftw_destroy_plan(fftw_plan)
       fftw_plan=0
#endif
     endif
     WF%k=0
     WF%b=0
     WF%spin=0
     WF%space=' '
   end subroutine
   !
   subroutine WF_derivative_alloc(cart_direction)
     use memory_m, ONLY : mem_est
     use FFT_m,    ONLY : fft_size,fftw_plan,fft_dim
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

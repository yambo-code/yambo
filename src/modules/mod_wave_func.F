!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
 integer                :: wf_b(2)
 integer                :: wf_k(2)
 integer                :: wf_s(2)
 integer                :: wf_n_states
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
 character(1)           :: wf_space
 complex(SP),allocatable:: wf(:,:)
 !
 ! Wave function derivatives
 !
 complex(SP), allocatable :: wf_x(:,:)
 complex(SP), allocatable :: wf_y(:,:)
 complex(SP), allocatable :: wf_z(:,:)
 !
 integer    ,allocatable:: wf_state(:,:,:)
 !
#if defined _DISTRIBUTED
 !
 ! Parallel wave-function distribution
 !
 logical    ,allocatable:: states_loaded(:,:,:) 
 logical    ,allocatable:: states_to_load(:,:,:) 
#endif
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
   subroutine WF_load(iG_max,iGo_max,bands_to_load,kpts_to_load,&
&                     spins_to_load,space,title,impose_free_and_alloc)
     integer :: iG_max,iGo_max,bands_to_load(2),&
&               kpts_to_load(2)
     integer,     optional :: spins_to_load(2)
     character(*),optional :: space,title
     logical     ,optional :: impose_free_and_alloc
   end subroutine
   !
 end interface 
 !
 contains
   !
   subroutine WF_alloc()
     use memory_m, ONLY : mem_est
     use FFT_m,    ONLY : fft_size,fftw_plan,fft_dim
     integer :: err
#if defined _DISTRIBUTED
     integer :: ik,ib,is
#endif 
     !
#if defined _DISTRIBUTED
     !
     ! Distributed allocation
     !
     if(allocated(states_to_load)) then
       !
       wf_n_states=0
       do ib=wf_b(1),wf_b(2)
         do ik=wf_k(1),wf_k(2)
           do is=wf_s(1),wf_s(2)
             if(states_to_load(ib,ik,is)) wf_n_states=wf_n_states+1
           enddo
         enddo
       enddo
       !
     else
#endif
       !
       wf_n_states=(wf_b(2)-wf_b(1)+1)*(wf_k(2)-wf_k(1)+1)*(wf_s(2)-wf_s(1)+1)
       !
#if defined _DISTRIBUTED
     endif
#endif
     !
     if (wf_space=="R") allocate(wf(fft_size,wf_n_states),stat=err)
     if (wf_space=="G") allocate(wf(wf_ng,wf_n_states),stat=err)
     if (wf_space=="C") allocate(wf(wf_ncx,wf_n_states),stat=err)
     !
     if (err==0) allocate(wf_state(wf_b(2),wf_k(2),wf_s(2)),stat=err)
     !
     call mem_est('WF',(/size(wf)/),errors=(/err/))
     !
     wf=(0.,0.)
     wf_state=0
#if defined _FFTW
     fftw_plan=0
#endif
   end subroutine
   !
   subroutine WF_free()
     use memory_m, ONLY : mem_est
     use FFT_m,    ONLY : fft_size,fft_dim,fft_dim_loaded,fft_g_table,&
  &                       fft_rot_r,fft_rot_r_notr,fftw_plan,fft_multiplier
     if (.not.allocated(wf)) return
     !
     call mem_est('WF',quiet=QUIET_free)
     !
     deallocate(wf,wf_state)
     if (wf_space=="R") then
       fft_size=0
       fft_dim=0
       fft_dim_loaded=0
       fft_multiplier=1
       deallocate(fft_g_table,fft_rot_r,fft_rot_r_notr)
       call mem_est('FFT_g_tab FFT_rot FFT_rot_notr')
#if defined _FFTW
       call dfftw_destroy_plan(fftw_plan)
       fftw_plan=0
#endif
     endif
     wf_k=0
     wf_b=0
     wf_s=0
     wf_space=' '
   end subroutine
   !
   subroutine WF_derivative_alloc(cart_direction)
     use memory_m, ONLY : mem_est
     use FFT_m,    ONLY : fft_size,fftw_plan,fft_dim
     logical  :: cart_direction(3)
     !
     if(cart_direction(1)) then
       allocate(wf_x(fft_size,wf_n_states))
       call mem_est("WF_x",(/fft_size*wf_n_states/),quiet=QUIET_alloc)
       wf_x=(0.,0.)
     endif
     if(cart_direction(2)) then
       allocate(wf_y(fft_size,wf_n_states))
       call mem_est("WF_y",(/fft_size*wf_n_states/),quiet=QUIET_alloc)
       wf_y=(0.,0.)
     endif
     if(cart_direction(3)) then
       allocate(wf_z(fft_size,wf_n_states))
       call mem_est("WF_z",(/fft_size*wf_n_states/),quiet=QUIET_alloc)
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

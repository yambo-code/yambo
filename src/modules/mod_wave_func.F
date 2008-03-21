!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
 integer    ,allocatable:: wf_state(:,:,:)
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
 interface
   !
   integer function ioWF(ID,wf)
     use pars,       ONLY: SP
     integer            :: ID
     real(SP), optional :: wf(:,:,:,:)
   end function 
   !
   subroutine wf_load(iG_max,iGo_max,bands_to_load,kpts_to_load,&
&                     spins_to_load,space,title)
     integer :: iG_max,iGo_max,bands_to_load(2),&
&               kpts_to_load(2)
     integer,     optional :: spins_to_load(2)
     character(*),optional :: space,title
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
     !
     wf_n_states=(wf_b(2)-wf_b(1)+1)*(wf_k(2)-wf_k(1)+1)*(wf_s(2)-wf_s(1)+1)
     !
     if (wf_space=="R") allocate(wf(fft_size,wf_n_states),stat=err)
     if (wf_space=="G") allocate(wf(wf_ng,wf_n_states),stat=err)
     !
     if (err==0) allocate(wf_state(wf_b(2),wf_k(2),wf_s(2)),stat=err)
     !
#if defined _BIGSYS
     call mem_est('WF',(/size(wf)/),quiet=.true.,errors=(/err/))
#else
     call mem_est('WF',(/size(wf)/),errors=(/err/))
#endif
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
  &                       fft_rot_r,fftw_plan
     if (.not.allocated(wf)) return
#if defined _BIGSYS
     call mem_est('WF',quiet=.true.)
#else
     call mem_est('WF')
#endif
     deallocate(wf,wf_state)
     if (wf_space=="R") then
       fft_size=0
       fft_dim=0
       fft_dim_loaded=0
       deallocate(fft_g_table,fft_rot_r)
       call mem_est('FFT_g_tab FFT_rot')
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
end module

!
! Copyright (C) 2000-2008 D. Sangalli and the YAMBO team 
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
subroutine WF_derivative(load,n_bands,der_k,do_der)
 !
 use pars,           ONLY:SP,DP,pi
 use magnetic,       ONLY:wf_x,wf_y,wf_z
 use wave_func,      ONLY:wf,wf_state,wf_load,WF_free,wf_ng,wf_b
 use electrons,      ONLY:n_sp_pol
 use FFT_m,          ONLY:fft_size,fft_dim,fft_g_table,fft_norm,fftw_plan
 use R_lattice,      ONLY:g_vec
 use stderr,         ONLY:intc
 use timing,         ONLY:live_timing
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:k_pt
 use D_lattice,      ONLY:alat
 !
 ! Input variables
 integer :: n_bands(2)
 integer :: der_k(2)
 logical :: do_der(3)
 logical :: load
 !
 ! Internal variables
 integer :: n_states
 complex(SP), allocatable  :: wf_der(:)
 complex(DP), allocatable  :: WF_DP(:)
 ! Dummys
 integer :: i1,i_spin,ib,ik,ifft
 !
 if(.not.load) then
   if(allocated(wf_x)) deallocate(wf_x)
   if(allocated(wf_y)) deallocate(wf_y)
   if(allocated(wf_z)) deallocate(wf_z)
   call mem_est("wf_x wf_y wf_z")
   return
 endif
 !
 n_states=n_sp_pol*(n_bands(2)-n_bands(1)+1)*(der_k(2)-der_k(1)+1)
 !
 ! Wavefunctions 
 !===============
 ! To construct the magnetic part of the Hamiltonian
 ! I need the wave functions in G space.
 call fft_setup(0,1,.false.)
 call wf_load(0,1,(/n_bands(1),n_bands(2)/),(/der_k(1),der_k(2)/),space='G',title='-MAG')
 !
 allocate(wf_der(wf_ng))                              ! G space
 call mem_est("wf_der",(/wf_ng/))
 allocate(wf_DP(fft_size))                            ! real space
 call mem_est("wf_DP",(/fft_size/),(/DP/))
 if(do_der(1)) then
   allocate(wf_x(fft_size,n_states))
   call mem_est("wf_x",(/fft_size*n_states/))
   wf_x=(0.,0.)
 endif
 if(do_der(2)) then
   allocate(wf_y(fft_size,n_states))
   call mem_est("wf_y",(/fft_size*n_states/))
   wf_y=(0.,0.)
 endif
 if(do_der(3)) then
   allocate(wf_z(fft_size,n_states))
   call mem_est("wf_z",(/fft_size*n_states/))
   wf_z=(0.,0.)
 endif
 !
 call live_timing('WF derivatives',n_states)
#if defined _FFTW
 fftw_plan = 0
#endif
 !
 do ik=der_k(1),der_k(2)
   do ib=n_bands(1),n_bands(2)
     do i_spin=1,n_sp_pol   
       ifft=wf_state(ib,ik,i_spin)
       !
       ! Evaluate the wf_derivative in G space
       ! wf_derivative(:,i)=-i grad_i wf(:)=p_i wf(:)    i=x,y
       ! wf_derivative(G,i)= (k_i+G_i) wf(:)
       ! wf_z is not needed
       do i1=1,3
         if(.not.do_der(i1)) cycle
         wf_der=(0.,0.)
         wf_der(:)=wf(:,ifft)*(k_pt(ik,i1)+g_vec(: wf_ng ,i1))*2.0_SP*pi/alat(i1)
         !
         ! Here we FFT wf_derivative to real space
         wf_DP=(0._DP,0._DP)
         wf_DP(fft_g_table(1:wf_ng,1))= wf_der(1:wf_ng)*fft_norm
#if defined _FFTW
         call fft_3d(wf_DP,fft_dim,1,fftw_plan)
#else
         call fft_3d(wf_DP,fft_dim,1)
#endif
         if(i1==1) wf_x(:,ifft)=wf_DP(:)
         if(i1==2) wf_y(:,ifft)=wf_DP(:)
         if(i1==3) wf_z(:,ifft)=wf_DP(:)
         !
       enddo
       call live_timing(steps=1)
       !
     enddo
   enddo
 enddo
 !
 call live_timing()
 call WF_free()
 !
 deallocate(wf_DP)
 call mem_est("wf_DP")
 deallocate(wf_der)
 call mem_est("wf_der")
 !
end subroutine

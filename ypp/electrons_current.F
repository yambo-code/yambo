! 
! Copyright (C) 2000-2010 D. Sangalli and the YAMBO team
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
subroutine electrons_current(Xk,Xen)
 !
 use pars,           ONLY:SP
 use units,          ONLY:AU2nAmpere
 use YPP,            ONLY:mag_dir,v2plot,plot_dim,use_gnuplot,use_xcrysden,output_fname, &
&                         plot_title
 use com,            ONLY:msg,of_open_close,warning
 use stderr,         ONLY:intc
 use electrons,      ONLY:n_sp_pol,levels,default_nel,Boltz_Efield
 use R_lattice,      ONLY:bz_samp,k_pt
 use vec_operate,    ONLY:sort
 use FFT_m,          ONLY:fft_size
#if defined _YPP_MAGNETIC
 use magnetic,       ONLY:x,load_cc,free_cc,MAG_B,MAG_theta
#endif
 use timing,         ONLY:live_timing
 use wave_func,      ONLY:WF_load,WF_free,WF_derivative_free
 !
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 real(SP)     :: j_current(3,n_sp_pol,fft_size),jd_current(3,n_sp_pol,fft_size)
 real(SP)     :: r(fft_size)
 real(SP)     :: average_current(3),psi_current
 real(SP)     :: average_current_diam(3),psi_current_diam
 integer      :: i_dir,i_spin,ifft,ik
 character(4) :: current_spin
 logical      :: plot,cartesian,cylindrical
 logical      :: l_dir(3)
 real(SP),    allocatable :: vel_per_state(:,:,:,:)
 !
 cartesian=.true.
 cylindrical=.true.
 l_dir=.true.
 do i_dir=1,3
   if(Boltz_Efield(i_dir)==0.) l_dir(i_dir)=.false.
 enddo
 !
 if ( Xen%nbf == Xen%nbm ) then
   call warning ('Gap present: current is zero')
   return
 endif
 !
 call section('*','Current from a Boltzmann ditribution')
 !=======================================================
 !
 allocate(Xen%velocity(3,Xen%nb,Xen%nk,n_sp_pol))
 Xen%velocity=0._SP
 !
 call live_timing('Velocities',Xk%nibz)
 do ik=1,Xk%nibz
   !
   call WF_derivative((/1,Xen%nbm/),(/ik,ik/),l_dir)
   call WF_load(0,1,(/1,Xen%nbm/),(/ik,ik/),title='-WF',impose_free_and_alloc=.TRUE.)
   !
!   call el_velocity(Xen,Xk,ik,vel_per_state,l_dir)
   call live_timing(steps=1)
   !
   call WF_derivative_free()
   call WF_free()
   !
 enddo
 call live_timing()
 stop
 !
 call Boltzmann_occupations(Xen,Xk,vel_per_state)
 !
 deallocate(vel_per_state)
 !
 call section('*','Electrons-Holes Current')
#if defined _YPP_MAGNETIC
 call MAG_Hamiltonian(.true.)
#endif
 !
 j_current=0._SP
 jd_current=0._SP
 call live_timing('Evaluate current',Xk%nibz)
 do ik=1,Xk%nibz
   !
   call WF_derivative((/1,Xen%nbm/),(/ik,ik/),(/.true.,.true.,.true./))
   call WF_load(0,1,(/1,Xen%nbm/),(/ik,ik/),title='-WF',impose_free_and_alloc=.TRUE.)
   !
   call el_current(Xen,Xk,ik,j_current,jd_current)
   call live_timing(steps=1)
   !
   call WF_derivative_free()
   if(ik/=Xk%nibz) call WF_free()
   !
 enddo
 call live_timing()
 !
 average_current=0.
 average_current_diam=0.
 psi_current=0.
 psi_current_diam=0.
 !
 if (cartesian) then
   do i_dir=1,3
     do i_spin=1,n_sp_pol
       do ifft=1,fft_size
         average_current(i_dir)=average_current(i_dir)+j_current(i_dir,i_spin,ifft)
         average_current_diam(i_dir)=average_current_diam(i_dir)+jd_current(i_dir,i_spin,ifft)
       enddo
     enddo
   enddo
 endif
 !
#if defined _YPP_MAGNETIC
 if (cylindrical) then
   call load_cc(.true.,.true.)
   r(:)=sqrt( x(:,1)**2+x(:,2)**2 )
   do i_spin=1,n_sp_pol
     do ifft=1,fft_size
         psi_current=psi_current+j_current(2,i_spin,ifft)*(x(ifft,1)/r(ifft)) &
&                               -j_current(1,i_spin,ifft)*(x(ifft,2)/r(ifft))
         psi_current_diam=psi_current_diam+jd_current(2,i_spin,ifft)*(x(ifft,1)/r(ifft)) &
&                               -jd_current(1,i_spin,ifft)*(x(ifft,2)/r(ifft))
     enddo
   enddo
   call free_cc()
 endif
#endif
 !
 output_fname='mean_current'
 call of_open_close(trim(output_fname),'ot')
 call msg('o mean',"#")
 call msg('o mean',"#",'average current x , y , z [nanoAmpere]')
 call msg('o mean','',average_current*Au2nAmpere)
 call msg('o mean','',average_current_diam*Au2nAmpere)
 call msg('o mean',"#")
 call msg('o mean',"#",'psi_current [nanoAmpere]')
 call msg('o mean','',(/psi_current*Au2nAmpere/))
 call msg('o mean','',(/psi_current_diam*Au2nAmpere/))
 call of_open_close(trim(output_fname))
 !
 plot=.false.
 if(.not.plot) return
end subroutine

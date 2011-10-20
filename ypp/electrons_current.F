! 
! Copyright (C) 2000-2011 D. Sangalli and the YAMBO team
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
subroutine electrons_current(Xk,Xen,success)
 !
 use pars,           ONLY:SP
 use units,          ONLY:AU2nAmpere
 use YPP,            ONLY:v2plot,output_fname,nr,l_norm_to_one,use_xcrysden
 use com,            ONLY:msg,of_open_close,warning
 use electrons,      ONLY:n_sp_pol,levels,Boltz_Efield,Boltz_vel,Boltz_f
 use R_lattice,      ONLY:bz_samp
 use FFT_m,          ONLY:fft_size,fft_dim
#if defined _YPP_MAGNETIC
 use magnetic,       ONLY:x,load_cc,free_cc
#endif
#if defined _YPP_ELPH
 use R_lattice,      ONLY:nkibz
 use electrons,      ONLY:n_bands
#endif
 use timing,         ONLY:live_timing
 use wave_func,      ONLY:WF_load,WF_free,WF_derivative_free,wf_state,wf,wf_x,wf_y,wf_z
 !
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 logical       ::success
 !
 real(SP)     :: r(fft_size),average_current(3),average_current_diam(3)
#if defined _YPP_MAGNETIC
 real(SP)     :: psi_current,psi_current_diam
#endif
 integer      :: ic,i_spin,ifft,ik,ib,i_wf
 logical      :: l_directions(3)
 real(SP), allocatable :: J(:,:,:), J_diam(:,:,:)
#if defined _DOUBLE
 complex(SP)  :: zdotc
#else
 complex(SP)  :: cdotc
#endif
 !
 l_directions=.FALSE.
 do ic=1,3
   if(abs(Boltz_Efield(ic))>1.E-8) l_directions(ic)=.TRUE.
 enddo
 !
 success=.TRUE.
 if (.not.any(l_directions)) then
   success=.FALSE.
   return
 endif
 !
 if ( Xen%nbf == Xen%nbm ) then
   call warning ('Gap present: current is zero')
   success=.FALSE.
   return
 endif
 !
 call section('*','Current from a Boltzmann ditribution')
 !=======================================================
 !
 allocate(Boltz_vel(3,Xen%nb,Xen%nk,n_sp_pol))
 Boltz_vel=0._SP
 !
 do ik=1,Xk%nibz
   !
   call IO_and_Messaging_switch("-log")
   !
   ! NB Should I load more bands here to accomodate the new states created by Boltzmann ?
   !
#if defined _YPP_ELPH
   if (ik==1) then
     call WF_derivative((/1,n_bands/),(/1,nkibz/),l_directions)
     call WF_load(0,1,(/1,n_bands/),(/1,nkibz/),title='-WF',impose_free_and_alloc=.TRUE.)
   endif
#else
   call WF_derivative((/1,Xen%nbm/),(/ik,ik/),l_directions)
   call WF_load(0,1,(/1,Xen%nbm/),(/ik,ik/),title='-WF',impose_free_and_alloc=.TRUE.)
#endif
   !
   if (ik==1) call live_timing('Velocities & Current',Xk%nibz)
   !
   ! Velocities
   !
   do ib=1,Xen%nbm
     do i_spin=1,n_sp_pol
       i_wf=wf_state(ib,ik,i_spin)
       if (l_directions(1)) Boltz_vel(1,ib,ik,i_spin) = real( cdotc(fft_size, wf(:,i_wf),1,wf_x(:,i_wf),1) )
       if (l_directions(2)) Boltz_vel(2,ib,ik,i_spin) = real( cdotc(fft_size, wf(:,i_wf),1,wf_y(:,i_wf),1) )
       if (l_directions(3)) Boltz_vel(3,ib,ik,i_spin) = real( cdotc(fft_size, wf(:,i_wf),1,wf_z(:,i_wf),1) )
     enddo
   enddo
   !
   ! New (Boltzmann) occupations
   !
   call OCCUPATIONS_Boltzmann(Xen,Xk,ik)
   !
   if( ik == 1) then
     allocate(J(3,n_sp_pol,fft_size), J_diam(3,n_sp_pol,fft_size),v2plot(fft_size))
     nr=fft_dim
     J=0._SP
     J_diam=0._SP
     !
     ! Vector potential -> diamagnetic current ...
     !
#if defined _YPP_MAGNETIC
     !
     ! ... initial check only  ....
     call MAG_Hamiltonian(.true.)
     ! ... and A 
     call MAG_common_build_A(.true.)
     !
#endif
   endif
   !
   ! Current @ ik
   !==============
   !
   call el_current(Xen,Xk,ik,J,J_diam,l_directions)
   !
   ! Local Clean
   !
#if !defined _YPP_ELPH
   call WF_derivative_free()
   if (ik/=Xk%nibz) call WF_free()
#endif
   !
   ! -report added beacuse a small bug in WF_load  (QUIET_alloc call to IO_and...)
   !
   call IO_and_Messaging_switch("+log -report")
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
#if defined _MAGNETIC
 if(MAG_landau) call MAG_common_build_A(.false.)
#endif
#if defined _YPP_ELPH
 call WF_derivative_free()
#endif
 !
 ! Mean current in cartesian coordinates
 !
 do ic=1,3
   do i_spin=1,n_sp_pol
     average_current(ic)     =sum(J(ic,i_spin,:))
     average_current_diam(ic)=sum(J_diam(ic,i_spin,:))
   enddo
 enddo
 !
#if defined _YPP_MAGNETIC
 !
 ! Radial current
 !
 psi_current=0.
 psi_current_diam=0.
 !
 call load_cc(.true.,.true.)
 r(:)=sqrt( x(:,1)**2+x(:,2)**2 )
 do i_spin=1,n_sp_pol
  do ifft=1,size(J,3)
     psi_current=psi_current+(J(2,i_spin,ifft)*x(ifft,1)-J(1,i_spin,ifft)*x(ifft,2))/r(ifft)
     psi_current_diam=psi_current_diam+(J_diam(2,i_spin,ifft)*x(ifft,1)-J_diam(1,i_spin,ifft)*x(ifft,2))/r(ifft)
   enddo
 enddo
 call free_cc()
 !
#endif
 !
 ! Final Plot
 !
 l_norm_to_one=.FALSE.
 v2plot=(J(1,1,:)+J_diam(1,1,:))*AU2nAmpere
 !
 output_fname='el_current'
 if (use_xcrysden) output_fname='el_current.xsf'
 call of_open_close(trim(output_fname),'ot')
 call msg('o curr',"#")
 call msg('o curr','# Mean param. current x , y , z [nanoAmpere]:',average_current*AU2nAmpere,INDENT=0)
 call msg('o curr','# Mean diam.  current x , y , z [nanoAmpere]:',average_current_diam*AU2nAmpere,INDENT=0)
 call msg('o curr',"#")
 !
#if defined _YPP_MAGNETIC
 call msg('o curr','# Psi current                   [nanoAmpere]:',(/psi_current*AU2nAmpere/))
#endif
 !
 deallocate(Boltz_vel,Boltz_f,J,J_diam)
 !
end subroutine

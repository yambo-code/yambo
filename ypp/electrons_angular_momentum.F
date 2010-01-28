! 
! Copyright (C) 2000-2010 A. Marini, D. Sangalli and the YAMBO team
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
subroutine electrons_angular_momentum(nkpt_angular,nband_angular)
 !
 ! Important:
 ! We are in presence of a potential vector field, so
 ! L = r x p                  is the quantized canonical momentum
 ! M = r x (p-A) = r x mv     is the mechanical angula momentum,
 !                            which is gauge invariant
 !
 use pars,           ONLY:SP,lchlen,schlen
 use xc_functionals, ONLY:two_spin_density
 use magnetic,       ONLY:MAG_landau,MAG_pauli,&
&                         A_magn_x,A_magn_y,A_magn_z,x,y,z,&
&                         MAG_hamiltonian_type,Bx,By,Bz,&
&                         MAG_gauge
 use FFT_m,          ONLY:fft_size
 use D_lattice,      ONLY:a
 use wave_func,      ONLY:wf,wf_x,wf_y,wf_z,wf_state,WF_load, &
&                         WF_free,WF_derivative_free
 use SC,             ONLY:load_SC_components
 use electrons,      ONLY:n_sp_pol
 use timing,         ONLY:live_timing
 use units,          ONLY:BOHR
 use com,            ONLY:msg,of_open_close
 !
 implicit none
 !
 ! Input variables
 integer                  :: nkpt_angular(2), nband_angular(2),nkpt_tot,nband_tot
 !
 ! Internal variables
 complex(SP), allocatable :: Lx_tmp(:),Mx_tmp(:),Ly_tmp(:),My_tmp(:),Lz_tmp(:),Mz_tmp(:)
 complex(SP)              :: Lx,Mx,Ly,My,Lz,Mz
 logical                  :: angular_dir(3),do_derivative(3)
 !
 ! Dummyes
 integer                  :: ifft,ib,ik,i_spin
 character(lchlen)        :: fname(6)
 character(schlen)        :: titles(5)
 ! DEBUG <
 integer                  :: ir,i1
 ! DEBUG >
 !
#if defined _DOUBLE
 complex(SP):: zdotc
#else
 complex(SP):: cdotc
#endif
 !
 call load_SC_components('check_WF_DB',n_bands=nband_angular(2))
 !
 MAG_landau=trim(MAG_hamiltonian_type)=='landau' .or. &
&           trim(MAG_hamiltonian_type)=='all'
 MAG_pauli=trim(MAG_hamiltonian_type)=='pauli' .or. &
&          trim(MAG_hamiltonian_type)=='all'
 ! 
 ! Build the vector potential
 call MAG_common_build_A(.true.)
 ! 
 ! Choose which are the directions you are interested in
 angular_dir(:)=.false.
 if(Bx>epsilon(1.)) angular_dir(1)=.true.
 if(By>epsilon(1.)) angular_dir(2)=.true.
 if(Bz>epsilon(1.)) angular_dir(3)=.true.
 do_derivative(:)=.false.
 if(MAG_landau) then
   if(allocated(A_magn_x)) do_derivative(1)=.true.
   if(allocated(A_magn_y)) do_derivative(2)=.true.
   if(allocated(A_magn_z)) do_derivative(3)=.true.
 else
   ! Presently evaluate only the z component of the angular momentum if
   ! no landau term is not present
   do_derivative(1:2)=.true.
 endif
 !
 ! If ypp_magnetic is used on a database with no magnetic field
 ! then it evaluates Lz
 if(.not.any(angular_dir(:))) angular_dir(3)=.true.
 !
 ! Prepare files for the output
 fname=(/'el_angular_Lx','el_angular_Ly','el_angular_Lz','el_angular_Mx','el_angular_My','el_angular_Mz'/)
 titles(1)='band'
 titles(2)='kpt'
 titles(3)='spin'
 do i1=1,3
   if (.not.angular_dir(i1)) cycle
   call of_open_close(trim(fname(i1)),'ot')
   if(MAG_landau) call of_open_close(trim(fname(i1+3)),'ot')
   select case (i1)
     case(1)
       titles(4:5)=(/'re(L_x)','im(L_x)'/)
       call msg('o Lx','#',titles,INDENT=0,USE_TABS=.true.)    
       if(MAG_landau) then
         call msg('o Lx','#',(/'MAG_gauge:',MAG_gauge/),INDENT=0,USE_TABS=.true.)    
         titles(4:5)=(/'re(M_x)','im(M_x)'/)
         call msg('o Mx','#',titles,INDENT=0,USE_TABS=.true.)    
       endif
     case(2)
       titles(4:5)=(/'re(L_y)','im(L_y)'/)
       call msg('o Ly','#',titles,INDENT=0,USE_TABS=.true.)    
       if(MAG_landau) then
         call msg('o Ly','#',(/'MAG_gauge:',MAG_gauge/),INDENT=0,USE_TABS=.true.)    
         titles(4:5)=(/'re(M_y)','im(M_y)'/)
         call msg('o My','#',titles,INDENT=0,USE_TABS=.true.)    
       endif
     case(3)
       titles(4:5)=(/'re(L_z)','im(L_z)'/)
       call msg('o Lz','#',titles,INDENT=0,USE_TABS=.true.)    
       if(MAG_landau) then
         call msg('o Lz','#',(/'MAG_gauge:',MAG_gauge/),INDENT=0,USE_TABS=.true.)    
         titles(4:5)=(/'re(M_z)','im(M_z)'/)
         call msg('o Mz','#',titles,INDENT=0,USE_TABS=.true.)    
       endif
   end select
 enddo
 !
 call local_alloc()
 !
 do ik=nkpt_angular(1),nkpt_angular(2)
   !
   ! First wf_derivative (important)
   call WF_derivative(nband_angular,(/ik,ik/),do_derivative)
   call WF_load(0,1,nband_angular,(/ik,ik/),title='-MAG',impose_free_and_alloc=.TRUE.)
   !
   if(ik==nkpt_angular(1)) then
     nkpt_tot   = nkpt_angular(2)-nkpt_angular(1)
     nband_tot = nband_angular(2)-nband_angular(1)
     if(.not.MAG_landau) call live_timing("ypp: L",nkpt_tot*n_sp_pol*nband_tot)
     if(MAG_landau) call live_timing("ypp: L & M ",nkpt_tot*n_sp_pol*nband_tot)
   endif
   !
   do i_spin=1,n_sp_pol
     do ib=nband_angular(1),nband_angular(2)
       ifft=wf_state(ib,ik,i_spin)
       if(i_spin==2.and..not.MAG_pauli) cycle
       !
       ! Li =(r x p)_i for i=x,y,z
       ! These are gauge dependent if MAG_landau=.true.
       ! These are quantized if wfs are eigenstate
       if(angular_dir(1)) Lx_tmp(:)=y(:)*wf_z(:,ifft)-z(:)*wf_y(:,ifft)
       if(angular_dir(2)) Ly_tmp(:)=z(:)*wf_x(:,ifft)-x(:)*wf_z(:,ifft)
       if(angular_dir(3)) Lz_tmp(:)=x(:)*wf_y(:,ifft)-y(:)*wf_x(:,ifft)
       !
       if(MAG_landau) then
         ! Mi = [r x (p - A)]_i = Li - (r x A)_i for i=x,y,z
         ! These are the mechanicals momenta and are gauge independent
         if(angular_dir(1)) Mx_tmp(:)=Lx_tmp(:)-(y(:)*A_magn_z(:)- &
&                             z(:)*A_magn_y(:))*wf(:,ifft)
         if(angular_dir(2)) My_tmp(:)=Ly_tmp(:)-(z(:)*A_magn_x(:)- &
&                             x(:)*A_magn_z(:))*wf(:,ifft)
         if(angular_dir(3)) Mz_tmp(:)=Lz_tmp(:)-(x(:)*A_magn_y(:)- &
&                             y(:)*A_magn_x(:))*wf(:,ifft)
       endif
       !
       Lx=0.
       Ly=0.
       Lz=0.
       Mx=0.
       My=0.
       Mz=0.
       !
#if defined _DOUBLE
       if(angular_dir(1)) Lx = zdotc(fft_size,wf(:,ifft),1,Lx_tmp(:),1)
       if(angular_dir(2)) Ly = zdotc(fft_size,wf(:,ifft),1,Ly_tmp(:),1)
       if(angular_dir(3)) Lz = zdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
       if(MAG_landau) then
         if(angulr_dir(1)) Mx = zdotc(fft_size,wf(:,ifft),1,Mx_tmp(:),1)
         if(angulr_dir(2)) My = zdotc(fft_size,wf(:,ifft),1,My_tmp(:),1)
         if(angulr_dir(3)) Mz = zdotc(fft_size,wf(:,ifft),1,Mz_tmp(:),1)
       endif
#else
       if(angular_dir(1)) Lx = cdotc(fft_size,wf(:,ifft),1,Lx_tmp(:),1)
       if(angular_dir(2)) Ly = cdotc(fft_size,wf(:,ifft),1,Ly_tmp(:),1)
       if(angular_dir(3)) Lz = cdotc(fft_size,wf(:,ifft),1,Lz_tmp(:),1)
       if(MAG_landau) then
         if(angular_dir(1)) Mx = cdotc(fft_size,wf(:,ifft),1,Mx_tmp(:),1)
         if(angular_dir(2)) My = cdotc(fft_size,wf(:,ifft),1,My_tmp(:),1)
         if(angular_dir(3)) Mz = cdotc(fft_size,wf(:,ifft),1,Mz_tmp(:),1)
       endif
#endif
       if(angular_dir(1)) call msg('o Lx','',(/real(ib),real(ik),real(i_spin),real(Lx),aimag(Lx)/),INDENT=-2,USE_TABS=.true.)
       if(angular_dir(2)) call msg('o Ly','',(/real(ib),real(ik),real(i_spin),real(Ly),aimag(Ly)/),INDENT=-2,USE_TABS=.true.)
       if(angular_dir(3)) call msg('o Lz','',(/real(ib),real(ik),real(i_spin),real(Lz),aimag(Lz)/),INDENT=-2,USE_TABS=.true.)
       if(MAG_landau) then
         if(angular_dir(1)) call msg('o Mx','',(/real(ib),real(ik),real(i_spin),real(Mx),aimag(Mx)/),INDENT=-2,USE_TABS=.true.)
         if(angular_dir(2)) call msg('o My','',(/real(ib),real(ik),real(i_spin),real(My),aimag(My)/),INDENT=-2,USE_TABS=.true.)
         if(angular_dir(3)) call msg('o Mz','',(/real(ib),real(ik),real(i_spin),real(Mz),aimag(Mz)/),INDENT=-2,USE_TABS=.true.)
       endif
       !
       call live_timing(steps=1)
       !
     enddo
   enddo
   !
   call WF_derivative_free()
   call WF_free()
   !
 enddo
 !
 call live_timing() 
 !
 do i1=1,3
   if (.not.angular_dir(i1)) cycle
   call of_open_close(trim(fname(i1)))
   if(MAG_landau) call of_open_close(trim(fname(i1+3)))
 enddo
 !
 ! Free the vector potential
 call MAG_common_build_A(.false.)
 !
 contains
   subroutine local_alloc()
     if(angular_dir(1)) then
       allocate(Lx_tmp(fft_size))
       Lx_tmp=(0.,0.)
       Lx=(0.,0.)
     endif
     if(angular_dir(2)) then
       allocate(Ly_tmp(fft_size))
       Ly_tmp=(0.,0.)
       Ly=(0.,0.)
     endif
     if(angular_dir(3)) then
       allocate(Lz_tmp(fft_size))
       Lz_tmp=(0.,0.)
       Lz=(0.,0.)
     endif
     if(MAG_landau) then
       if(angular_dir(1)) then
         allocate(Mx_tmp(fft_size))
         Mx_tmp=(0.,0.)
         Mx=(0.,0.)
       endif
       if(angular_dir(2)) then
         allocate(My_tmp(fft_size))
         My_tmp=(0.,0.)
         My=(0.,0.)
       endif
       if(angular_dir(3)) then
         allocate(Mz_tmp(fft_size))
         Mz_tmp=(0.,0.)
         Mz=(0.,0.)
       endif
     endif
   end subroutine 
   !
   subroutine local_free()
     if(angular_dir(1)) deallocate(Lx_tmp)
     if(angular_dir(2)) deallocate(Ly_tmp)
     if(angular_dir(3)) deallocate(Lz_tmp)
     if(MAG_landau) then
       if(angular_dir(1)) deallocate(Mx_tmp)
       if(angular_dir(2)) deallocate(My_tmp)
       if(angular_dir(3)) deallocate(Mz_tmp)
     endif
   end subroutine
   !
end subroutine

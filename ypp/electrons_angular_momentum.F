! 
! Copyright (C) 2000-2009 A. Marini, D. Sangalli and the YAMBO team
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
subroutine electrons_angular_momentum()
 !
 ! Important:
 ! We are in presence of a potential vector field, so
 ! L = r x p                  is the quantized canonical momentum
 ! M = r x (p-A) = r x mv     is the mechanical angula momentum,
 !                            which is gauge invariant
 !
 use pars,           ONLY:SP,lchlen,schlen
 use xc_functionals, ONLY:el_mag
 use magnetic,       ONLY:wf_x,wf_y,wf_z,MAG_landau,MAG_pauli,&
&                         A_magn_x,A_magn_y,A_magn_z,x,y,z,&
&                         MAG_hamiltonian_type,Bx,By,Bz
 use FFT_m,          ONLY:fft_dim,fft_size
 use D_lattice,      ONLY:a
 use wave_func,      ONLY:wf,wf_state,wf_load,WF_free
 use QP_m,           ONLY:QP_nk
 use SC,             ONLY:SC_bands,SC_R,load_SC_components
 use electrons,      ONLY:n_sp_pol
 use timing,         ONLY:live_timing
 use units,          ONLY:HARTREE,BOHR
 use com,            ONLY:msg,of_open_close
 !
 implicit none
 !
 ! Input variables
 integer                  :: call_type
 !
 ! Internal variables
 complex(SP), allocatable :: Lx_tmp(:),Mx_tmp(:),Ly_tmp(:),My_tmp(:),Lz_tmp(:),Mz_tmp(:)
 complex(SP)              :: Lx,Mx,Ly,My,Lz,Mz
 logical                  :: angular_dir(3),der_dir(3)
 !
 ! Dummyes
 integer                  :: ifft,ib,ik,i_spin
 character(lchlen)        :: fname(6)
 character(schlen)        :: titles(5)
 ! DEBUG <
 real(SP), allocatable    :: r_cell_cc(:,:)
 integer                  :: ir,ir1,ir2,ir3,i1
 ! DEBUG >
 !
#if defined _DOUBLE
 complex(SP):: zdotc
#else
 complex(SP):: cdotc
#endif
 !
 call load_SC_components('check_WF_DB',n_bands=SC_bands)
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
 der_dir(:)=.false.
 if(allocated(A_magn_x)) der_dir(1)=.true.
 if(allocated(A_magn_y)) der_dir(2)=.true.
 if(allocated(A_magn_z)) der_dir(3)=.true.
 !
 ! Prepare files for the output
 ! Evaluate L_z and Mz
 fname=(/'el_angular_Lx','el_angular_Ly','el_angular_Lz','el_angular_Mx','el_angular_My','el_angular_Mz'/)
 titles(1)='band'
 titles(2)='kpt'
 titles(3)='spin'
 do i1=1,3
   if (angular_dir(i1)) then
     call of_open_close(trim(fname(i1)),'ot')
     if(MAG_landau) call of_open_close(trim(fname(i1+3)),'ot')
     select case (i1)
       case(1)
         titles(4:5)=(/'re(L_x)','im(L_x)'/)
         call msg('o Lx','#',titles,INDENT=0,USE_TABS=.true.)    
         titles(4:5)=(/'re(M_x)','im(M_x)'/)
         call msg('o Mx','#',titles,INDENT=0,USE_TABS=.true.)    
       case(2)
         titles(4:5)=(/'re(L_y)','im(L_y)'/)
         call msg('o Lx','#',titles,INDENT=0,USE_TABS=.true.)    
         titles(4:5)=(/'re(M_y)','im(M_y)'/)
         call msg('o Mx','#',titles,INDENT=0,USE_TABS=.true.)    
       case(3)
         titles(4:5)=(/'re(L_z)','im(L_z)'/)
         call msg('o Lx','#',titles,INDENT=0,USE_TABS=.true.)    
         titles(4:5)=(/'re(M_z)','im(M_z)'/)
         call msg('o Mx','#',titles,INDENT=0,USE_TABS=.true.)    
     end select
   endif
 enddo
 !
 call local_alloc()
 !
 call WF_derivative(.true.,(/1,SC_bands/),(/1,QP_nk/),der_dir)
 call wf_load(0,1,(/1,SC_bands/),(/1,QP_nk/),title='-MAG')
 !
 if(.not.MAG_landau) call live_timing("L",QP_nk*n_sp_pol*SC_bands)                        !*SC_bands) 
 if(MAG_landau) call live_timing("L & M ",QP_nk*n_sp_pol*SC_bands) !*SC_bands)
 !
 do ik=1,QP_nk
   !
   do i_spin=1,n_sp_pol
     do ib=1,SC_bands
       ifft=wf_state(ib,ik,i_spin)
       if(i_spin==2.and..not.MAG_pauli) cycle ! This will be needed for non collinear systems
       !
       !  (r x p)_x = Lx (this is quantized if wfs are eigenstate)
       if(angular_dir(1)) Lz_tmp(:)=y(:)*wf_z(:,ifft)-z(:)*wf_y(:,ifft)
       !  (r x p)_y = Ly (this is quantized if wfs are eigenstate)
       if(angular_dir(2)) Lz_tmp(:)=z(:)*wf_x(:,ifft)-x(:)*wf_z(:,ifft)
       !  (r x p)_z = Lz (this is quantized if wfs are eigenstate)
       if(angular_dir(3)) Lz_tmp(:)=x(:)*wf_y(:,ifft)-y(:)*wf_x(:,ifft)
       !  r2 and r
       !r2_tmp(:)=rad(:)**2*wf(:,ifft)
       !r_tmp(:)=rad(:)*wf(:,ifft)
       !
       if(MAG_landau) then
         ! Subtract to L_i the expectation value of (r x A)_i i=x,y,z
         ! Mx= Lx - (r x A)_x
         if(angular_dir(1)) Mx_tmp(:)=Lx_tmp(:)-(y(:)*A_magn_z(:)- &
&                             z(:)*A_magn_y(:))*wf(:,ifft)
         ! My= Ly - (r x A)_y
         if(angular_dir(2)) My_tmp(:)=Ly_tmp(:)-(z(:)*A_magn_x(:)- &
&                             x(:)*A_magn_z(:))*wf(:,ifft)
         ! Mz= Lz - (r x A)_z
         if(angular_dir(3)) Mz_tmp(:)=Lz_tmp(:)-(x(:)*A_magn_y(:)- &
&                             y(:)*A_magn_x(:))*wf(:,ifft)
         ! v2 --> (< [(py-Ay)*cons(a) - (px-Ax)*sin(a)]^2 >)
         !v2_tmp(:)=  (wf_y(:,ifft) - A_magn_y(:)*wf(:,ifft))*sinphi(:) - &
&        !            (wf_x(:,ifft) - A_magn_y(:)*wf(:,ifft))*cosphi(:)  
       else
         ! v2 --> < (py*cos(a)-px*sin(a))^2 >
         !v2_tmp(:)= wf_y(:,ifft)*cosphi(:) - wf_x(:,ifft)*sinphi(:)
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
       !
       call msg('o Lx','',(/real(ib),real(ik),real(i_spin),real(Lx),aimag(Lx)/),INDENT=-2,USE_TABS=.true.)
       call msg('o Ly','',(/real(ib),real(ik),real(i_spin),real(Ly),aimag(Ly)/),INDENT=-2,USE_TABS=.true.)
       call msg('o Lz','',(/real(ib),real(ik),real(i_spin),real(Lz),aimag(Lz)/),INDENT=-2,USE_TABS=.true.)
       call msg('o Mx','',(/real(ib),real(ik),real(i_spin),real(Mx),aimag(Mx)/),INDENT=-2,USE_TABS=.true.)
       call msg('o My','',(/real(ib),real(ik),real(i_spin),real(My),aimag(My)/),INDENT=-2,USE_TABS=.true.)
       call msg('o Mz','',(/real(ib),real(ik),real(i_spin),real(Mz),aimag(Mz)/),INDENT=-2,USE_TABS=.true.)
       !
       call live_timing(steps=1)
       !
     enddo
   enddo
 enddo
 !
 call live_timing() 
 !
 do i1=1,6
   call of_open_close(trim(fname(i1)),'ot')
 enddo
 !
 if(MAG_landau) call MAG_common_build_A(.false.)
 if(MAG_landau) call WF_derivative(.false.,(/0,0/),(/0,0/),der_dir)    ! Deallocate the derivatives...
 call WF_free()
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

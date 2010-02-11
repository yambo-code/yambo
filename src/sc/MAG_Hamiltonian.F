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
subroutine MAG_Hamiltonian(initial_check_only)
 !
 ! ANDREA 3/7/09
 ! 
 ! STILL TODO: 
 !
 !  1. add spin density check in SC cycle (including mixing)
 !  2. Save el_spin_density in SC DBs
 !  3. Check if SC works in metals (not obvious)
 !
 ! WARNING: you need to put you system at the corner of the supercell <---
 !
 ! This subroutine evaluate the magnetic Hamiltonian in the space of 
 ! KS wave-functions.
 ! MAG_B and MAG_radius are given in input
 ! Landau magnetism, possible only for isolated or 1D systems
 ! If MAG_radius == 0 then Bz=MAG_B everywhere
 ! If MAG_radius /= 0 then we have the field inside a circle in each supercell.
 ! Pauli magnetism, for every system, everywhere, except when Landau is on;
 ! Then we have magnetic field in the same region of the supercell.
 !
 ! The field due to a single supercell is supposed to be:
 ! *******************************************************************************************
 !                               [  MAG_B    r<=MAG_radius
 !    Bx=0   ,   By=0    ,  Bz = {                                ====> A; B=rot(A)
 !                               [   0       r> MAG_radius  
 !
 ! With three possible gauges for the vector potential
 ! *****************************************************
 ! Gauge 0 --> Symmetric gauge
 !      [ -0.5*MAG_B y                                     [ 0.5*MAG_B x
 ! Ax = {                                             Ay = {                                          A_z=0
 !      [ -0.5*MAG_B (MAG_radius**2)*y/(x**2+y**2)         [ 0.5*MAG_B (MAG_radius**2)*x/(x**2+y**2)
 !
 ! Gauge 1 --> Asymmetric gauge (active only if MAG_radiu==0)
 ! Ax = -MAG_B y        Ay=  0              Az=0    
 ! Gauge 1 --> Asymmetric gauge (active only if MAG_radiu==0)
 ! Ax =  0              Ay=  MAG_B x        Az=0    
 ! *******************************************************************************************
 !
 !
 use pars,           ONLY:SP,pi
 use SC,             ONLY:Ho
 use memory_m,       ONLY:mem_est
 use com,            ONLY:warning,error
 use FFT_m,          ONLY:fft_size
 use D_lattice,      ONLY:a
 use R_lattice,      ONLY:nkibz
 use magnetic,       ONLY:MAG_radius,MAG_B,MAG_hamiltonian_type,MAG_landau,MAG_pauli, &
&                         MAG_gauge,A_magn_x,A_magn_y,A_magn_z,x,y,z,  &
&                         Bx,By,Bz,MAG_theta,MAG_psi,ir_table
 use electrons,      ONLY:n_sp_pol
 use SC,             ONLY:SC_bands,SC_potential
 use wave_func,      ONLY:wf,wf_state,wf_x,wf_y,wf_z,WF_load,WF_free,WF_derivative_free
 use timing,         ONLY:live_timing
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 use vec_operate,    ONLY:v_norm
 implicit none
 !
 logical :: initial_check_only
 !
 ! Parallelization indexes
 !
 type(pp_indexes) ::px
 !
 ! Dummies
 !
 integer   :: ir,i_spin,ifft,jfft,ik,ib,ibp   
 real(SP)  :: radius
 logical   :: do_derivatives(3),H_magn_x_on,H_magn_y_on,H_magn_z_on
 logical   :: timing_init
 !
 ! Allocs
 !
 real(SP),    allocatable :: r_cell_cc(:,:)
 complex(SP), allocatable :: H_tmp(:)
 real(SP),    allocatable :: H_paramagn_x(:),H_paramagn_y(:),H_paramagn_z(:),&
&                            H_diamagn(:), H_pauli(:,:)
 !
 ! From lapac libraries, functions for the scalar product of two vectors
 !
#if defined _DOUBLE
 complex(SP):: zdotc,zdotu
#else
 complex(SP):: cdotc,cdotu
#endif
 !
 if (trim(SC_potential)=='default'.and.initial_check_only) then
   call section('*','Magnetic field ('//trim(MAG_hamiltonian_type)//') Hamiltonian') 
 else if (initial_check_only) then
   call section('+','Magnetic field ('//trim(MAG_hamiltonian_type)//') Hamiltonian') 
 endif
 !
 if (initial_check_only) then
   ! Set up MAG_hamiltonian_type
   !============================
   MAG_landau=trim(MAG_hamiltonian_type)=='landau'.or.trim(MAG_hamiltonian_type)=='all'
   MAG_pauli =trim(MAG_hamiltonian_type)=='pauli'.or.trim(MAG_hamiltonian_type)=='all'
   ! Switch off Pauli if spin is not present
   if(n_sp_pol==1) then
     if(MAG_pauli) then
       MAG_pauli=.false.
       call warning('Pauli magnetism switched off (spin unpolarized system).')
     endif
     if(MAG_landau) MAG_hamiltonian_type='landau'
     if(.not.MAG_landau) call error('You need a spin polarized system')
   endif
   if(MAG_pauli .and. MAG_theta/=0.) &
     call warning('The B field is not along z => the spins are assumed to be aligned to B. Please check.')
   ! Impose the symmetric gauge
   ! Impose flux to follow the a(3,:) direction 
   if(MAG_radius/=0) then
     if(.not.MAG_gauge=='SYMM') then
       MAG_gauge='SYMM'
       call warning('Finite radius B field in the asymm. gauge has not been implemented. Symm. gauge imposed')
     endif
     if(MAG_landau) then
       if(any(a(3,:2)>epsilon(1.))) then
         MAG_theta=atan(v_norm((/a(3,1),a(3,2),0./))/a(3,3)) 
         if(a(3,1)/=0) MAG_psi= atan( a(3,2) / a(3,1) )
         if(a(3,1)<0.) MAG_psi= MAG_psi+pi
         if(a(3,1)==0 .and. a(3,2)>0.) MAG_psi=pi/2.
         if(a(3,1)==0 .and. a(3,2)<0.) MAG_psi=-pi/2.
       else
         MAG_theta=0.
         MAG_psi=0.
       endif
     endif
   endif
   return
 endif
 !
 if(MAG_landau) then
   call MAG_common_build_A(.true.)
   H_magn_x_on=.false.
   H_magn_y_on=.false.
   H_magn_z_on=.false.
   if(allocated(A_magn_x)) H_magn_x_on=.true.
   if(allocated(A_magn_y)) H_magn_y_on=.true.
   if(allocated(A_magn_z)) H_magn_z_on=.true.
 else if(MAG_pauli) then
   call fft_setup(0,1,.false.) ! FFT size
 endif
 !
 ! Symmetries check
 call SC_check_symmetries((/Bx,By,Bz/))
 !
 call local_alloc(0) 
 !
 ! Build now the magnetic Hamiltonian
 ! To calculate the m.e. of the B-induced Hamiltonian we need
 ! to evaluate the WFs derivative
 !
 ! 1) H_paramagn = 1/2*(Ax px + Ay py + Az pz) + (px Ax + py Ay + Az pz)   as [px,Ax]+[py,Ay]+[pz,Az]=0 we obtain 
 !               = (Ax px + Ay py + Az pz)            (orbital component only of H_paramagnetic)
 ! 2) H_diamagn  = 1/2*(Ax**2 + A_y**2 + A_z**2)
 ! 3) H_pauli    = 1/2*(sigma ddot rot^A)        (spin component of H_paramagnetic)
 !               = 1/2*(sigma ddot B) (remember here that the hamiltonian has a minus
 !                               which is canceled by the minus of the magnetic
 !                               moment with respect to the spin)
 if (MAG_landau) then
   ! 1) H_paramagn (the px,py,pz component will be applied directly to the wf)
   if(H_magn_x_on) H_paramagn_x(:)=A_magn_x(:)
   if(H_magn_y_on) H_paramagn_y(:)=A_magn_y(:)
   if(H_magn_z_on) H_paramagn_z(:)=A_magn_z(:)
   ! 2) H_diamagnetic
   if(H_magn_x_on) H_diamagn(:)=A_magn_x(:)**2
   if(H_magn_y_on) H_diamagn(:)=H_diamagn(:)+A_magn_y(:)**2
   if(H_magn_z_on) H_diamagn(:)=H_diamagn(:)+A_magn_z(:)**2
   H_diamagn(:)=0.5*H_diamagn(:)
 endif
 !
 ! 3) H_pauli
 !    Here we have MAG_B, not Bz as the Pauli matricies are chosen
 !    so that the one along the B direction is diagonal
 if(MAG_pauli) then
   if(MAG_landau) then
     do ir=1,fft_size
       if(.not.ir_table(ir)) cycle
       H_pauli(ir,1)= 0.5*MAG_B
       H_pauli(ir,2)=-0.5*MAG_B
     enddo 
   else
     H_pauli(:,1)= 0.5*MAG_B
     H_pauli(:,2)=-0.5*MAG_B
   endif
 endif
 !
 ! Local alloc/free
 !==================
 if(MAG_landau) call MAG_common_build_A(.false.)
 !
 call local_free(1)
 call local_alloc(1)
 !
 ! Parallel indexes
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/nkibz,SC_bands/))
 call pp_redux_wait
 !==================================================
 !
 if (MAG_landau) then
   do_derivatives(:)=.false.
   if(H_magn_x_on) do_derivatives(1)=.true.
   if(H_magn_y_on) do_derivatives(2)=.true.
   if(H_magn_z_on) do_derivatives(3)=.true.
 endif
 !
 Ho=(0.,0.)
 timing_init=.true.
 !
 do ik=1,nkibz   
   ! 
   if( any(px%element_2D(ik,:)) ) then
     if (MAG_landau) call WF_derivative((/1,SC_bands/),(/ik,ik/),do_derivatives)
     call WF_load(0,1,(/1,SC_bands/),(/ik,ik/),title='-MAG',impose_free_and_alloc=.TRUE.)
     if(timing_init) call live_timing('MAG-Hamiltonian',px%n_of_element_1D(myid+1))
     timing_init=.false.
   endif
   !
   do ib=1,SC_bands
     !
     if (.not.px%element_2D(ik,ib)) cycle
     !
     do ibp=1,SC_bands
       do i_spin=1,n_sp_pol   
         ifft=wf_state(ib,ik,i_spin)
         jfft=wf_state(ibp,ik,i_spin)
         !
         ! Matrix Elements
         !=================
         ! Now we compute < n1| H_magn |n2 > = < n1 | H_paramagn_x | n2x> + 
         !                                     < n1 | H_paramagn_y | n2y> +
         !                                     < n1 | H_paramagn_z | n2z> +
         !                                     < n1 | H_diamagnetic| n2 >
         ! where n2x= px|n2 > ;  n2y= py|n2 > 
         !
         H_tmp=(0.,0.)
         !
         if (MAG_landau) then
           if(H_magn_x_on) H_tmp(:)= H_paramagn_x(:)*wf_x(:,jfft)
           if(H_magn_y_on) H_tmp(:)= H_tmp(:)+H_paramagn_y(:)*wf_y(:,jfft)
           if(H_magn_z_on) H_tmp(:)= H_tmp(:)+H_paramagn_z(:)*wf_z(:,jfft)
           H_tmp(:)=H_tmp(:)+H_diamagn(:)*wf(:,jfft)
         endif
         if (MAG_pauli)  H_tmp(:)= H_tmp(:)+H_pauli(:,i_spin)*wf(:,jfft)
         !
#if defined _DOUBLE
         Ho(ib,ibp,ik,i_spin) = zdotc(fft_size,wf(:,ifft),1,H_tmp(:),1)
#else
         Ho(ib,ibp,ik,i_spin) = cdotc(fft_size,wf(:,ifft),1,H_tmp(:),1)
#endif
         !
       enddo
     enddo
     !
     call live_timing(steps=1)
     !
   enddo
   !
   if(MAG_landau) call WF_derivative_free()
   call WF_free()
   !
   call pp_redux_wait(Ho(:,:,ik,:))
   !
 enddo
 !
 call live_timing()
 call pp_indexes_reset(px)
 !
 ! Clean Up
 !
 call local_free(2)
 !
 contains
   !
   subroutine local_alloc(level)
     integer :: level
     select case(level)
       case(0)
         allocate(r_cell_cc(fft_size,3))
         call mem_est("r_cell_cc",(/fft_size*3/))
         if (MAG_landau) then
           if(H_magn_x_on) allocate(H_paramagn_x(fft_size))
           if(H_magn_y_on) allocate(H_paramagn_y(fft_size))
           if(H_magn_z_on) allocate(H_paramagn_z(fft_size))
           call mem_est("H_paramagn_x H_paramagn_y H_paramagn_y",(/fft_size,fft_size,fft_size/))
           allocate(H_diamagn(fft_size))
           call mem_est("H_diamagn",(/fft_size/))
           if(H_magn_x_on) H_paramagn_x=0.
           if(H_magn_y_on) H_paramagn_y=0.
           if(H_magn_z_on) H_paramagn_z=0.
           H_diamagn=0.
         endif
         if (MAG_pauli) then
           allocate(H_pauli(fft_size,n_sp_pol))
           call mem_est("H_pauli",(/n_sp_pol*fft_size/))
           H_pauli=0.
         endif
       case(1)
         allocate(H_tmp(fft_size))
         call mem_est("H_tmp",(/fft_size/)*2)
         allocate(Ho(SC_bands,SC_bands,nkibz,n_sp_pol))
         call mem_est("Ho",(/size(Ho)/),(/2*SP/))
         Ho=(0.,0.)
         H_tmp=(0.,0.)
     end select
   end subroutine
   !
   subroutine local_free(level)
     integer :: level
     select case(level)
       case(1)
         deallocate(r_cell_cc)
         call mem_est("r_cell_cc")
       case(2)
         if (MAG_landau) then
           if(H_magn_x_on) deallocate(H_paramagn_x)
           if(H_magn_y_on) deallocate(H_paramagn_y)
           if(H_magn_z_on) deallocate(H_paramagn_z)
           deallocate(H_diamagn)
         endif
         if (MAG_pauli)  deallocate(H_pauli)
         deallocate(H_tmp)
         call mem_est("H_paramagn_x H_paramagn_y H_paramagn_y")
         call mem_est("H_pauli H_diamagn")
         call mem_est("H_tmp")
     end select
  end subroutine
  !
end subroutine

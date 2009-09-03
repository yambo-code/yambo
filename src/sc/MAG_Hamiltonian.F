!
! Copyright (C) 2000-2008 A. Marini, D. Sangalli and the YAMBO team 
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
 !  1. add magnetization check in SC cycle (including mixing)
 !  2. Save el_mag_n in SC DBs
 !  3. Check if SC works in metals (not obvious)
 !
 ! WARNING: you need to put you system at the corner of the supercell <---
 !
 ! This subroutine evaluate the magnetic Hamilnian in the space of 
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
 use pars,           ONLY:SP,DP,pi
 use SC,             ONLY:Ho
 use memory_m,       ONLY:mem_est
 use FFT_m,          ONLY:fft_size,fft_dim,fft_g_table,fft_norm,fftw_plan
 use D_lattice,      ONLY:a,alat
 use R_lattice,      ONLY:g_vec,k_pt,nkibz
 use magnetic,       ONLY:MAG_radius,MAG_B_z,MAG_hamiltonian_type,MAG_landau,MAG_pauli,&
&                         MAG_gauge,wf_x,wf_y,eval_Lz,A_magn_x,A_magn_y,gauge_factors
 use electrons,      ONLY:n_sp_pol
 use SC,             ONLY:SC_bands,SC_potential
 use wave_func,      ONLY:wf,wf_state,wf_load,WF_free,wf_ng
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
 integer   :: ir,ir1,ir2,ir3,i_spin,ifft,jfft,i1,ik,ib,ibp   
 real(SP)  :: radius, r_mod,cell_center(3),x,y
 !
 ! Allocs
 !
 real(SP),    allocatable :: r_cell_cc(:,:)
 complex(SP), allocatable :: H_tmp(:)
 real(SP),    allocatable :: H_paramagn_x(:),H_paramagn_y(:),&
&                            H_diamagn(:), H_pauli(:,:)
 complex(SP), allocatable :: wf_derivative(:,:)
 complex(DP), allocatable :: wf_DP(:)
 !
 ! From lapac libraries, functions for the scalar product of two vectors
 !
#if defined _DOUBLE
 complex(SP):: zdotc,zdotu
#else
 complex(SP):: cdotc,cdotu
#endif
 !
 if (initial_check_only) then
   MAG_landau=trim(MAG_hamiltonian_type)=='landau'.or.trim(MAG_hamiltonian_type)=='all'
   MAG_pauli =trim(MAG_hamiltonian_type)=='pauli'.or.trim(MAG_hamiltonian_type)=='all'
   if(MAG_radius==0) then
     eval_Lz=.true.
   else
     eval_Lz=.false.
     ! Set the gauge
     MAG_gauge='SYMM'
   endif
!   if (MAG_landau.and.MAG_radius<epsilon(1.)) then
!     MAG_hamiltonian_type='pauli'
!     MAG_landau=.false.
!   endif
   return
 endif
 !
 if (trim(SC_potential)=='default') then
   call section('*','Magnetic field ('//trim(MAG_hamiltonian_type)//') Hamiltonian') 
 else 
   call section('+','Magnetic field ('//trim(MAG_hamiltonian_type)//') Hamiltonian') 
 endif
 !
 ! Symmetries check
 !
 call SC_check_symmetries((/0.,0.,MAG_B_z/))
 !
 ! Wavefunctions 
 !===============
 !
 ! To construct the magnetic part of the Hamiltonian
 ! I need the wave functions in G space.
 !
 call IO_mute("report")
 if (MAG_landau) then
   call fft_setup(0,1,.false.) ! FFT size
   call wf_load(0,1,(/1,SC_bands/),(/1,nkibz/),space='G',title='-MAG')
 else
   call wf_load(0,1,(/1,SC_bands/),(/1,nkibz/),title='-MAG')
 endif
 call IO_mute(" ")
 !
 ! DEBUG <
 open(unit=3455, file='wfx.txt', status='unknown')
 open(unit=3456, file='wfy.txt', status='unknown')
 open(unit=3457, file='wfx_x.txt', status='unknown')
 open(unit=3458, file='wfy_y.txt', status='unknown')
 open(unit=3655, file='Ax.txt', status='unknown')
 open(unit=3656, file='Ay.txt', status='unknown')
 ! DEBUG >
 !
 ! Allocation 
 !============
 !
 call local_alloc(0)
 !
 ! Set the gauge factors
 !=======================
 if(MAG_gauge=='X_ASYMM') then
   gauge_factors(1)=2
   gauge_factors(2)=0
 else if(MAG_gauge=='Y_ASYMM') then
   gauge_factors(1)=0
   gauge_factors(2)=2
 else
   MAG_gauge='SYMM' 
   gauge_factors(:)=1
 endif
 !
 ! A(r) Build-Up
 ! Ax , Ay  (Az=0)   
 !=================
 !
 cell_center(:)=0.
 !
 do ir1 = 0, fft_dim(1)-1         ! x 
   do ir2 = 0, fft_dim(2)-1       ! y
     do ir3 = 0, fft_dim(3)-1     ! z
       ir = 1 + ir1 + ir2*fft_dim(1) + ir3*fft_dim(1)*fft_dim(2)
       ! Definition: rcell_i=a1_i+a_2i+a3_i     i=x,y,z
       !             in cartesian coordinates
       r_cell_cc(ir,:) =  ir1*a(1,:)/fft_dim(1) +&
&                         ir2*a(2,:)/fft_dim(2) +&
&                         ir3*a(3,:)/fft_dim(3)
       !
       cell_center(1)=a(1,1)/2+a(2,1)*ir2/fft_dim(2)+a(3,1)*ir3/fft_dim(3)
       cell_center(2)=a(2,2)/2+a(1,2)*ir1/fft_dim(1)+a(3,2)*ir3/fft_dim(3)
       !
       if (MAG_landau) then
         ! Radius in the direction perpendicular to the magnetic field
         r_mod=v_norm((/r_cell_cc(ir,1),r_cell_cc(ir,2),0./))
         ! correclty compute x and y with respect to cartesian axis
         ! They are saw-like and the jump is always at the axis parallel to the vectors
         ! which defines the supercell and which cross at the center of the supercell
         ! The differents if take into account how the supercell is "compound" around the
         ! supercell corner
         if (r_cell_cc(ir,2)<cell_center(2) .and. r_cell_cc(ir,1)<cell_center(1)) then
           y=r_cell_cc(ir,2)
           x=r_cell_cc(ir,1)
         endif
         if (r_cell_cc(ir,2)<cell_center(2) .and. r_cell_cc(ir,1)>cell_center(1)) then
           y=r_cell_cc(ir,2)-a(1,2)
           x=r_cell_cc(ir,1)-a(1,1)
         endif
         if (r_cell_cc(ir,2)>cell_center(2) .and. r_cell_cc(ir,1)<cell_center(1)) then
           y=r_cell_cc(ir,2)-a(2,2)
           x=r_cell_cc(ir,1)-a(2,1)
         endif
         if (r_cell_cc(ir,2)>cell_center(2) .and. r_cell_cc(ir,1)>cell_center(1)) then
           y=r_cell_cc(ir,2)-a(1,2)-a(2,2)
           x=r_cell_cc(ir,1)-a(2,1)-a(1,1)
         endif
         ! Now build up the vector potential         
         if(r_mod<=MAG_radius**2 .or. MAG_radius==0) then
           A_magn_x(ir)=-0.5*MAG_B_z*y
           A_magn_y(ir)= 0.5*MAG_B_z*x
           if (MAG_pauli) then
             H_pauli(ir,1)= MAG_B_z
             H_pauli(ir,2)=-MAG_B_z
           endif
         else
           A_magn_x(ir)=-0.5*MAG_B_z*MAG_radius**2*y/r_mod
           A_magn_y(ir)= 0.5*MAG_B_z*MAG_radius**2*x/r_mod
           ! I evaluate here the H_pauli
           if (MAG_pauli) then
             H_pauli(ir,1)=0 
             H_pauli(ir,2)=0
           endif
         endif
       else
         H_pauli(ir,1)= MAG_B_z
         H_pauli(ir,2)=-MAG_B_z
       endif
     enddo
   enddo
 enddo
 !
 if (MAG_landau) then
   !
   ! To calculate the m.e. of the B-induced Hamiltonian we need
   ! to evaluate the WFs derivative
   !
   ! 1) H_paramagn = (Ax px + Ay py) + (px Ax + py Ay)        as [px,Ax]+[py,Ay]=0 we obtain 
   !               = 2*(Ax px + Ay py)                    (orbital component only of H_paramagnetic)
   ! 2) H_diamagn  = Ax**2 + A_y**2
   ! 3) H_pauli    = \sigma \ddot \rot^A   (spin component of H_paramagnetic)
   !               = \sigma_z B_z (remember here that the hamiltonian has a minus
   !                               which is canceled by the minus of the magnetic
   !                               moment with respect to the spin)
   ! 1) H_paramagn (the px,py component will be applied directly to the wf)
   H_paramagn_x(:)=2*A_magn_x(:)*gauge_factors(1) 
   H_paramagn_y(:)=2*A_magn_y(:)*gauge_factors(2)
   ! 2) H_diamagnetic
   H_diamagn(:)=(A_magn_x(:)*gauge_factors(1))**2 + (A_magn_y(:)*gauge_factors(2))**2
   ! 3) H_pauli
   ! Already computed when A has been constructed
   !
   !
   ! Local alloc/free
   !==================
   call local_free(1)
   call local_alloc(1)
   !
   ! Parallel indexes
   !
   call pp_indexes_reset(px)
   call par_indexes(px,(/nkibz,SC_bands/))
   call pp_redux_wait
   call live_timing("WFs derivative",px%stps(myid+1))
   !==================================================
#if defined _FFTW
   fftw_plan = 0
#endif
   !
   do ik=1,nkibz   
     do ib=1,SC_bands
       ! 
       if (.not.px%i2p(ik,ib)) cycle
       !
       do i_spin=1,n_sp_pol   
         ifft=wf_state(ib,ik,i_spin)
         ! Evaluate the wf_derivative in G space
         ! wf_derivative(:,i)=-i grad_i wf(:)=p_i wf(:)    i=x,y
         ! wf_derivative(G,i)= (k_i+G_i) wf(:)
         ! wf_z is not needed
         !
         do i1=1,2
           wf_derivative(:,i1)=wf(:,ifft)*(k_pt(ik,i1)+g_vec(: wf_ng ,i1))*2.0_SP*pi/alat(i1)
         enddo
         ! Here we FFT wf_derivative to real space
         ! a) wf_x
         wf_DP=(0._DP,0._DP)
         wf_DP(fft_g_table(1:wf_ng,1))= wf_derivative(1:wf_ng,1)*fft_norm
#if defined _FFTW
         call fft_3d(wf_DP,fft_dim,1,fftw_plan)
#else
         call fft_3d(wf_DP,fft_dim,1)
#endif
         wf_x(:,ifft)=wf_DP(:)
         ! b) wf_y
         wf_DP=(0._DP,0._DP)
         wf_DP(fft_g_table(1:wf_ng,1))= wf_derivative(1:wf_ng,2)*fft_norm
#if defined _FFTW
         call fft_3d(wf_DP,fft_dim,1,fftw_plan)
#else
         call fft_3d(wf_DP,fft_dim,1)
#endif
         wf_y(:,ifft)=wf_DP(:)
       enddo
       !
       call live_timing(steps=1)
       !
     enddo
   enddo
   !
   call live_timing()
   call pp_redux_wait(wf_x)
   call pp_redux_wait(wf_y)
   call pp_indexes_reset(px)
   !
   !
   ! Free the wf in G space and load in real space
   !
   call WF_free()
   call wf_load(0,1,(/1,SC_bands/),(/1,nkibz/),title='-MAG')
   !
 endif
 !
 call local_free(2)
 call local_alloc(2)
 !
 ! DEBUG <
 ! For this debug r_cell_cc deallocation should be moved from
 ! case 1 to case 3 in local_free
 do ir1 = 10*fft_dim(2)+1, 10*fft_dim(2)+fft_dim(1)
   write(3455,*) r_cell_cc(ir1,1),real(wf(ir1,6:7)),aimag(wf(ir1,6:7))
   write(3457,*) r_cell_cc(ir1,1),-aimag(wf_x(ir1,6:7)),real(wf_x(ir1,6:7))
   write(3656,*) r_cell_cc(ir1,1),A_magn_y(ir1)
 enddo
 do ir = 1, fft_dim(2)
   ir2=(ir-1)*fft_dim(1)+10
   write(3456,*) r_cell_cc(ir2,2)*v_norm(a(2,:))/a(2,2),real(wf(ir2,6:7)),aimag(wf(ir2,6:7))
   write(3458,*) r_cell_cc(ir2,2)*v_norm(a(2,:))/a(2,2),-aimag(wf_y(ir2,6:7)*sqrt(3.)/2.-wf_x(ir2,6:7)*0.5),&
&                                   real(wf_y(ir2,6:7)*sqrt(3.)/2.-wf_x(ir2,6:7)*0.5)
   write(3655,*) r_cell_cc(ir2,2),A_magn_x(ir2)
 enddo
 ! DEBUG >
 !
 ! Matrix Elements
 !=================
 !
 ! Now we compute < n1| H_magn |n2 > = < n1| H_paramagn_x | n2x> + < n1 | H_paramagn_y | n2y> +
 !                                     + < n1| H_diamagnetic |n2 >
 ! were n2x= px|n2 > ;  n2y= py|n2 > 
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/nkibz,SC_bands/))
 call pp_redux_wait
 call live_timing("MAG-Hamiltonian",px%stps(myid+1))
 !
 do ik=1,nkibz   
   do ib=1,SC_bands   
     !
     if (.not.px%i2p(ik,ib)) cycle
     !
     do ibp=1,SC_bands
       do i_spin=1,n_sp_pol   
         !
         ifft=wf_state(ib,ik,i_spin)  
         jfft=wf_state(ibp,ik,i_spin) 
         !
         H_tmp=(0.,0.)
         !
         if (MAG_landau) H_tmp(:)= H_paramagn_x(:)*wf_x(:,jfft)+H_paramagn_y(:)*wf_y(:,jfft)  +&
&                                  H_diamagn(:)*wf(:,jfft)
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
 enddo
 !
 !
 call live_timing()
 call pp_redux_wait(Ho)
 call pp_indexes_reset(px)
 !
 ! If eval_Lz = .true. then
 !   store H_paramag_x,H_paramag_y in memory before the cleaning
 !   moreover wf_x and wf_y will not be cleaned
 !   and evaluate <i|Lz|j> on the LDA wf
 if(eval_Lz) call MAG_eval_Lz(1)  !  2*A_magn_y/MAG_B_z,-2*(A_magn_x/MAG_B_z)) 
 ! 
 ! Clean Up
 !
 call local_free(3)
 call WF_free()
 !
 ! DEBUG >
 close(unit=3455)
 close(unit=3456)
 close(unit=3457)
 close(unit=3458)
 close(unit=3655)
 close(unit=3656)
 ! DEBUG <
 !
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
           allocate(A_magn_x(fft_size),A_magn_y(fft_size))
           call mem_est("A_magn_x A_magn_y",(/fft_size,fft_size/))
           allocate(H_paramagn_x(fft_size),H_paramagn_y(fft_size))
           call mem_est("H_paramagn_x H_paramagn_y",(/fft_size,fft_size/))
           allocate(H_diamagn(fft_size))
           call mem_est("H_diamagn",(/fft_size/))
           A_magn_x=0.
           A_magn_y=0.
           H_paramagn_x=0.
           H_paramagn_y=0.
           H_diamagn=0.
         endif
         if (MAG_pauli) then
           allocate(H_pauli(fft_size,2))
           call mem_est("H_pauli",(/2*fft_size/))
           H_pauli=0.
         endif
       case(1)
         ! 
         ! Allocate the wf_derivative and wf_x, wf_y
         !
         allocate(wf_derivative(wf_ng,2))                     ! G space
         call mem_est("wf_derivative",(/wf_ng*2/))
         allocate(wf_DP(fft_size))                            ! real space
         call mem_est("wf_DP",(/fft_size/),(/DP/))
         allocate(wf_x(fft_size,nkibz*n_sp_pol*SC_bands),wf_y(fft_size,nkibz*n_sp_pol*SC_bands))   ! real space
         call mem_est("wf_x wf_y",(/fft_size*nkibz*n_sp_pol*SC_bands,fft_size*nkibz*n_sp_pol*SC_bands/))
         wf_derivative=(0.,0.)
         wf_DP=(0._DP,0._DP)
         wf_x=(0.,0.)
         wf_y=(0.,0.)
       case(2)
         allocate(H_tmp(fft_size))
         call mem_est("H_tmp",(/fft_size/)*2)
         allocate(Ho(SC_bands,SC_bands,nkibz,n_sp_pol))
         call mem_est("Ho",(/size(Ho)/),(/2*SP/))
         H_tmp=(0.,0.)
     end select
   end subroutine
   !
   subroutine local_free(level)
     integer :: level
     select case(level)
       case(1)
         if(.not.eval_Lz) then
           deallocate(A_magn_x,A_magn_y)
           call mem_est("A_magn_x A_magn_y")
         endif
         ! NO DEBUG <
!         deallocate(r_cell_cc)
!         call mem_est("r_cell_cc")
         ! NO DEBUG >
       case(2)
         if (.not.MAG_landau) return
         deallocate(wf_DP)
         call mem_est("wf_DP")
         deallocate(wf_derivative)
         call mem_est("wf_derivative")
       case(3)
         if (MAG_landau) deallocate(H_paramagn_x,H_paramagn_y)
         if (MAG_pauli)  deallocate(H_pauli)
         if (MAG_landau) then
           deallocate(H_diamagn)
           if (.not.eval_Lz) deallocate(wf_x,wf_y) 
         endif
         deallocate(H_tmp)
         call mem_est("H_paramagn_x H_paramagn_y")
         call mem_est("H_pauli H_diamagn")
         call mem_est("wf_x wf_y")
         call mem_est("H_tmp")
         ! DEBUG <
         deallocate(r_cell_cc)
         call mem_est("r_cell_cc")
         ! DEBUG >
     end select
  end subroutine
  !
end subroutine

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
 use pars,           ONLY:SP,DP
 use SC,             ONLY:Ho
 use memory_m,       ONLY:mem_est
 use stderr,         ONLY:intc
 use FFT_m,          ONLY:fft_size,fft_dim
 use D_lattice,      ONLY:a,alat
 use R_lattice,      ONLY:g_vec,nkibz
 use magnetic,       ONLY:MAG_radius,MAG_B_z,MAG_hamiltonian_type,MAG_landau,MAG_pauli,&
&                         MAG_gauge,wf_x,wf_y,A_magn_x,A_magn_y,x,y,gauge_factors
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
 real(SP)  :: radius, r_mod,cell_center(3)
 !
 ! Allocs
 !
 real(SP),    allocatable :: r_cell_cc(:,:)
 complex(SP), allocatable :: H_tmp(:)
 real(SP),    allocatable :: H_paramagn_x(:),H_paramagn_y(:),&
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
 if (initial_check_only) then
   MAG_landau=trim(MAG_hamiltonian_type)=='landau'.or.trim(MAG_hamiltonian_type)=='all'
   MAG_pauli =trim(MAG_hamiltonian_type)=='pauli'.or.trim(MAG_hamiltonian_type)=='all'
   if(n_sp_pol==1) MAG_pauli=.false.
   if(MAG_radius/=0) then
     ! Impose the symmetric gauge
     MAG_gauge='SYMM'
   endif
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
 if(MAG_landau) call MAG_common_build_A(.true.)
 !
 call local_alloc(0) 
 !
 ! Build now the magnetic Hamiltonian
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
 if (MAG_landau) then
 ! 1) H_paramagn (the px,py component will be applied directly to the wf)
   H_paramagn_x(:)=2*A_magn_x(:)*gauge_factors(1) 
   H_paramagn_y(:)=2*A_magn_y(:)*gauge_factors(2)
 ! 2) H_diamagnetic
   H_diamagn(:)=(A_magn_x(:)*gauge_factors(1))**2 + (A_magn_y(:)*gauge_factors(2))**2
 endif
 ! 3) H_pauli
 if(MAG_pauli) then
   if(MAG_radius/=0) then
     do ir1 = 0, fft_dim(1)-1       ! x
       do ir2 = 0, fft_dim(2)-1       ! y
         do ir3 = 0, fft_dim(3)-1     ! z
           ir = 1 + ir1 + ir2*fft_dim(1) + ir3*fft_dim(1)*fft_dim(2)
           ! Definition: rcell_i=a1_i+a_2i+a3_i     i=x,y,z
           !             in cartesian coordinates
           r_cell_cc(ir,:) =  ir1*a(1,:)/fft_dim(1) +&
&                             ir2*a(2,:)/fft_dim(2) +&
&                             ir3*a(3,:)/fft_dim(3)
           ! Radius in the direction perpendicular to the magnetic field
           r_mod=v_norm((/r_cell_cc(ir,1),r_cell_cc(ir,2),0./))
           if(r_mod<=MAG_radius .or. MAG_radius==0) then
             H_pauli(ir,1)= MAG_B_z
             H_pauli(ir,2)=-MAG_B_z
           else
             H_pauli(ir,1)= MAG_B_z
             H_pauli(ir,2)=-MAG_B_z
           endif
         enddo
       enddo
     enddo
   else
     H_pauli(:,1)=MAG_B_z
     H_pauli(:,2)=MAG_B_z
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
 call par_indexes(px,(/nkibz/))
 call pp_redux_wait
 !==================================================
 !
 if (MAG_landau) call WF_derivative(.true.,(/1,SC_bands/),(/1,nkibz/),(/.true.,.true.,.false./))
 call wf_load(0,1,(/1,SC_bands/),(/1,nkibz/),title='-MAG')
 !
 do ik=1,nkibz   
   ! 
   if (.not.px%i1p(ik)) cycle
!   if (MAG_landau) call WF_derivative(.true.,(/1,SC_bands/),(/ik,ik/),(/.true.,.true.,.false./))
   !
   ! Wavefunctions 
!   call IO_mute("report")
!   call wf_load(0,1,(/1,SC_bands/),(/ik,ik/),title='-MAG')
!   call IO_mute(" ")
   !
   call live_timing('MAG-Hamiltonian. kpt#'//trim(intc(ik))//'',SC_bands*SC_bands*n_sp_pol)
   !
   do ib=1,SC_bands
     do ibp=1,SC_bands
       do i_spin=1,n_sp_pol   
         ifft=wf_state(ib,ik,i_spin)
         jfft=wf_state(ibp,ik,i_spin)
         !
         ! Matrix Elements
         !=================
         ! Now we compute < n1| H_magn |n2 > = < n1| H_paramagn_x | n2x> + < n1 | H_paramagn_y | n2y> +
         !                                     + < n1| H_diamagnetic |n2 >
         ! where n2x= px|n2 > ;  n2y= py|n2 > 
         !
         H_tmp=(0.,0.)
         !
         if (MAG_landau) H_tmp(:)= H_paramagn_x(:)*wf_x(:,jfft)+H_paramagn_y(:)*wf_y(:,jfft)  +&
&                                     H_diamagn(:)*wf(:,jfft)
         if (MAG_pauli)  H_tmp(:)= H_tmp(:)+H_pauli(:,i_spin)*wf(:,jfft)
         !
#if defined _DOUBLE
           Ho(ib,ibp,ik,i_spin) = zdotc(fft_size,wf(:,ifft),1,H_tmp(:),1)
#else
           Ho(ib,ibp,ik,i_spin) = cdotc(fft_size,wf(:,ifft),1,H_tmp(:),1)
#endif
         !
         call live_timing(steps=1)
         !
       enddo
     enddo
   enddo
   !
   call live_timing()
!   call WF_free()
!   if(MAG_landau) call WF_derivative(.false.,(/0,0/),(/0,0/),(/.true.,.true.,.false./))    ! Deallocate the derivatives...
   !
 enddo
 !
 if(MAG_landau) call WF_derivative(.false.,(/0,0/),(/0,0/),(/.true.,.true.,.false./))    ! Deallocate the derivatives...
 call WF_free()
 !
 call pp_redux_wait(Ho)
 call pp_indexes_reset(px)
 !
 ! Clean Up
 !
 call local_free(2)
 !
 contains
   !
   Subroutine local_alloc(level)
     integer :: level
     select case(level)
       case(0)
         allocate(r_cell_cc(fft_size,3))
         call mem_est("r_cell_cc",(/fft_size*3/))
         if (MAG_landau) then
           allocate(H_paramagn_x(fft_size),H_paramagn_y(fft_size))
           call mem_est("H_paramagn_x H_paramagn_y",(/fft_size,fft_size/))
           allocate(H_diamagn(fft_size))
           call mem_est("H_diamagn",(/fft_size/))
           H_paramagn_x=0.
           H_paramagn_y=0.
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
         if (MAG_landau) deallocate(H_paramagn_x,H_paramagn_y,H_diamagn)
         if (MAG_pauli)  deallocate(H_pauli)
         deallocate(H_tmp)
         call mem_est("H_paramagn_x H_paramagn_y")
         call mem_est("H_pauli H_diamagn")
         call mem_est("H_tmp")
     end select
  end subroutine
  !
end subroutine

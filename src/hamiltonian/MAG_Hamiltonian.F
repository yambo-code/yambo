!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine MAG_Hamiltonian()
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
 use pars,           ONLY:SP,cZERO
 use units,          ONLY:SPEED_OF_LIGHT
 use hamiltonian,    ONLY:Hzero
 use FFT_m,          ONLY:fft_size
 use D_lattice,      ONLY:sigma_x,sigma_y,sigma_z
 use R_lattice,      ONLY:nkibz
 use magnetic,       ONLY:MAG_B,MAG_landau,MAG_pauli,ir_table, &
&                         A_magn_x,A_magn_y,A_magn_z,Bx,By,Bz
 use electrons,      ONLY:n_sp_pol,n_spinor,n_spin
 use SC,             ONLY:SC_bands
 use wave_func,      ONLY:WF,wf_x,wf_y,wf_z,WF_derivative_free
 use LIVE_t,         ONLY:live_timing
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait
 use interfaces,     ONLY:PARALLEL_index,WF_load,WF_free
 use wrapper,        ONLY:Vstar_dot_V
 !
#include<memory.h>
 !
 ! Parallelization indexes
 !
 type(PP_indexes) ::px
 !
 ! Dummies
 !
 integer    :: ir,i_sp_pol,i_wf,ip_wf,ik,ib,ibp,i_spinor,j_spinor
 logical    :: do_derivatives(3),H_magn_x_on,H_magn_y_on,H_magn_z_on,timing_init
 !
 ! Allocs
 !
 real(SP),    allocatable :: r_cell_cc(:,:)
 complex(SP), allocatable :: H_tmp(:,:,:),H_pauli(:,:,:,:)
 real(SP),    allocatable :: H_paramagn_x(:),H_paramagn_y(:),H_paramagn_z(:),&
&                            H_diamagn(:) 
 !
 call MAG_common_build_A(.true.)
 !
 H_magn_x_on=allocated(A_magn_x)
 H_magn_y_on=allocated(A_magn_y)
 H_magn_z_on=allocated(A_magn_z)
 !
 ! Symmetries check
 call Check_symmetries((/Bx,By,Bz/))
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
   if(H_magn_x_on) H_paramagn_x(:)=A_magn_x(:)/SPEED_OF_LIGHT
   if(H_magn_y_on) H_paramagn_y(:)=A_magn_y(:)/SPEED_OF_LIGHT
   if(H_magn_z_on) H_paramagn_z(:)=A_magn_z(:)/SPEED_OF_LIGHT
   ! 2) H_diamagnetic
   if(H_magn_x_on) H_diamagn(:)=(A_magn_x(:)/SPEED_OF_LIGHT)**2
   if(H_magn_y_on) H_diamagn(:)=H_diamagn(:)+(A_magn_y(:)/SPEED_OF_LIGHT)**2
   if(H_magn_z_on) H_diamagn(:)=H_diamagn(:)+(A_magn_z(:)/SPEED_OF_LIGHT)**2
   H_diamagn(:)=0.5*H_diamagn(:)
 endif
 !
 ! 3) H_pauli
 !    Here we have MAG_B, not Bz as the Pauli matricies are chosen
 !    so that the one along the B direction is diagonal
 if(MAG_pauli) then
   if(n_sp_pol==2) then
     do ir=1,fft_size
       if(.not.ir_table(ir)) cycle
       H_pauli(ir,1,1,1)= 0.5*MAG_B/SPEED_OF_LIGHT
       H_pauli(ir,1,1,2)=-0.5*MAG_B/SPEED_OF_LIGHT
     enddo 
   else if(n_spinor==2) then
     ! Pauli matrices
     do ir=1,fft_size
       if(.not.ir_table(ir)) cycle
       H_pauli(ir,:,:,1)=Bx*sigma_x+By*sigma_y+Bz*sigma_z
     enddo 
     H_pauli=0.5*H_pauli/SPEED_OF_LIGHT
   endif
 endif
 !
 ! Local alloc/free
 !==================
 call MAG_common_build_A(.false.)
 !
 call local_free(1)
 call local_alloc(1)
 !
 ! Parallel indexes
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/nkibz,SC_bands(2)/))
 call PP_redux_wait
 !==================================================
 !
 if (MAG_landau) then
   do_derivatives(:)=.false.
   if(H_magn_x_on) do_derivatives(1)=.true.
   if(H_magn_y_on) do_derivatives(2)=.true.
   if(H_magn_z_on) do_derivatives(3)=.true.
 endif
 !
 timing_init=.true.
 !
 do ik=1,nkibz   
   ! 
   if( any(px%element_2D(ik,:)) ) then
     if (MAG_landau) call WF_derivative((/1,SC_bands(2)/),(/ik,ik/),do_derivatives)
     call WF_load(WF,0,1,(/1,SC_bands(2)/),(/ik,ik/),title='-MAG',impose_free_and_alloc=.TRUE.)
     if(timing_init) call live_timing('MAG-Hamiltonian',px%n_of_elements(myid+1)*n_sp_pol)
     timing_init=.false.
   endif
   !
   do i_sp_pol=1,n_sp_pol   
     !
     do ib=1,SC_bands(2)
       !
       if (.not.px%element_2D(ik,ib)) cycle
       !
       i_wf=WF%index(ib ,ik,i_sp_pol)
       !
       do ibp=1,SC_bands(2)
         !
         ip_wf=WF%index(ibp,ik,i_sp_pol)
         !
         do j_spinor=1,n_spinor
           !
           ! Matrix Elements
           !=================
           ! Now we compute < n1| H_magn |n2 > = < n1 | H_paramagn_x | n2x> + 
           !                                     < n1 | H_paramagn_y | n2y> +
           !                                     < n1 | H_paramagn_z | n2z> +
           !                                     < n1 | H_diamagnetic| n2 >
           ! where n2x= px|n2 > ;  n2y= py|n2 > 
           !
           H_tmp=cZERO
           !
           if (MAG_landau) then
             if(H_magn_x_on) H_tmp(:,j_spinor,j_spinor)= H_paramagn_x(:)*wf_x(:,j_spinor,ip_wf)
             if(H_magn_y_on) H_tmp(:,j_spinor,j_spinor)= H_tmp(:,j_spinor,j_spinor)+H_paramagn_y(:)*wf_y(:,j_spinor,ip_wf)
             if(H_magn_z_on) H_tmp(:,j_spinor,j_spinor)= H_tmp(:,j_spinor,j_spinor)+H_paramagn_z(:)*wf_z(:,j_spinor,ip_wf)
             H_tmp(:,j_spinor,j_spinor)=H_tmp(:,j_spinor,j_spinor)+H_diamagn(:)*WF%c(:,j_spinor,ip_wf)
           endif
           !
           do i_spinor=1,n_spinor
             !
             if (MAG_pauli) H_tmp(:,i_spinor,j_spinor)= H_tmp(:,i_spinor,j_spinor) &
             &                                          +H_pauli(:,i_spinor,j_spinor,i_sp_pol)*WF%c(:,j_spinor,ip_wf)
             !
             Hzero(ib,ibp,ik,i_sp_pol)= Hzero(ib,ibp,ik,i_sp_pol)+&
&              Vstar_dot_V(fft_size,WF%c(:,i_spinor,i_wf),H_tmp(:,i_spinor,j_spinor))
             !
           enddo
         enddo
         !
       enddo
       !
       call live_timing(steps=1)
       !
     enddo
     !
   enddo
   !
   if(MAG_landau) call WF_derivative_free()
   call WF_free(WF)
   !
   call PP_redux_wait(Hzero(:,:,ik,:))
   !
 enddo
 !
 call live_timing()
 call PP_indexes_reset(px)
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
         YAMBO_ALLOC(r_cell_cc,(fft_size,3))
         if (MAG_landau) then
           if(H_magn_x_on) then
             YAMBO_ALLOC(H_paramagn_x,(fft_size))
           endif
           if(H_magn_y_on) then
             YAMBO_ALLOC(H_paramagn_y,(fft_size))
           endif
           if(H_magn_z_on) then
             YAMBO_ALLOC(H_paramagn_z,(fft_size))
           endif
           YAMBO_ALLOC(H_diamagn,(fft_size))
           if(H_magn_x_on) H_paramagn_x=0._SP
           if(H_magn_y_on) H_paramagn_y=0._SP
           if(H_magn_z_on) H_paramagn_z=0._SP
           H_diamagn=0._SP
         endif
         if (MAG_pauli) then
           YAMBO_ALLOC(H_pauli,(fft_size,n_spin,n_spinor,n_sp_pol))
           H_pauli=cZERO
         endif
       case(1)
         YAMBO_ALLOC(H_tmp,(fft_size,n_spinor,n_spinor))
         H_tmp = cZERO
     end select
   end subroutine
   !
   subroutine local_free(level)
     integer :: level
     select case(level)
       case(1)
         YAMBO_FREE(r_cell_cc)
       case(2)
         if (MAG_landau) then
           if(H_magn_x_on) then
             YAMBO_FREE(H_paramagn_x)
           endif
           if(H_magn_y_on) then
             YAMBO_FREE(H_paramagn_y)
           endif
           if(H_magn_z_on) then
             YAMBO_FREE(H_paramagn_z)
           endif
           YAMBO_FREE(H_diamagn)
         endif
         if (MAG_pauli)  then
           YAMBO_FREE(H_pauli)
         endif
         YAMBO_FREE(H_tmp)
     end select
  end subroutine
  !
end subroutine

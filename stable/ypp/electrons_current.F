! 
! Copyright (C) 2000-2012 D. Sangalli and the YAMBO team
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
 use electrons,      ONLY:n_sp_pol,n_spinor,levels,Boltz_Efield,Boltz_vel,Boltz_f
 use R_lattice,      ONLY:bz_samp
 use FFT_m,          ONLY:fft_size,fft_dim
 use pseudo,         ONLY:pp_kbv_dim
 use D_lattice,      ONLY:n_atomic_species,n_atoms_species,nsym
 use magnetic,       ONLY:x,load_cc,free_cc,MAG_B,MAG_landau,new_pp,old_pp
 use timing,         ONLY:live_timing
 use wave_func,      ONLY:WF_load,WF_free,WF_derivative_free,wf_state,wf, &
&                         wf_x,wf_y,wf_z,wf_ng
 use IO_m,           ONLY:OP_RD_CL,OP_RD,RD,RD_CL,io_control,REP,VERIFY,DUMP,NONE
 use memory_m,       ONLY:mem_est
 !
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 logical       ::success
 !
 real(SP)     :: r(fft_size),average_current(3),average_current_diam(3),average_current_Vnl_x(3)
 real(SP)     :: psi_current,psi_current_diam,psi_current_Vnl_x
 integer      :: ic,i_spin,ifft,ik,ib,i_wf,pp_spin
 integer      :: ia,il,is,im
 logical      :: l_directions(3)
 real(SP), allocatable   :: J(:,:,:), J_diam(:,:,:), J_Vnl_x(:,:,:)
 complex(SP),allocatable :: kbv(:,:,:)
 !
#if defined _DOUBLE
 complex(SP)  :: zdotc
#else
 complex(SP)  :: cdotc
#endif
 ! I / O
 integer                 :: ID,io_KB_abinit_err,io_KB_pwscf_err,io_err(3)
 integer, external       :: ioKB_abinit,ioKB_pwscf
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
   call warning ('Gap present: ground state persistent currents only')
   !success=.FALSE.
   !return
 endif
 !
 ! Vector potential -> diamagnetic current
 !
 ! ... initial check only  ....
 if(MAG_B/=0)  call MAG_Hamiltonian(.true.)
 ! ... and A 
 if(MAG_landau) call MAG_common_build_A(.true.)
 !
 ! Non-local Pseudo -> commutator in the current
 !
 call Dipole_kb_init(Xen,X,io_err,ID)
 !
 io_KB_abinit_err= io_err(1)
 io_KB_pwscf_err = io_err(2)
 !io_Vnl_err     = io_err(3)
 !
 if(io_KB_abinit_err/=0.and.io_KB_pwscf_err/=0) return
 !
 call fft_setup(0,1,.true.)  ! fft_size
 !
 allocate(kbv(wf_ng,pp_kbv_dim,4))
 call mem_est("kbv",(/size(kbv)/))
 !
 allocate(new_pp(fft_size,pp_kbv_dim),old_pp(fft_size,pp_kbv_dim))
 call mem_est('new_pp old_pp',(/fft_size,fft_size,fft_size,fft_size/))
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
   call WF_derivative((/1,Xen%nbm/),(/ik,ik/),l_directions)
   call WF_load(0,1,(/1,Xen%nbm/),(/ik,ik/),title='-WF',impose_free_and_alloc=.TRUE.)
   !
   if ( Xen%nbf /= Xen%nbm ) then
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
   else
     if (ik==1) then
       call live_timing('Current',Xk%nibz)
       allocate(Boltz_f(Xen%nbm,Xk%nibz,nsym,n_sp_pol))
     endif
     forall (ib=1:Xen%nbm , i_spin=1:n_sp_pol ) Boltz_f(ib,ik,:,i_spin)=Xen%f(ib,ik,i_spin)
   endif
   !
   if( ik == 1) then
     allocate(J(fft_size,n_sp_pol,3),J_diam(fft_size,n_sp_pol,3),J_Vnl_x(fft_size,n_sp_pol,3))
     allocate(v2plot(fft_size))
     nr=fft_dim
     J=0._SP
     J_diam=0._SP
     J_Vnl_x=0._SP
   endif
   !
   if (io_KB_pwscf_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_KB_pwscf_err=ioKB_pwscf(ID)
   endif
   !
   if (io_KB_abinit_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_KB_abinit_err=ioKB_abinit(ID)
   endif
   !
   do i_spin=1,1 !n_spin to be fixed
     !  
     ! Pseudo(k+G,k+G',i1) = kbv^*(k+G,i1,1) kbv(k+G',i1,1)
     ! i1 runs over atomic_species, atoms_per_specie , l_per_atom, m=-l,-l+1,...,l-1,sl
     pp_spin=i_spin
     if (io_KB_abinit_err==0.and.n_spinor==2) pp_spin=1
     if (io_KB_pwscf_err ==0.and.n_sp_pol==2) pp_spin=1
     if (io_KB_abinit_err==0) call Dipole_kb_abinit_comp(ik,pp_spin,Xk,kbv)
     if (io_KB_pwscf_err ==0) call Dipole_kb_pwscf_comp(ik,pp_spin,Xk,kbv)
     ! 
     call MAG_pseudo_nl(kbv)
     !
     ! Current @ ik
     !==============
     !
     call el_current(Xen,Xk,ik,J,J_diam,J_Vnl_x,l_directions)
     !
   enddo
   !
   ! Local Clean
   !
   call WF_derivative_free()
   if (ik/=Xk%nibz) call WF_free()
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
 deallocate(kbv)
 call mem_est('kbv')
 !
 deallocate(new_pp,old_pp)
 call mem_est('new_pp old_pp')
 !
 if(MAG_landau) call MAG_common_build_A(.false.)
 !
 ! Mean current in cartesian coordinates
 !
 do ic=1,3
   do i_spin=1,n_sp_pol
     average_current(ic)      =sum(J(:,i_spin,ic))
     average_current_diam(ic) =sum(J_diam(:,i_spin,ic))
     average_current_Vnl_x(ic)=sum(J_Vnl_x(:,i_spin,ic))
   enddo
 enddo
 !
 ! Radial current
 !
 psi_current=0.
 psi_current_diam=0.
 psi_current_Vnl_x=0.
 !
 call load_cc(.true.,.true.)
 r(:)=sqrt( x(:,1)**2+x(:,2)**2 )
 do i_spin=1,n_sp_pol
  do ifft=1,size(J,1)
     psi_current=psi_current+(J(ifft,i_spin,2)*x(ifft,1)-J(ifft,i_spin,1)*x(ifft,2))/r(ifft)
     if(MAG_landau) psi_current_diam=psi_current_diam+&
&                  (J_diam(ifft,i_spin,2)*x(ifft,1)-J_diam(ifft,i_spin,1)*x(ifft,2))/r(ifft)
     psi_current_Vnl_x=psi_current_Vnl_x+&
&                  (J_Vnl_x(ifft,i_spin,2)*x(ifft,1)-J_Vnl_x(ifft,i_spin,1)*x(ifft,2))/r(ifft)
   enddo
 enddo
 call free_cc()
 !
 ! Final Plot
 !
 l_norm_to_one=.FALSE.
 v2plot=(J(:,1,1)+J_diam(:,1,1))*AU2nAmpere
 !
 output_fname='el_current'
 if (use_xcrysden) output_fname='el_current.xsf'
 call of_open_close(trim(output_fname),'ot')
 call msg('o curr',"#")
 call msg('o curr','# Mean param.  current x , y , z [nanoAmpere]:',average_current*AU2nAmpere,INDENT=0)
 call msg('o curr','# Mean diam.   current x , y , z [nanoAmpere]:',average_current_diam*AU2nAmpere,INDENT=0)
 call msg('o curr','# Mean [x,Vnl] current x , y , z [nanoAmpere]:',average_current_Vnl_x*AU2nAmpere,INDENT=0)
 call msg('o curr',"#")
 !
 call msg('o curr','# Psi current                   [nanoAmpere]:',(/psi_current*AU2nAmpere/))
 call msg('o curr','# Psi current diam              [nanoAmpere]:',(/psi_current_diam*AU2nAmpere/))
 call msg('o curr','# Psi current [x,Vnl]           [nanoAmpere]:',(/psi_current_Vnl_x*AU2nAmpere/))
 !
 deallocate(Boltz_vel,Boltz_f,J,J_diam,J_Vnl_x)
 !
end subroutine

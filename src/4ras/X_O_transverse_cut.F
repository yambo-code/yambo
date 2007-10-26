!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be   useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
 subroutine X_O_transverse_cut(Xen,Xk,X)
!====================================
   use pars
   use com,                     ONLY : msg ! DEBUG
   use timing,                  ONLY : live_timing
   use electrons,               ONLY : levels, n_spin, n_spinor
   use par_proc_m,              ONLY : pp_redux_wait, pp_indexes, myid, &
&                                      pp_indexes_reset
   use par_indexes_m,           ONLY : par_indexes
   use D_lattice,               ONLY : alat
   use R_lattice,               ONLY : g_vec, bz_samp
   use X_m,                     ONLY : X_t
   use memory_m,                ONLY : mem_est
   use wave_func,               ONLY : wf, wf_ng, wf_state
   use optcut,                  ONLY : Xcut_q0grad, ng_limits, setup_optcut, &
&                                      PScut, PScut_slow, PScut_better
   use surface_geometry,        ONLY : setup_gvecaff
   implicit none
   type(bz_samp),  intent(in)       :: Xk
   type(levels),   intent(in)       :: Xen
   type(X_t)                        :: X
!ws
   integer                          :: ik, i1, icfft, ivfft, ic, iv, is, i_spin
   real(SP)                         :: omega, field_dir_rot(3)
   type(pp_indexes)                 :: px
   real(SP), allocatable            :: kg(:,:)
   complex(SP)                      :: PS(3), spinor_avg(3)
   real(SP), parameter          :: fac = 0.70710678118654752440_SP
   character(schlen)                :: sch ! DEBUG

!    call setup_gvecaff
!    call nG_limits( Xk%nibz )


   allocate( kg(3,wf_ng) )

   call pp_indexes_reset(px)
   call par_indexes(px, (/ Xk%nibz, Xen%nbm /),(/ 1, X%ib(1) /))
   call live_timing('Optical strengths-cut (T):',px%stps(myid+1))
   call pp_redux_wait

   do ik = 1, Xk%nibz

!--> DEBUG
!    write(sch,'(a,i4,a,i4)') 'Loop over k oscillators: ',ik,'/',Xk%nibz
!    call msg('ns',trim(sch))
!    write(*,*) trim(sch)
!<-- DEBUG

     do i1 = 1,3
       kg(i1,:) = ( Xk%pt(ik,i1) + g_vec(1:wf_ng, i1) ) * 2. * pi/alat(i1) ! -i grad Wf
     enddo
     call setup_optcut( ik )

     do iv = X%ib(1), Xen%nbm
       if (.not.px%i2p(ik,iv)) cycle
       do i_spin   = 1, n_spin
         ivfft=wf_state(iv,ik,i_spin)
         do ic = Xen%nbf+1, X%ib(2)
           omega = Xen%E(iv,ik,i_spin)-Xen%E(ic,ik,i_spin)

           if (associated(Xen%Eo)) omega=Xen%Eo(iv,ik,i_spin)-Xen%Eo(ic,ik,i_spin)
           if (any((/-omega<X%ehe(1).and.X%ehe(1)>0.,-omega>X%ehe(2).and.X%ehe(2)>0./))) cycle
           if (abs(omega)<=1.E-5) cycle
           icfft=wf_state(ic,ik,i_spin)

           call PScut(PS, wf(:,ivfft), wf(:,icfft), kg = kg )
!<--- DEBUG
!          call PScut_slow(PS, wf(:,ivfft), wf(:,icfft), kg )
!          call PScut_better(PS, wf(:,ivfft), wf(:,icfft), kg )
!---> DEBUG

           Xcut_q0grad(:,ic,iv,ik,i_spin) = PS(:)/omega
         enddo ! cond. bands
       enddo   ! spinors
       !
       ! Correct for spinor case
       !
       if(n_spinor.eq.2) then
         do ic = Xen%nbf+1, X%ib(2)
           spinor_avg(:) =  ( Xcut_q0grad(:,ic,iv,ik,1) + Xcut_q0grad(:,ic,iv,ik,2) )*fac
           Xcut_q0grad(:,ic,iv,ik,1) = spinor_avg(:)
           Xcut_q0grad(:,ic,iv,ik,2) = spinor_avg(:)
         enddo
       endif
       call live_timing(steps=1)
     enddo   ! valence bands
   enddo     ! k-points

   call live_timing
!  call end_optcut
   deallocate(kg)
   return
 end subroutine X_O_transverse_cut

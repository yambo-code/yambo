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
   use electrons,               ONLY : levels, n_spin, n_spinor, n_sp_pol
   use par_proc_m,              ONLY : pp_redux_wait, pp_indexes, myid, &
&                                      pp_indexes_reset
   use par_indexes_m,           ONLY : par_indexes
   use D_lattice,               ONLY : alat
   use R_lattice,               ONLY : g_vec, bz_samp
   use X_m,                     ONLY : X_t
   use memory_m,                ONLY : mem_est
   use wave_func,               ONLY : wf, wf_ng, wf_state, wf_load
   use optcut,                  ONLY : Xcut_q0grad, ng_limits, setup_optcut, pscut, PScut_slow
   use surface_geometry,        ONLY : setup_gvecaff
   implicit none
   type(bz_samp),  intent(in)       :: Xk
   type(levels),   intent(in)       :: Xen
   type(X_t)                        :: X
   !
   ! Work Space
   !
   integer                          :: ik, i1, icfft, ivfft, ic, iv, is, i_spin, E_i_spin
   real(SP)                         :: Ev_m_Ec, field_dir_rot(3)
   type(pp_indexes)                 :: px
   real(SP), allocatable            :: kg(:,:)
   complex(SP)                      :: PS(3), spinor_avg(3), rho(3,n_spin)
   character(schlen)                :: sch ! DEBUG
!  real(SP), parameter          :: fac = 0.70710678118654752440_SP

!    call setup_gvecaff
!    call nG_limits( Xk%nibz )


   allocate( kg(3,wf_ng) )

   call pp_indexes_reset(px)
   call par_indexes(px, (/ Xk%nibz, Xen%nbm /),(/ 1, X%ib(1) /))
   call live_timing('Dipole, cut (T):',px%stps(myid+1))
   call pp_redux_wait

   do ik = 1, Xk%nibz

!--> DEBUG
!    write(sch,'(a,i4,a,i4)') 'Loop over k oscillators: ',ik,'/',Xk%nibz
!    call msg('ns',trim(sch))
!    write(*,*) trim(sch)
!<-- DEBUG

#if defined _BIGSYS
     call wf_load(0,1,X%ib,(/ik,ik/),space='G',title='-Oscillators/G space')
#endif

     do i1 = 1,3
       kg(i1,:) = ( Xk%pt(ik,i1) + g_vec(1:wf_ng, i1) ) * 2. * pi/alat(i1) ! -i grad Wf
     enddo
     call setup_optcut( ik )

     do iv = X%ib(1), Xen%nbm
       if (.not.px%i2p(ik,iv)) cycle
       do ic = Xen%nbf+1, X%ib(2)
         do i_spin   = 1, n_spin
           !
           rho(:,i_spin)=(0.,0.)
           !
           E_i_spin=i_spin
           if (n_sp_pol/=2) E_i_spin=1
           Ev_m_Ec = Xen%E(iv,ik,E_i_spin)-Xen%E(ic,ik,E_i_spin)

           if (associated(Xen%Eo)) Ev_m_Ec=Xen%Eo(iv,ik,E_i_spin)-Xen%Eo(ic,ik,E_i_spin)
           if (any((/-Ev_m_Ec<X%ehe(1).and.X%ehe(1)>0.,-Ev_m_Ec>X%ehe(2).and.X%ehe(2)>0./))) cycle
           if (any((/-Ev_m_Ec<X%ehe(1).and.X%ehe(1)>0.0_SP,&
&                    -Ev_m_Ec>X%ehe(2).and.X%ehe(2)>0.0_SP/))) cycle
           if (abs(Ev_m_Ec)<=1.E-5) cycle

           ivfft=wf_state(iv,ik,i_spin)
           icfft=wf_state(ic,ik,i_spin)

           call PScut(PS, wf(:,ivfft), wf(:,icfft), kg = kg )
!          call PScut_slow(PS, wf(:,ivfft), wf(:,icfft), kg )

!          Xcut_q0grad(:,ic,iv,ik,i_spin) = PS(:)/Ev_m_Ec
           rho(:,i_spin) = PS(:)/Ev_m_Ec
         enddo   ! spin
         !
         ! Correct for spin case
         !
         if (n_sp_pol==2) then
           Xcut_q0grad(:,ic,iv,ik,1)=rho(:,1)
           Xcut_q0grad(:,ic,iv,ik,2)=rho(:,2)
         else if (n_spinor==2) then
           Xcut_q0grad(:,ic,iv,ik,1)=(rho(:,1)+rho(:,2))
         else if (n_spin==1) then
           Xcut_q0grad(:,ic,iv,ik,1)=rho(:,1)
         endif

       enddo ! cond. bands
       call live_timing(steps=1)
     enddo   ! valence bands
   enddo     ! k-points

   call live_timing
!  call end_optcut
   deallocate(kg)
!  if (ioKB_err==0) then
!    deallocate(kbv)
!    call mem_est("KBV")
!    call PP_free()
!  endif
   !
   do i_spin=1,n_sp_pol
     call pp_redux_wait(Xcut_q0grad(:,:,:,:,i_spin))
   enddo
   !
   return
 end subroutine X_O_transverse_cut

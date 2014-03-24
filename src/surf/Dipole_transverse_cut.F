!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
 subroutine Dipole_transverse_cut(Xen,Xk,X)
!====================================
   use pars
   use com,                     ONLY : msg, error
   use LIVE_t,                  ONLY : live_timing
   use electrons,               ONLY : levels, n_spin, n_spinor, n_sp_pol
   use parallel_m,              ONLY : pp_redux_wait, pp_indexes, myid, &
&                                      pp_indexes_reset
   use interfaces,           ONLY : PARALLEL_index
   use D_lattice,               ONLY : alat
   use R_lattice,               ONLY : g_vec, bz_samp
   use X_m,                     ONLY : X_t, Dipole_bands_ordered
   use memory_m,                ONLY : mem_est
   use wave_func,               ONLY : WF, wf_ng
   use optcut,                  ONLY : DIP_iR_cut, ng_limits, setup_optcut, pscut, PScut_slow
   use surface_geometry,        ONLY : setup_gvecaff
   implicit none
   type(bz_samp),  intent(in)       :: Xk
   type(levels),   intent(in)       :: Xen
   type(X_t)                        :: X
   !
   ! Work Space
   !
   integer                          :: ik, i1, icfft, ivfft, ic, iv, is, i_spin, E_i_spin
   integer                          :: iv_max, ic_min
   real(SP)                         :: Ev_m_Ec, field_dir_rot(3)
   type(pp_indexes)                 :: px
   real(SP), allocatable            :: kg(:,:)
   complex(SP)                      :: PS(3), spinor_avg(3)
   complex(SP)                      :: rho(4)
   character(schlen)                :: sch 

   allocate( kg(3,wf_ng) )

 !
 ! Set up band limits
 !
 if (Dipole_bands_ordered) then
   iv_max=Xen%nbm
   ic_min=Xen%nbf+1
 else
   iv_max=X%ib(2)
   ic_min=X%ib(1)
 endif
 !
 ! Set up the parallel environment
 !
 call pp_indexes_reset(px)
#if defined _DISTRIBUTED
 call PARALLEL_index(px,(/Xk%nibz,X%ib(2)/),(/1,ic_min/))
#else
 call PARALLEL_index(px,(/Xk%nibz,iv_max/),(/1,X%ib(1)/))
#endif
 call live_timing('Dipole, cut (T):',px%n_of_elements(myid+1)*n_spin)
 call pp_redux_wait
 !


   do ik = 1, Xk%nibz

     do i1 = 1,3
       kg(i1,:) = ( Xk%pt(ik,i1) + g_vec(1:wf_ng, i1) ) * 2. * pi/alat(i1) ! -i grad Wf
     enddo
     call setup_optcut( ik )

     do i_spin   = 1, n_spin
       E_i_spin=i_spin
       if (n_spinor==2) E_i_spin=1
#if defined _DISTRIBUTED
       do ic=ic_min,X%ib(2)
         !
         if(.not.px%element_2D(ik,ic)) cycle
         !
         do iv=X%ib(1),iv_max
#else
       do iv=X%ib(1),iv_max
         !
         if (.not.px%element_2D(ik,iv)) cycle
         !
         do ic=ic_min,X%ib(2)
#endif
           !
           rho(:)=(0._SP,0._SP)
           !
           Ev_m_Ec = Xen%E(iv,ik,E_i_spin)-Xen%E(ic,ik,E_i_spin)

           if (associated(Xen%Eo)) Ev_m_Ec=Xen%Eo(iv,ik,E_i_spin)-Xen%Eo(ic,ik,E_i_spin)
           if (any((/-Ev_m_Ec<X%ehe(1).and.X%ehe(1)>0.0_SP,-Ev_m_Ec>X%ehe(2).and.X%ehe(2)>0.0_SP/))) cycle
           if (abs(Ev_m_Ec)<=1.E-5) cycle

           ivfft=WF%index(iv,ik,i_spin)
           icfft=WF%index(ic,ik,i_spin)
#if defined _DISTRIBUTED
         if(ivfft==0) call error(" Error in parallel wf distribution iv")
         if(icfft==0) call error(" Error in parallel wf distribution ic")
#endif

           call PScut(PS, WF%c(:,ivfft), WF%c(:,icfft), kg = kg )
!          call PScut_slow(PS, wf(:,ivfft), wf(:,icfft), kg )

!          DIP_iR_cut(:,ic,iv,ik,i_spin) = PS(:)/Ev_m_Ec
!          rho(:,i_spin) = PS(:)/Ev_m_Ec
           rho(1:3) = PS(:)/Ev_m_Ec
           if (abs(Ev_m_Ec)<=1.E-5_SP) rho(1:3)=(0._SP,0._SP)
           if (n_spinor==2) then
             DIP_iR_cut(:,ic,iv,ik,1)=DIP_iR_cut(:,ic,iv,ik,1)+rho(:3)
           else
             DIP_iR_cut(:,ic,iv,ik,i_spin) = rho(:3)
           endif
         !
         ! Correct for spin case
         !
!        if (n_sp_pol==2) then
!          DIP_iR_cut(:,ic,iv,ik,1)=rho(:,1)
!          DIP_iR_cut(:,ic,iv,ik,2)=rho(:,2)
!        else if (n_spinor==2) then
!          DIP_iR_cut(:,ic,iv,ik,1)=(rho(:,1)+rho(:,2))
!        else if (n_spin==1) then
!          DIP_iR_cut(:,ic,iv,ik,1)=rho(:,1)
!        endif

         enddo ! cond. bands
         call live_timing(steps=1)
       enddo   ! valence bands
     enddo     ! spin loop
   enddo       ! k-points
!DIP_iR = DIP_iR_cut
   call live_timing
   deallocate(kg)
   call pp_indexes_reset(px)
   !
   do i_spin=1,n_sp_pol
     call pp_redux_wait(DIP_iR_cut(:,:,:,:,i_spin))
   enddo
   !
   return
 end subroutine Dipole_transverse_cut

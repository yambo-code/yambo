!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine el_density_and_current(E,k,rho,J,force_spatial_inversion)
 !
 ! Electronic density (and current).
 !
 ! Note that here the identity
 !
 ! \sum_{S_k} F(S_k r) = 1/R_k \sum_{S} F(S r) (1)
 ! 
 ! where S_k is a symm. op. in the star of k, S is a generic symm. op.
 ! and R_k is the rank of the small group at k.
 !
 ! Using (1) the density is calculated in two steps
 !
 !  rho(r) = 1/nkibz \sum_{ n k S_k } f_{n k} |wf_{n k}(S_k^-1 r)|^2=
 !         = 1/nkibz \sum_{S} \sum_{n k} f_{n k}/R_k |wf_{n k}(S^-1 r)|^2 =
 !         = 1/nsym \sum_{S} ( \sum_{n k} f_{n k} w_k |wf_{n k}(S^-1 r)|^2 )
 !
 ! where we have used the relation
 !
 ! w_k = nsym/(nkibz * R_k)
 !
 ! Same procedure for the current
 !
 use pars,          ONLY:SP
 use com,           ONLY:warning
 use electrons,     ONLY:levels,n_spin,n_sp_pol
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym,i_time_rev,mag_syms
 use FFT_m,         ONLY:fft_size,fft_rot_r,fft_rot_r_inv
 use wave_func,     ONLY:WF
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_WF_linear,PAR_COM_density
 !
 implicit none
 type(bz_samp)::k
 type(levels) ::E       
 real(SP), optional             ::J(fft_size,3)
 real(SP), optional             ::rho(fft_size)
 logical,  optional, intent(in) ::force_spatial_inversion
 !
 ! Work Space 
 !
 integer    :: ib,ik,i_spin_wf,i_spin_E,ifft,rho_syms,i_s,i_r
 real(SP)   :: rho_no_sym(fft_size),J_no_sym(fft_size,3)
 complex(SP):: WF_der(3,fft_size)
 logical    :: warn_done,l_si,l_rho,l_J
 !
 ! Basic Setups
 !
 l_rho     =present(rho)
 l_J       =present(J)
 l_si      =.false.
 if (present(force_spatial_inversion)) l_si=force_spatial_inversion
 !
 if (l_rho) then
   rho       =0._SP
   rho_no_sym=0._SP
 endif
 if (l_J) then
   J         =0._SP
   J_no_sym  =0._SP
 endif
 !
 warn_done=.false.
 i_spin_E=1
 !
 bands_loop: do ib=1,E%nbm
   !
   do ik=1,k%nibz
     !
     if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
     !
     do i_spin_wf=1,n_spin
       !
       if (size(WF%index,1)<ib)        cycle
       if (size(WF%index,2)<ik)        cycle
       if (size(WF%index,3)<i_spin_wf) cycle
       !
       ifft=WF%index(ib,ik,i_spin_wf)
       if (n_sp_pol==2) i_spin_E=i_spin_wf
       !
       if (ifft==0) then
         if (.not.warn_done) call warning('Not enough states to calculate rho')
         warn_done=.true.
         cycle
       endif
       !
       if (l_rho) then
         !
         rho_no_sym(:)=rho_no_sym(:)+E%f(ib,ik,i_spin_E)*k%weights(ik)*abs(WF%c(:,ifft))**2.
         !
       endif
       !
       if (l_J) then
         !
         call eval_Gradient(WF%c(:,ifft),WF_der)
         !
         do i_r=1,fft_size
           J_no_sym(i_r,:)=J_no_sym(i_r,:)+E%f(ib,ik,i_spin_E)*k%weights(ik)*aimag(WF%c(i_r,ifft)*WF_der(i_r,:))
         enddo
         !
       endif
       !
     enddo
   enddo
 enddo bands_loop
 !
 if (l_rho) call PP_redux_wait(rho_no_sym,COMM=PAR_COM_density%COMM)
 if ( l_J ) call PP_redux_wait(J_no_sym  ,COMM=PAR_COM_density%COMM)
 !
 ! Symmetrization
 !
 rho_syms=nsym/(i_time_rev+1)
 !
 if (mag_syms) rho_syms=nsym 
 !
 if (l_rho) then
   do i_s=1,rho_syms
     rho(:)=rho(:)+real(rho_no_sym(fft_rot_r(i_s,:)),SP)/real(nsym,SP)
   enddo
   if (mag_syms)      return
   if (.not.l_si) rho(:)=(1+i_time_rev)*     rho(:)
   if (     l_si) rho(:)=   i_time_rev *real(rho(fft_rot_r_inv(:)),SP)+rho(:)
 endif
 !
 if (l_J) then
   do i_s=1,rho_syms
     J(:,:)=J(:,:)+real(J_no_sym(fft_rot_r(i_s,:),:),SP)/real(nsym,SP)
   enddo
 endif
 ! 
end subroutine

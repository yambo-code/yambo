!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): PM
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
subroutine QED_Sigma_c(E,k,q,qp)
 !
!Xen,Xem,ixyz,k,u)
 !
 !========================================================================================
 !This will hopefully evaluate the dipolar moments needed in the ele-photon self-energy
 !
 !========================================================================================
 !
 !Xen, Xem - levels 
 !ixyz - direction along the axis
 !k - k vector
 !u - k + q vector where q is the photon momentum
 !
! use pars,                ONLY:SP
 use drivers,             ONLY:l_elel_corr
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:qindx_S,bz_samp
 use QP_m,                ONLY:QP_t
!, n_spin, n_spinor, n_sp_pol
! use wave_func,           ONLY:wf_ng,WF
! use X_m,                 ONLY:X_alloc, X_t, DIP_iR, Dipole_bands_ordered,&
! &                             Vnl_commutator_warning, Dipole_uses_shifted_grids, DIP_P
! use collision,           ONLY:ggwinfo, collision_reset
! use interfaces,          ONLY:WF_load
! use SC,                  ONLY:SC_potential, SC_bands, it_now, local_V, WF_G_max, WF_Go_indx, SC_R,rho_n, Ho,&
! &                             SC_QP_setup
! use FFT_m,               ONLY:fft_rot_r,fft_size
!
 implicit none
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(QP_t)    ::qp
 !
 if (l_elel_corr) then
   call section('+','Correlation: Photon-mediated Self-energy')
 else
   call section('=','Correlation: Photon-mediated Self-energy')
 endif
 stop
!
! type(ggwinfo)           :: isc !Do I even need this?
! type(WAVEs)             :: WF
!
! integer :: ikbz1, ikbz2, ikibz1, ikibz2, is1, is2, ib1, ib2
! complex(SP) :: WF_symm1(fft_size, n_spinor), WF_symm2(fft_size, n_spinor)
! complex(SP), allocatable :: wf_int1(:,:,:,:), wf_int2(:,:,:,:)
!
!!call WF_load(WF, WF_G_max, WF_Go_indx, (/1,SC_bands(2)/), (/1,k%nibz/), space='R', title='-RT')
!!WF_load(WF, iG_in, iGo_max_in, bands_to_load, kpts_to_load, space)
!!bands_to_load, kpts_to_load = integer, array
!
!!WF_G_max, WF_Go_indx = integer - no idea what they are... maximum G vector and G0 index?
!!Can I get them from the SC
!
! call WF_Load(WF, WF_G_max, WF_Go_indx, (/1, SC_bands(2)/), (/1, Xk%nbz/), Space='R')
!
!!ikbz1, ikbz2 - ith k point in the Brillouin zone
!!ikibz1, ikiz2 - ith k point in the irreducible Brillouin zone - do I need them? With an electromagnetic vector potential
!!symetries may not be working that well
!
! allocate(wf_int1(1:nb, 1:Xk%nbz, 1:nb, 1:Xk%nbz))
!!wait, what the hell does nb stand for?
!
! do ikbz1 = 1, Xk%nbz
!!   ikibz1 = Xk%sstar(ikbz1, 1)
!   is1 = Xk%sstar(ikbz, 2)
!   do ikbz2 = 1, Xk%nbz
!!     ikibz1 = Xk%sstar(ikbz2, 1)
!     is2 = Xk%sstar(ikbz2, 2)
!       do ib1 = 1, nb
!         do ib2 = 1, nb
!           WF_apply_symm((/ib1, ikbz1, is1, 1/), wf_symm1)
!           WF_apply_symm((/ib2, ikbz2, is2, 1/), wf_symm2)
!           wf_int1(ib1, ikbz1, ib2, ikbz2) = dot_product(wf_symm1(:,1), wf_symm1(:,1))
!         end do
!       end do
!   end do
! end do
! 
! deallocate(wf_int1)
 !
end subroutine QED_sigma_c

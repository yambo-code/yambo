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
 use pars,                ONLY:SP
 use units,               ONLY:SPEED_OF_LIGHT
 use drivers,             ONLY:l_elel_corr
 use electrons,           ONLY:levels,n_spinor
 use R_lattice,           ONLY:qindx_S,bz_samp,nqbz
 use QP_m,                ONLY:QP_t,QP_n_G_bands,QP_table
 use vec_operate,         ONLY:v_norm
 use wave_func,           ONLY:WF,WAVEs
 use FFT_m,               ONLY:fft_size
 use interfaces,          ONLY:WF_load
 !
 implicit none
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(QP_t)    ::qp
 !
 ! WorkSpace
 integer       ::i_q_bz,i_q,i_q_s,i_qp,i_b,i_k,i_n,i_p_bz,i_p_G0,i_m,i_p_s,i_p,&
&                i_c,i_cp,i_wf_at_k,i_wf_at_p
 real(SP)      ::q_cc(3),Photon_E
 complex(SP)   ::Photon_pol_vec(3),XHI(3),O
 type(WAVES)   ::WF_g
 complex(SP), allocatable  ::WF_p(:,:)
 !
 if (l_elel_corr) then
   call section('+','Correlation: Photon-mediated Self-energy')
 else
   call section('=','Correlation: Photon-mediated Self-energy')
 endif
 !
 WF_g%b=0
 WF_g%k=0
 WF_g%space=' '
 !
 call WF_load(WF_g,0,1,(/1,QP_n_G_bands(2)/),(/1,k%nibz/),space='G',title='-[Photon-El]Sigma_c/G space')
 call WF_load(WF  ,0,1,(/1,QP_n_G_bands(2)/),(/1,k%nibz/),space='R',title='-[Photon-El]Sigma_c/R space')
 !
 allocate(WF_p(fft_size,n_spinor))
 !
 call k_ibz2bz(q,'c',.false.)
 !
 ! (i_n,i_k,i_k_s) --- (i_q,i_q_s) ---> (i_m,i_p,i_p_s)
 !
 Q_loop: do i_q_bz=1,nqbz 
   !
   i_q  =q%sstar(i_q_bz,1)
   i_q_s=q%sstar(i_q_bz,2)
   !
   q_cc=q%ptbz(i_q_bz,:)
   !
   !call QED_polarization_vectors(q_cc,Photon_pol_vec)
   !
   Photon_E=SPEED_OF_LIGHT*v_norm(q_cc)
   !
   QP_loop: do i_qp=1,qp%n_states
     !
     i_n = QP_table(i_qp,1)
     i_k = QP_table(i_qp,3)
     !
     i_p_bz = qindx_S(i_k,i_q_bz,1)
     i_p_G0 = qindx_S(i_k,i_q_bz,2)
     !
     i_p  =k%sstar(i_p_bz,1)
     i_p_s=k%sstar(i_p_bz,2)
     !
     B_loop: do i_m=QP_n_G_bands(1),QP_n_G_bands(2)
       !
       i_wf_at_k=WF%index(i_n,i_k,1)
       i_wf_at_p=WF%index(i_m,i_p,1)
       !
       ! WF%c(i_wf_at_k,:) = wf @ k in r-space -> O
       ! WF_g%c(i_wf_at_k,:) = wf @ k in G-space - > XHI
       !
       call WF_apply_symm((/i_m, i_p, i_p_s, 1/), WF_p)
       !
       O=dot_product(WF_p(:,1),WF%c(i_wf_at_k,:))
       !
       write (*,*) i_wf_at_p,i_wf_at_k,i_p_s,O,WF_p(1,1),WF%c(i_wf_at_k,1)
       !
       do i_c=1,3
         do i_cp=1,3
           !
           !QP_Sc(i_qp) = QP_Sc(i_qp) + ...
           !
           !
           !
           !
           !
           !
           !
         enddo 
       enddo 
       !
     enddo B_loop
   enddo QP_loop
 enddo Q_loop
 !
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

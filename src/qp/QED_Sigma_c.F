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
 use pars,                ONLY:SP,pi,cI
 use units,               ONLY:SPEED_OF_LIGHT
 use drivers,             ONLY:l_elel_corr
 use electrons,           ONLY:levels,n_spinor
 use D_lattice,           ONLY:idt_index,alat
 use R_lattice,           ONLY:qindx_S,bz_samp,nqbz,g_rot,g_vec
 use QP_m,                ONLY:QP_t,QP_n_G_bands,QP_table
 use vec_operate,         ONLY:v_norm
 use wave_func,           ONLY:WF,WAVEs,wf_x,wf_y,wf_z,WF_derivative_alloc,wf_ng
 use FFT_m,               ONLY:fft_size,fft_g_table
 use interfaces,          ONLY:WF_load
 !
 implicit none
 !
 type(levels)              ::E
 type(bz_samp)             ::k,q
 type(QP_t)                ::qp
 !
 ! WorkSpace
 integer                   ::i_q_bz,i_q,i_q_s,i_qp,i_b,i_k,i_n,i_p_bz,i_p_G0,i_m,i_p_s,i_p,&
&                            i_c,i_cp,i_wf_at_k,i_wf_at_p!,ifft
 real(SP)                  ::q_cc(3),k_cc(3),Photon_E,Photon_pol_vec(3,3)
 complex(SP)               ::XHI(3),O,P
 type(WAVES)               ::WF_g
 complex(SP), allocatable  ::WF_p(:,:),WF_p_rot(:,:),WF_der(:,:)
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
 allocate(WF_p(fft_size,n_spinor),WF_p_rot(fft_size,n_spinor),WF_der(wf_ng,3))
 !
 call k_ibz2bz(q,'c',.false.)
 !
 ! (i_n,i_k,i_k_s) --- (i_q,i_q_s) ---> (i_m,i_p,i_p_s)
 !
 P=(0.0_SP,0.0_SP)
 !
 Q_loop: do i_q_bz=1,nqbz 
   !
   i_q  =q%sstar(i_q_bz,1)
   i_q_s=q%sstar(i_q_bz,2)
   !
   q_cc=q%ptbz(i_q_bz,:)
   !
   call QED_polarization_vectors(q_cc,Photon_pol_vec)
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
       i_wf_at_k=WF_g%index(i_n,i_k,1)
       i_wf_at_p=WF_g%index(i_m,i_p,1)
       !
       ! WF%c(i_wf_at_k,:) = wf @ k in r-space -> O
       ! WF_g%c(i_wf_at_k,:) = wf @ k in G-space - > XHI
       !
       call WF_apply_symm((/i_m, i_p, i_p_s, 1/), WF_p)
       !
       !if I'm not wrong, we need the exp(iG_0*r) factor - to check with Andrea
       !also, how do I do it in real space?
!       if (i_p_s==idt_index) then
!         do ifft=1,wf_ng
!           write (*,*) ifft,wf_ng
!           WF_p_rot(ifft,1)=WF_p(fft_g_table(wf_ng,i_p_G0),1)
!         enddo
!       endif
!       if (i_p_s/=idt_index) WF_p_rot(1:fft_size,1)=WF_p(fft_g_table(g_rot(i_p_s,1:fft_size),i_p_G0),1)
       !
       O=dot_product(WF_p(:,1),WF%c(:,i_wf_at_k))
       !
!       if (i_wf_at_k == i_wf_at_p) then
!         write (*,*) i_wf_at_p,i_wf_at_k,i_p_s,O!,WF_p(1,1),WF%c(1,i_wf_at_k)
!       endif
!       write(*,*) i_m, QP_n_G_bands(2)
       write(*,*) 'is it working?'
       !
       do i_c=1,3
         do i_cp=1,3
           !
           WF_der(1:wf_ng,i_cp)=WF_g%c(1:wf_ng,i_wf_at_p)*g_vec(1:wf_ng,i_cp)*2.0_SP*pi/alat(i_cp)
           !
           XHI(i_cp) = dot_product(WF_der(1:wf_ng,i_cp),WF_g%c(1:wf_ng,i_wf_at_k))
           !
           !the next one is completely wrong, but I need the components of k. Since we don't do a loop on ikbz
           !I've no idea how to do it - ASK!!!!!
           P = P + Photon_pol_vec(i_cp,i_c)*(XHI(i_cp)*O - cI*(2.0_SP*pi*i_k - q_cc(i_cp))*real(conjg(O)*O))*q_cc(i_c)
!           QP_Sc(i_qp) = QP_Sc(i_qp) + 
           !
           !
         enddo 
       enddo 
       write(*,*) 'seems so'
       !
     enddo B_loop
   enddo QP_loop
 enddo Q_loop
 !
 stop
 !
end subroutine QED_sigma_c

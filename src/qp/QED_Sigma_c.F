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
 use pars,                ONLY:SP,pi,cI,cZERO
 use units,               ONLY:SPEED_OF_LIGHT,HA2EV
 use drivers,             ONLY:l_elel_corr
 use electrons,           ONLY:levels,n_spinor,spin_occ
 use D_lattice,           ONLY:idt_index,alat,a,Tel
 use R_lattice,           ONLY:qindx_S,bz_samp,nqbz,g_rot,g_vec
 use QP_m,                ONLY:QP_t,QP_n_G_bands,QP_table,QP_G_damp,QP_Sc,QP_dSc_steps
 use vec_operate,         ONLY:v_norm
 use wave_func,           ONLY:WF,WAVEs,wf_x,wf_y,wf_z,WF_derivative_alloc,wf_ng
 use FFT_m,               ONLY:fft_size,fft_g_table,fft_dim,fftw_plan,fft_norm
 use interfaces,          ONLY:WF_load
 use functions,           ONLY:Fermi_fnc,bose_f 
 use LIVE_t,        ONLY:live_timing
 !
 implicit none
 !
 type(levels)              ::E
 type(bz_samp)             ::k,q
 type(QP_t)                ::qp
 !
 ! WorkSpace
 integer                   ::i_q_bz,i_q,i_q_s,i_qp,i_b,i_k,i_n,i_p_bz,i_p_G0,i_m,i_p_s,i_p,&
&                            i_c,i_cp,i_wf_at_k,i_wf_at_p,i_k_bz,ir,ir1,ir2,ir3,i_wf_at_k_g,&
                             i_wf_at_p_g
 real(SP)                  ::q_cc(3),k_cc(3),Photon_E,Photon_pol_vec(3,3),sumdot
 complex(SP)               ::XHI(3),O,P,S
 type(WAVES)               ::WF_g
 complex(SP), allocatable  ::WF_p(:,:),WF_p_rot(:,:),WF_der(:,:),WF_der_g(:,:),fft_der(:),WF_der_rot(:,:)
 real(SP), allocatable     ::r_cell_cc(:,:)
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
 allocate(WF_p(fft_size,n_spinor),WF_p_rot(fft_size,n_spinor)) 
 allocate(r_cell_cc(3,fft_size))
 allocate(WF_der(fft_size,3),WF_der_g(wf_ng,3),fft_der(fft_size),WF_der_rot(fft_size,3))
 !
 ir = 0
 do ir1 = 0, fft_dim(1)-1
   do ir2 = 0, fft_dim(2)-1
     do ir3 = 0, fft_dim(3)-1
       ir = 1 + ir1 + ir2*fft_dim(1) + ir3*fft_dim(1)*fft_dim(2)
       r_cell_cc(:,ir) = ir1*a(1,:)/fft_dim(1) +&
&                        ir2*a(2,:)/fft_dim(2) +&
&                        ir3*a(3,:)/fft_dim(3)
     end do
   end do
 end do
 !
 call k_ibz2bz(q,'c',.false.)
 !
 ! (i_n,i_k,i_k_s) --- (i_q,i_q_s) ---> (i_m,i_p,i_p_s)
 ! 
 call live_timing('Sigma_c',nqbz) 
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
     i_k_bz=sum(k%nstar(:i_k-1))+1
     k_cc=q%ptbz(i_k_bz,:)
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

       i_wf_at_k_g=WF_g%index(i_n,i_k,1)
       i_wf_at_p_g=WF_g%index(i_m,i_p,1)
       !
       ! WF%c(i_wf_at_k,:) = wf @ k in r-space -> O
       ! WF_g%c(i_wf_at_k,:) = wf @ k in G-space - > XHI
       !
       call WF_apply_symm((/i_m, i_p, i_p_s, 1/), WF_p)
       !
       !We now evaluate the derivative of (i_m,i_p,i_p_s) in G space
       !
       WF_der=cZERO
       !
       !call eval_gradient(f,f_gradient)
       !
       do i_c=1,3
         !
         fft_der=cZERO
         WF_der_g(1:wf_ng,i_c)=WF_g%c(1:wf_ng,i_wf_at_p_g)*g_vec(1:wf_ng,i_c)*2.0_SP*pi/alat(i_c)
         !and fft it to real space
         fft_der(fft_g_table(1:wf_ng,1))=WF_der_g(1:wf_ng,i_c)
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan = 0
         call fft_3d(fft_der,fft_dim,1,fftw_plan)
#else
         call fft_3d(fft_der,fft_dim,1)
#endif
         WF_der(:,i_c)=fft_der(:)
       enddo
       !
       !Now we multiply (i_m,i_p,i_p_s) and its gradient by exp(iG_0r)
       do ir = 1, fft_size
         sumdot=0._SP
         do i_c=1, 3
           sumdot=sumdot + r_cell_cc(i_c,ir)*g_vec(i_p_G0,i_c)*2*pi/alat(i_c)
         enddo
         WF_p_rot(ir,1:n_spinor) = WF_p(ir,1:n_spinor)*exp(cI*sumdot)
         WF_der_rot(ir,1:3) = WF_der(ir,1:3)*exp(cI*sumdot)
       enddo
       !
       !<i_m,i_p,i_p_s|exp(-iG_0r)|i_n,i_k,i_k_s>
       O=dot_product(WF_p_rot(:,1),WF%c(:,i_wf_at_k))
       !
       P=cZERO
       do i_cp=1,3
         !
         !\int dr (i_n,i_k,i_k_s)exp(-iG_0r)\nabla_{i_cp}(i_m,i_q,i_q_s)^*
         XHI(i_cp) = dot_product(WF_der_rot(:,i_cp),WF%c(:,i_wf_at_k))
         !
         do i_c=1,3
           P=P + Photon_pol_vec(i_cp,i_c)*(XHI(i_cp)*O - cI*(k_cc(i_cp) - q_cc(i_cp) + &
&            g_vec(i_p_G0,i_cp)*2*pi/alat(i_cp))*real(conjg(O)*O))*q_cc(i_c)
           !
         enddo 
       enddo
       !
       QP_Sc(i_qp,:QP_dSc_steps)=QP_Sc(i_qp,:QP_dSc_steps) + &
&        P*((spin_occ-E%f(i_m,i_p,1))/(E%E(i_n,i_k,1)-E%E(i_m,i_p,1)+Photon_e+cI*QP_G_damp) +&
&        E%f(i_m,i_q,1)/(E%E(i_n,i_k,1)-E%E(i_m,i_p,1)+Photon_e+cI*QP_G_damp))/spin_occ /(8._SP*pi**2)
       !
     enddo B_loop
   enddo QP_loop
   !
   call live_timing(steps=1)
   !
 enddo Q_loop
 !
 call live_timing( )
 !
 do i_qp=1,qp%n_states
   write (*,*) i_qp,QP_Sc(i_qp,1)*HA2EV
 enddo
 stop
 !
end subroutine QED_sigma_c

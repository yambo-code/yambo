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
 use D_lattice,           ONLY:alat,a,Bose_Temp
 use R_lattice,           ONLY:qindx_S,bz_samp,nqbz,g_vec
 use QP_m,                ONLY:QP_t,QP_n_G_bands,QP_table,QP_G_damp,QP_Sc,QP_dSc_steps
 use vec_operate,         ONLY:v_norm
 use wave_func,           ONLY:WF
 use FFT_m,               ONLY:fft_size,fft_dim
 use interfaces,          ONLY:WF_load,PARALLEL_index
 use functions,           ONLY:Fermi_fnc,bose_f 
 use LIVE_t,              ONLY:live_timing
 use parallel_m,          ONLY:PP_redux_wait,PP_indexes,PP_indexes_reset,myid
 !
 implicit none
 !
 type(levels)              ::E
 type(bz_samp)             ::k,q
 type(QP_t)                ::qp
 !
 ! WorkSpace
 !
 integer                   ::i_q_bz,i_q,i_q_s,i_qp,i_k,i_n,i_p_bz,i_p_G0,i_m,i_p_s,i_p,&
&                            i_c,i_cp,i_wf_at_k,i_wf_at_p,i_k_bz,ir,ir1,ir2,ir3,&
                             i_wf_at_p_g
 real(SP)                  ::q_cc(3),k_cc(3),Photon_E,Photon_pol_vec(3,3),sumdot,bose,t_vec
 complex(SP)               ::XHI(3),I_wfk_wfp,Dipole_sum
 complex(SP), allocatable  ::WF_p(:,:),WF_p_rot(:,:),WF_der(:,:),WF_der_rot(:,:)
 real(SP),    allocatable  ::r_cell_cc(:,:)
 type(PP_indexes)          ::px
 !
 if (l_elel_corr) then
   call section('+','Correlation: Photon-mediated Self-energy')
 else
   call section('=','Correlation: Photon-mediated Self-energy')
 endif
 !
 call WF_load(WF  ,0,1,(/1,QP_n_G_bands(2)/),(/1,k%nibz/),space='R',title='-[Photon-El]Sigma_c/R space')
 !
 allocate(WF_p(fft_size,n_spinor),WF_p_rot(fft_size,n_spinor)) 
 allocate(r_cell_cc(3,fft_size))
 allocate(WF_der(3,fft_size),WF_der_rot(3,fft_size))
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
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/nqbz/))
 ! 
 call live_timing('Sigma_c',px%n_of_elements(myid+1))
 !
 Q_loop: do i_q_bz=1,nqbz 
   !
   if (.not.px%element_1D(i_q_bz)) cycle
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
   if (i_q==1) then
     call live_timing(steps=1)
     cycle
   endif
   !
   q_cc=q_cc/Photon_E
   bose=2._SP/(exp(Photon_E/Bose_Temp)-1._SP)
   !if (abs(Photon_E)>epsilon(1.)) then
   !  q_cc=q_cc/Photon_E
   !  if (abs(Photon_E)<=0.1_SP*Bose_Temp) bose=2._SP*Bose_Temp/Photon_E
   !  if (abs(Photon_E)> 0.1_SP*Bose_Temp) bose=2._SP/(exp(Photon_E/Bose_Temp)-1._SP)
   !else
   !  bose=2._SP*Bose_Temp/epsilon(1.)
   !  q_cc=q_cc/epsilon(1.)
   !endif
   !
   !QP_Sc(1,1)=(0.,0.)
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
       !
       ! WF%c(i_wf_at_k,:) = wf @ k in r-space -> O
       !
       call WF_apply_symm((/i_m, i_p, i_p_s, 1/), WF_p)
       !
       ! We now evaluate the derivative of (i_m,i_p,i_p_s)
       !
       WF_der=cZERO
       !
       call eval_gradient(WF_p,WF_der)
       !
       ! Now we multiply (i_m,i_p,i_p_s) and its gradient by exp(iG_0r)
       !
       do ir = 1, fft_size
         sumdot=0._SP
         do i_c=1, 3
           sumdot=sumdot + r_cell_cc(i_c,ir)*g_vec(i_p_G0,i_c)*2*pi/alat(i_c)
         enddo
         WF_p_rot(ir,1:n_spinor) = WF_p(ir,1:n_spinor)*exp(cI*sumdot)
         WF_der_rot(1:3,ir) = WF_der(1:3,ir)*exp(cI*sumdot)
       enddo
       !
       !<i_m,i_p,i_p_s|exp(-iG_0r)|i_n,i_k,i_k_s>
       !
       I_wfk_wfp=dot_product(WF_p_rot(:,1),WF%c(:,i_wf_at_k))
       !
       Dipole_sum=0.
       do i_cp=1,3
         !
         !\int dr (i_n,i_k,i_k_s)exp(-iG_0r)\nabla_{i_cp}(i_m,i_q,i_q_s)^*
         !
         XHI(i_cp) = dot_product(WF_der_rot(:,i_cp),WF%c(:,i_wf_at_k))
         !
         t_vec=k_cc(i_cp)-q_cc(i_cp)+g_vec(i_p_G0,i_cp)*2._SP*pi/alat(i_cp)
         !
         do i_c=1,3
           !
           Dipole_sum=Dipole_sum +    Photon_pol_vec(i_cp,i_c)*XHI(i_cp)*I_wfk_wfp*q_cc(i_c)
           !
           Dipole_sum=Dipole_sum - cI*Photon_pol_vec(i_cp,i_c)*t_vec*real(conjg(I_wfk_wfp)*I_wfk_wfp)*q_cc(i_c)
           !
!           Dipole_sum=Dipole_sum + &
!&            Photon_pol_vec(i_cp,i_c)*(&
!&                                      XHI(i_cp)*I_wfk_wfp - cI*(&
!&                                        k_cc(i_cp)-q_cc(i_cp)+g_vec(i_p_G0,i_cp)*2._SP*pi/alat(i_cp)&
!&                                                                )*real(conjg(I_wfk_wfp)*I_wfk_wfp)&
!&                                     )*q_cc(i_c)
         !
         enddo 
       enddo
       !
       QP_Sc(i_qp,:QP_dSc_steps)=QP_Sc(i_qp,:QP_dSc_steps) -&
&        cI*Dipole_sum*(&
&                       (spin_occ-E%f(i_m,i_p,1) + bose)/(E%E(i_n,i_k,1)-E%E(i_m,i_p,1)-Photon_E+cI*QP_G_damp) +&
&                       (E%f(i_m,i_p,1)+bose)/(E%E(i_n,i_k,1)-E%E(i_m,i_p,1)+Photon_E+cI*QP_G_damp)&
&                      )/spin_occ *pi/(2._SP)
       !
!       if(E%f(i_m,i_p,1) /= 0._SP .and. bose_f(Photon_E) /= 0._SP) then
!       write(*,*) E%f(i_m,i_p,1),bose,Photon_E,I_wfk_wfp!, 2._SP/(exp(Photon_E/Bose_Temp)-1._SP)!, E%f(i_m,i_p,1) + bose_f(Photon_e)
!       write(*,*) i_q_bz, i_qp, i_m, Dipole_sum!E%E(i_n,i_k,1),E%E(i_m,i_p,1),Photon_E!, E%E(i_n,i_k,1)-E%E(i_m,i_p,1)+Photon_E
!       endif
     enddo B_loop
   enddo QP_loop
   !
   call live_timing(steps=1)
   !
   !write (*,*) v_norm( q%ptbz(i_q_bz,:) ),real(QP_Sc(1,1))*HA2EV
   !
 enddo Q_loop
 !
 call live_timing( )
 !
 call PP_indexes_reset(px)
 !
 call PP_redux_wait(QP_Sc)
 !
! do i_qp=1,qp%n_states
!   write (*,*) i_qp,QP_Sc(i_qp,1)*HA2EV
! enddo
 !
end subroutine QED_sigma_c

!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): PM AM
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
 use units,               ONLY:SPEED_OF_LIGHT
 use drivers,             ONLY:l_elel_corr
 use electrons,           ONLY:levels,n_spinor
 use D_lattice,           ONLY:alat,a,DL_vol
 use R_lattice,           ONLY:qindx_S,bz_samp,nqbz,g_vec,RL_vol,RIM_n_rand_pts
 use QP_m,                ONLY:QP_t,QP_n_G_bands,QP_table,QP_Sc,QP_dSc_steps,QP_QED_ng
 use vec_operate,         ONLY:v_norm
 use wave_func,           ONLY:WF
 use FFT_m,               ONLY:fft_size,fft_dim
 use interfaces,          ONLY:PARALLEL_global_indexes,PARALLEL_WF_index,WF_load,PARALLEL_WF_distribute
 use functions,           ONLY:Fermi_fnc,bose_f 
 use LIVE_t,              ONLY:live_timing
 use parallel_m,          ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_QP,&
&                              PAR_IND_Q,PAR_COM_QP_A2A,PAR_IND_Q_ID,PAR_IND_QP_ID,&
&                              PAR_COM_QP_A2A
#if defined _TIMING
 use timing_m,            ONLY:timing
#endif
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
                             i_wf_at_p_g,timing_steps,i_g
 real(SP)                  ::q_photon(3),q_cc(3),p_cc(3),Photon_E,trans_matrix(3,3),sumdot,bose,t_vec(3),k_cc(3),E_diff
 complex(SP)               ::XHI_p(3),XHI_k(3),I_wfk_wfp,Dipole_sum,P_i_cp,P_i_c,Sc_dummy
 complex(SP), allocatable  ::WF_p(:,:),WF_p_rot(:,:),WF_der_p(:,:),WF_der_k(:,:),WF_der_p_rot(:,:)
 real(SP),    allocatable  ::r_cell_cc(:,:),q_weight(:)
 !
 if (l_elel_corr) then
   call section('+','Correlation: Photon-mediated Self-energy')
 else
   call section('=','Correlation: Photon-mediated Self-energy')
 endif
 !
#if defined _TIMING
 call timing('GW (Lamb)',OPR='start')
#endif
 !
 ! RIM 
 !=======================
 if (RIM_n_rand_pts==0) RIM_n_rand_pts=1000000
 call k_ibz2bz(q,'i',.TRUE.)
 allocate(q_weight(nqbz))
 call rim_spherical(nqbz,q%ptbz,q_weight,(3.*RL_vol/nqbz/4./pi)**(1./3.),1,.FALSE.)
 call k_ibz2bz(q,'d',.TRUE.)
 !
 ! PARALLEL distribution
 !=======================
 call PARALLEL_global_indexes(E,k,q,"Self_Energy")
 !
 call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_G_b,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_distribute(QP_index=PAR_IND_QP)
 call PARALLEL_WF_index( )
 !
 call WF_load(WF  ,0,1,(/1,QP_n_G_bands(2)/),(/1,k%nibz/),space='R',title='-[Photon-El]Sigma_c/R space')
 !
 allocate(WF_p(fft_size,n_spinor),WF_p_rot(fft_size,n_spinor)) 
 allocate(r_cell_cc(3,fft_size))
 allocate(WF_der_p(3,fft_size),WF_der_p_rot(3,fft_size))
 allocate(WF_der_k(3,fft_size))
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
 timing_steps=PAR_IND_QP%n_of_elements(PAR_IND_QP_ID+1)*&
&             count( PAR_IND_G_b%element_1D(QP_n_G_bands(1):QP_n_G_bands(2)) )*&
&             PAR_IND_Q%n_of_elements(PAR_IND_Q_ID+1)
 !
 call live_timing('Sigma_c',timing_steps)
 !
 Q_loop: do i_q_bz=1,nqbz 
   !
   i_q  =q%sstar(i_q_bz,1)
   i_q_s=q%sstar(i_q_bz,2)
   !
   if (.not.PAR_IND_Q%element_1D(i_q)) cycle
   !
   q_cc=q%ptbz(i_q_bz,:)
   !
   Photon_E=0._SP
   bose=0._SP
   !
   QP_loop: do i_qp=1,qp%n_states
     !
     if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
     !
     i_n  = QP_table(i_qp,1)
     i_k  = QP_table(i_qp,3)
     !
     i_k_bz=sum(k%nstar(:i_k-1))+1
     k_cc=q%ptbz(i_k_bz,:)
     !
     i_p_bz = qindx_S(i_k,i_q_bz,1)
     i_p_G0 = qindx_S(i_k,i_q_bz,2)
     p_cc=q%ptbz(i_p_bz,:)
     !
     i_p  =k%sstar(i_p_bz,1)
     i_p_s=k%sstar(i_p_bz,2)
     !
     B_loop: do i_m=QP_n_G_bands(1),QP_n_G_bands(2)
       !
       if (.not.PAR_IND_G_b%element_1D(i_m)) cycle
       !
       E_diff=E%E(i_n,i_k,1)-E%E(i_m,i_p,1)
       !
       if (abs(E_diff)<1.E-5) then
         if( i_q_s == 1) call live_timing(steps=1)
         cycle
       endif
       !
       i_wf_at_k=WF%index(i_n,i_k,1)
       i_wf_at_p=WF%index(i_m,i_p,1)
       !
       ! WF%c(i_wf_at_k,:) = wf @ k in r-space -> O
       !
       call WF_apply_symm((/i_m, i_p, i_p_s, 1/), WF_p)
       !
       ! We now evaluate the derivative of |i_m,i_p,i_p_s>
       ! and |i_m,i_k,i_k_s>
       !
       WF_der_p=cZERO
       WF_der_k=cZERO
       Sc_dummy=cZERO
       !
       call eval_Gradient(WF_p,WF_der_p)
       call eval_Gradient(WF%c(:,i_wf_at_k),WF_der_k)
       !
       G_loop: do i_g=1,QP_QED_ng
         !
         if (i_g==1.and.i_q==1) cycle
         !
         q_photon(:) = q_cc(:) + g_vec(i_g,:)*2._SP*pi/alat(:)
         !
         do i_c=1,3
           if (i_g==1) cycle
           do i_cp=i_c,3
             trans_matrix(i_c,i_cp)=-q_photon(i_c)*q_photon(i_cp)/v_norm(q_photon)**2
             trans_matrix(i_cp,i_c)=trans_matrix(i_c,i_cp)
           enddo
           trans_matrix(i_c,i_c)=1._SP-trans_matrix(i_c,i_c)
         enddo
         !
         Photon_E=SPEED_OF_LIGHT*v_norm(q_photon)
         !
         ! Now we multiply |i_m,i_p,i_p_s> and its gradient by exp(iG_0r)
         !
         do ir = 1, fft_size
           sumdot=0._SP
           do i_c=1, 3
             sumdot=sumdot+r_cell_cc(i_c,ir)*(g_vec(i_p_G0,i_c) - g_vec(i_g,i_c))*2*pi/alat(i_c)
           enddo
           WF_p_rot(ir,1:n_spinor) = WF_p(ir,1:n_spinor)*exp(cI*sumdot)
           WF_der_p_rot(1:3,ir) = WF_der_p(1:3,ir)*exp(cI*sumdot)
         enddo
         !
         ! <i_m,i_p,i_p_s|exp[i(G-G_0)r]|i_n,i_k,i_k_s>
         !
         I_wfk_wfp=dot_product(WF_p_rot(:,1),WF%c(:,i_wf_at_k))
         !
         Dipole_sum=0._SP
         !
         do i_cp=1,3
           !
           ! <i_n,i_k,i_k_s|exp[i(G0-G)r]\nabla_{i_cp}|i_m,i_p,i_p_s>
           !
           XHI_p(i_cp) = dot_product(WF%c(1:fft_size,i_wf_at_k),WF_der_p_rot(i_cp,1:fft_size))
           !
           ! <i_m,i_p,i_p_s|exp[-i(G-G0)r]\nabla_{i_cp}|i_n,i_k,i_k_s>
           !
           XHI_k(i_cp) = dot_product(WF_p_rot(:,1),WF_der_k(i_cp,:))
           !
           ! q_cc{i_cp} + G0_{i_cp}
           !
           t_vec=q_cc(i_cp)+g_vec(i_p_G0,i_cp)*2._SP*pi/alat(i_cp)
           !
         enddo
         !
         do i_cp=1,3
           !
           P_i_cp=XHI_k(i_cp)+conjg(XHI_p(i_cp))-cI*t_vec(i_cp)*I_wfk_wfp
           !
           do i_c=1,3
             !
             P_i_c=XHI_k(i_c)+conjg(XHI_p(i_c))-cI*t_vec(i_c)*I_wfk_wfp
             !
             if (i_g==1) then
               !
               if (i_c==i_cp) Dipole_sum = Dipole_sum+P_i_cp*conjg(P_i_c)/DL_vol*q_weight(i_q_bz)
               !
             else
               !
               Dipole_sum = Dipole_sum+P_i_cp*conjg(P_i_c)*trans_matrix(i_c,i_cp)/Photon_E/DL_vol/float(nqbz)
               !
             endif
             !
           enddo 
         enddo
         !
         Sc_dummy=Sc_dummy+Dipole_sum/(E_diff-Photon_E)*pi/2._SP
         !
       enddo G_loop
       !
       QP_Sc(i_qp,:QP_dSc_steps)=QP_Sc(i_qp,:QP_dSc_steps)+Sc_dummy
       !
       if ( i_q_s == 1) call live_timing(steps=1)
       !
     enddo B_loop
     !
   enddo QP_loop
   !
 enddo Q_loop
 !
 call live_timing( )
 !
 call PP_redux_wait(QP_Sc)
 !
#if defined _TIMING
 call timing('GW (Lamb)',OPR='stop')
#endif
 !
end subroutine QED_sigma_c


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
 use electrons,           ONLY:levels,spin
 use D_lattice,           ONLY:alat,a,DL_vol
 use R_lattice,           ONLY:qindx_S,bz_samp,nqbz,g_vec,RL_vol,RIM_n_rand_pts
 use QP_m,                ONLY:QP_t,QP_n_G_bands,QP_table,QP_Sc,QP_dSc_steps,QP_QED_ng,QP_nb
 use vec_operate,         ONLY:v_norm
 use collision,           ONLY:ggwinfo,collision_reset,scatterBamp_alloc,scatterBamp_free
 use wave_func,           ONLY:WF
 use interfaces,          ONLY:PARALLEL_global_indexes,PARALLEL_WF_index,WF_load,PARALLEL_WF_distribute
 use functions,           ONLY:Fermi_fnc,bose_f 
 use LIVE_t,              ONLY:live_timing
 use parallel_m,          ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_QP,&
&                              PAR_IND_Q,PAR_COM_QP_A2A,PAR_nQ,PAR_nQP,PAR_nG_bands,&
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
 integer                   ::i_q_bz,i_q,i_q_s,i_qp,i_k,i_n,i_p_G0,i_m,i_p,i_c,i_cp,i_wf_at_p,&
                             i_wf_at_p_g,timing_steps,i_g
 complex(SP)               ::XHI_p(3),XHI_k(3),I_wfk_wfp,Dipole_sum,P_i_cp,P_i_c,Sc_dummy
 real(SP)                  ::q_photon(3),q_cc(3),Photon_E,trans_matrix(3,3),bose,t_vec(3),E_diff
 real(SP),    allocatable  ::q_weight(:)
 type(ggwinfo)             ::isc,isc_grad_k,isc_grad_p
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
 ! Basic stuff
 !============
 !
 call collision_reset(isc)
 call collision_reset(isc_grad_k) 
 call collision_reset(isc_grad_p) 
 !
 ! RIM 
 !=====
 if (RIM_n_rand_pts==0) RIM_n_rand_pts=1000000
 call k_ibz2bz(q,'i',.TRUE.)
 allocate(q_weight(nqbz))
 call rim_spherical(nqbz,q%ptbz,q_weight,(3.*RL_vol/nqbz/4./pi)**(1./3.),1,.FALSE.)
 call k_ibz2bz(q,'c',.TRUE.)
 !
 ! PARALLEL distribution
 !=======================
 call PARALLEL_global_indexes(E,k,q,"Self_Energy")
 !
 call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_G_b,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_distribute(QP_index=PAR_IND_QP)
 call PARALLEL_WF_index( )
 !
 ! WF loading and further allocation
 !===================================
 !
 call WF_load(WF,QP_QED_ng,maxval(qindx_S(:,:,2)),(/1,max(QP_nb,QP_n_G_bands(2))/),&
&             (/1,k%nibz/),space='R',title='-[Photon-El]Sigma_c/R space')
 !
 call scatterBamp_alloc(isc)
 ! isc_grad_k and isc_grad_p need not to be scatterBamp-allcoated
 ! since scatterBamp_using_the_gradient allocates its own workspace
 allocate(isc%rhotw(QP_QED_ng),isc_grad_k%rhotw(3*QP_QED_ng),isc_grad_p%rhotw(3*QP_QED_ng))
 isc%ngrho       =QP_QED_ng
 isc_grad_k%ngrho=QP_QED_ng
 isc_grad_p%ngrho=QP_QED_ng
 !
 ! Loop
 !======
 !
 call live_timing('Sigma_c',PAR_nQP*PAR_nG_bands*PAR_nQ)
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
     i_n    = QP_table(i_qp,1)
     i_k    = QP_table(i_qp,3)
     i_p_G0 = qindx_S(i_k,i_q_bz,2)
     i_p    =k%sstar(qindx_S(i_k,i_q_bz,1),1)
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
       Sc_dummy=cZERO
       !
       ! m,k-q   n,k
       ! --<--.-<--
       !      | 
       !      |
       !     \|/ q,G,\alpha
       !      |
       !      
       ! n   =QP_table(i_qp,1)
       ! k   =QP_table(i_qp,3)
       !
       isc%is=(/QP_table(i_qp,1),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
       isc%os=(/i_m,k%sstar(qindx_S(isc%is(2),i_q_bz,1),:),spin(QP_table(i_qp,:))/)
       isc%qs=(/qindx_S(QP_table(i_qp,3),i_q_bz,2),q%sstar(i_q_bz,1),q%sstar(i_q_bz,2)/)
       !
       isc_grad_k%is=isc%is
       isc_grad_k%os=isc%os
       isc_grad_k%qs=isc%qs
       !
       isc_grad_p%is=isc%is
       isc_grad_p%os=isc%os
       isc_grad_p%qs=isc%qs
       !
       call scatterBamp(isc)
       call scatterBamp_using_the_gradient(isc_grad_k,"i")
       call scatterBamp_using_the_gradient(isc_grad_p,"o")
       !
       ! q_cc{i_cp} + G0_{i_cp}
       !
       do i_cp=1,3
         t_vec(i_cp)=q_cc(i_cp)+g_vec(i_p_G0,i_cp)*2._SP*pi/alat(i_cp)
       enddo
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
         I_wfk_wfp=isc%rhotw(i_g)
         !
         do i_cp=1,3
           XHI_k(i_cp)=isc_grad_k%rhotw(QP_QED_ng*(i_cp-1)+i_g)
           XHI_p(i_cp)=isc_grad_p%rhotw(QP_QED_ng*(i_cp-1)+i_g)
         enddo
         !
         Dipole_sum=0._SP
         !
         do i_cp=1,3
           !
           P_i_cp=XHI_k(i_cp)-XHI_p(i_cp)-cI*t_vec(i_cp)*I_wfk_wfp
           !
           do i_c=1,3
             !
             P_i_c=XHI_k(i_c)-XHI_p(i_c)-cI*t_vec(i_c)*I_wfk_wfp
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
 ! Clean-UP
 !
 deallocate(isc%rhotw,isc_grad_k%rhotw,isc_grad_p%rhotw)
 call scatterBamp_free(isc)
 !
#if defined _TIMING
 call timing('GW (Lamb)',OPR='stop')
#endif
 !
end subroutine QED_sigma_c


!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use pars,                ONLY:SP,pi,cI,rZERO
 use units,               ONLY:SPEED_OF_LIGHT,HA2EV
 use drivers,             ONLY:l_elel_corr,l_use_collisions,l_elph_corr,l_elphoton_scatt
 use electrons,           ONLY:levels,spin_occ,n_met_bands
 use D_lattice,           ONLY:DL_vol
 use R_lattice,           ONLY:qindx_S,bz_samp,nqbz,RL_vol,RIM_n_rand_pts
 use QP_m,                ONLY:QP_t,QP_n_G_bands,QP_Sc,QP_QED_ng,QP_nb,QP_states_simmetrize,&
&                              QP_dSc,QP_time_order_sign,QP_table,QP_n_states
 use IO_m,                ONLY:io_COLLs
 use collision_el,        ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc,&
&                              elemental_collision_copy
 use collision_ext,       ONLY:P_COLL_element,COLL_bands,P_collisions
 use wave_func,           ONLY:WF
 use interfaces,          ONLY:PARALLEL_global_indexes,PARALLEL_WF_index,WF_load,WF_free,PARALLEL_WF_distribute
 use COLL_interfaces,     ONLY:SCATTERING_GW_kinematics
 use LIVE_t,              ONLY:live_timing
 use vec_operate,         ONLY:v_norm
 use parallel_m,          ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_QP,&
&                              PAR_IND_Q,PAR_nQ,PAR_nQP
 use functions,           ONLY:theta_function
 use com,                 ONLY:warning
 use timing_m,            ONLY:timing
 !
#include<memory.h>
 !
 type(levels)              ::E
 type(bz_samp)             ::k,q
 type(QP_t)                ::qp
 !
 ! WorkSpace
 !
 integer                   ::i_q_bz,i_q,i_q_s,i_qp,i_G0,i_c,i_cp,i_g,i_m,i_m_wf_load,PAR_bands(2),n_lt_steps,i_qp_mem
 complex(SP)               ::P(3)
 real(SP)                  ::Photon_E,T_matrix(3,3),T_vec(3),E_diff,Photon_Gamma_E,f_k,f_k_m_q,&
&                            P_square_times_tau(QP_QED_ng),Sc_re,Sc_im,beta,R0
 real(SP),    allocatable  ::q_weight_power_1(:),q_weight_power_2(:)
 type(elemental_collision) ::I,I_grad_k,I_grad_p
 logical                   ::l_zero_delta_E,l_SI_term
 !
 ! Prefactors
 !
 real(SP)                  ::C_G_finite,C_G_eq_0_finite_E,C_G_eq_0_null_E,FRACTION(2)
 !
 if (l_elel_corr.or.l_elph_corr) then
   call section('+','Correlation: Photon-mediated Self-energy and Radiative Lifetimes')
 else
   call section('=','Correlation: Photon-mediated Self-energy and Radiative Lifetimes')
 endif
 !
 call timing('GW (Lamb)',OPR='start')
 !
 ! PARALLEL distribution
 !=======================
 call PARALLEL_global_indexes(E,k,q,"Self_Energy")
 !
 call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_G_b,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_distribute(QP_index=PAR_IND_QP)
 call PARALLEL_WF_index( )
 !
 ! RIM 
 !=====
 if (RIM_n_rand_pts==0) RIM_n_rand_pts=1000000
 !
 call k_ibz2bz(q,'i',.TRUE.)
 !
 YAMBO_ALLOC(q_weight_power_1,(nqbz))
 YAMBO_ALLOC(q_weight_power_2,(nqbz))
 call rim_spherical(nqbz,q%ptbz,q_weight_power_1,(3.*RL_vol/nqbz/4./pi)**(1./3.),1,.FALSE.)
 call rim_spherical(nqbz,q%ptbz,q_weight_power_2,(3.*RL_vol/nqbz/4./pi)**(1./3.),2,.FALSE.)
 !
 R0 = (3.*RL_vol/nqbz/4./pi)**(1./3.) ! the radius for the sum at Q=0
 !
 call k_ibz2bz(q,'c',.TRUE.)
 !
 ! COLLISIONS
 !============
 !
 COLL_bands      =QP_n_G_bands
 l_elphoton_scatt=.TRUE.
 !
 if (io_COLLs) then
   call COLLISIONS_load(.TRUE.)
   if (l_use_collisions) then
     call COLLISIONS_load(.FALSE.)
     if (QP_n_states/=P_collisions%N) then
       call warning("COLLISIONS do not fit the QP states. Switched off")
       l_use_collisions=.FALSE.
     endif
   endif
 endif
 !
 ! BASIC WF allocation (FILLED bands)
 !====================================
 !
 call LOCAL_WF_and_COLLISIONS_allocation( QP_QED_ng , n_met_bands )
 !
 ! Loop
 !======
 !
 !The correction is evaluated through
 !
 ! \delta_{nk} = \sum_{m,G,q} prefactor/\omega_{q+G}*R^{qG}_{nmk}/(e_{nk} - e_{mk-q} - \omega_{q+G})
 !
 !We distinguish 3 cases: 
 !
 ! i)   G !=0, prefactor=pi/2/DL_vol/Nq
 ! ii)  G=0 and e_{nk} - e_{mk-q} !=0, prefactor= pi/2/c/DL_vol*RIM(q,power=1) 
 ! iii) G=0 and e_{nk} - e_{mk-q}  =0, prefactor= -pi/2/c^2/DL_vol*RIM(q,power=2)
 ! iv)  G=0 and q=0, prefactor= R0^2/12/pi/c
 !
 ! Band index for Empty bands allocation 
 !=======================================
 !
 i_m_wf_load=-1
 if (PAR_IND_G_b%element_1D(n_met_bands+1)) i_m_wf_load=n_met_bands+1
 do i_m=n_met_bands+1,QP_n_G_bands(2)
   if (PAR_IND_G_b%element_1D(i_m).and.i_m_wf_load<0) i_m_wf_load=i_m
 enddo
 PAR_bands(1)=count(PAR_IND_G_b%element_1D(:n_met_bands  ))
 PAR_bands(2)=count(PAR_IND_G_b%element_1D(n_met_bands+1:))
 !
 n_lt_steps=PAR_nQP*PAR_bands(1)*PAR_nQ
 !
 if (n_lt_steps>0) call live_timing('Sigma_c [FILLED bands]',n_lt_steps)
 !
 B_loop: do i_m=QP_n_G_bands(1),QP_n_G_bands(2)
   !
   if (.not.PAR_IND_G_b%element_1D(i_m)) cycle
   !
   if (i_m==i_m_wf_load.and.PAR_bands(2)>0) then
     !
     ! BASIC WF allocation (FILLED bands)
     !====================================
     !
     QP_QED_ng=1
     !
     call LOCAL_WF_and_COLLISIONS_allocation( 1 , QP_n_G_bands(2))
     !
     call live_timing('Sigma_c  [EMPTY bands]',PAR_nQP*PAR_bands(2)*PAR_nQ)
     !
   endif
   !
   Q_loop: do i_q_bz=1,nqbz 
     !
     i_q  =q%sstar(i_q_bz,1)
     i_q_s=q%sstar(i_q_bz,2)
     !
     if (.not.PAR_IND_Q%element_1D(i_q)) cycle
     !
     Photon_E      =0._SP
     Photon_Gamma_E=SPEED_OF_LIGHT*v_norm(q%ptbz(i_q_bz,:))
     !
     QP_loop: do i_qp=1,qp%n_states
       !
       if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
       !
       call SCATTERING_GW_kinematics(i_q_bz,i_m,I,k,q,QP_table(i_qp,:),E=E,E_diff=E_diff,G0=i_G0)
       !
       if (Photon_Gamma_E>1.E-5) then
         l_zero_delta_E=abs(E_diff)<Photon_Gamma_E/1000.
       else
         l_zero_delta_E=abs(E_diff)<1.E-5
       endif
       !
       l_SI_term     =l_zero_delta_E.and.i_q_bz==1
       !
       if (l_SI_term) then  
         call live_timing(steps=1)
         cycle
       endif
       !
       ! Collisions ...
       !
       if (l_use_collisions) then
         !
         i_qp_mem=P_collisions%PAR_map(i_qp)
         !
         P_square_times_tau(:)=P_COLL_element(i_qp_mem)%v3_r(:,i_m,i_q_bz)
         !
       else
         !
         call elemental_collision_copy(I,I_grad_k)
         call elemental_collision_copy(I,I_grad_p)
         !
         call scatter_Bamp(I)
         call scatter_Bamp_using_the_gradient(I_grad_k,"i")
         call scatter_Bamp_using_the_gradient(I_grad_p,"o")
         !
       endif
       !
       ! Occupations ...
       !
       f_k    =E%f(I%is(1),I%is(2),I%is(4))/spin_occ
       f_k_m_q=E%f(I%os(1),I%os(2),I%os(4))/spin_occ
       !
       ! G=0 
       !=====
       !
       call EVAL_pre_factors(.TRUE.)
       call EVAL_the_G_loop(1,1)
       !
       ! G/=0 
       !======
       !
       if (f_k_m_q>1.E-5_SP.and.QP_QED_ng>1) then
         !
         call EVAL_pre_factors(.FALSE.)
         call EVAL_the_G_loop(2,QP_QED_ng)
         !
       endif
       !
       if ( i_q_s == 1 ) call live_timing(steps=1)
       !
     enddo QP_loop
     !
   enddo Q_loop
   !
 enddo B_loop
 !
 call live_timing( )
 !
 call PP_redux_wait(QP_Sc)
 call PP_redux_wait(QP_dSc)
 !
 ! Symmetrize
 !
 call QP_states_simmetrize(E,V_complex=QP_Sc(:,1))
 call QP_states_simmetrize(E,V_complex=QP_dSc(:,1))
 !
 ! Clean-UP
 !
 YAMBO_FREE(q_weight_power_1)
 YAMBO_FREE(q_weight_power_2)
 !
 if (l_use_collisions) then
   !
   call COLLISIONS_alloc_and_free("Momentum","v3","clean",0)
   !
 else
   !
   call WF_free(WF)
   !
   call elemental_collision_free(I)
   call elemental_collision_free(I_grad_k) 
   call elemental_collision_free(I_grad_p) 
   !
 endif
 !
 call timing('GW (Lamb)',OPR='stop')
 !
 contains
   !
   subroutine EVAL_the_G_loop(iG_start,iG_end)
     !
     integer :: iG_start,iG_end
     !
     if (.not.l_use_collisions) P_square_times_tau=rZERO
     Sc_re             =rZERO
     Sc_im             =rZERO
     beta              =rZERO
     !
     ! We need the objects
     !
     !  i) <nk|exp(iqr)\grad|mk-q>
     !  ii) [<mk-q|exp(-iqr)\grad|nk>]*
     !  i) gives an i(k-q-G0) factor and ii) a -ik so we need at the end q_cc{i_cp} + G0_{i_cp}
     !
     G_loop: do i_g=iG_start,iG_end
       !
       if (iG_start==iG_end.and.iG_start==1) then
         !
         call SCATTERING_QED_transverse_matrix(I%is(2),i_q_bz,i_g,i_G0,k,q,T_matrix,T_vec,photon_E)
         !
         if(i_q_bz==1.and.i_g==1) then
           FRACTION(1)=1._SP/E_diff
           FRACTION(2)=1._SP/E_diff**2
         else  
           FRACTION(1)=(1._SP-f_k_m_q)/(E_diff-Photon_E)+f_k_m_q/(E_diff+Photon_E)
           FRACTION(2)=(1._SP-f_k_m_q)/(E_diff-Photon_E)**2+f_k_m_q/(E_diff+Photon_E)**2
         endif
         !
       else
         !
         call SCATTERING_QED_transverse_matrix(I%is(2),i_q_bz,i_g,i_G0,k,q,T_matrix,T_vec,photon_E)
         !
         FRACTION(1)=f_k_m_q/Photon_E*2._SP
         FRACTION(2)=f_k_m_q/Photon_E**2*2._SP
         !
       endif
       !
       if (.not.l_use_collisions) then
         !
         ! COLLISION SPECIFIC SECTION
         !=============================================================================================================================
         do i_c=1,3
           P(i_c)=I_grad_k%rhotw(QP_QED_ng*(i_c-1)+i_g)-I_grad_p%rhotw(QP_QED_ng*(i_c-1)+i_g)-cI*T_vec(i_c)*I%rhotw(i_g)
         enddo
         !
         do i_cp=1,3
           do i_c=1,3
             P_square_times_tau(i_g)=P_square_times_tau(i_g)+P(i_c)*T_matrix(i_c,i_cp)*conjg(P(i_cp))
           enddo
         enddo
         !=============================================================================================================================
       endif
       !
       ! Real-Part of the Self-Energy
       !
       if (.not.l_SI_term) then
         !
         if (i_g==1) then
           if (l_zero_delta_E) then
             Sc_re=Sc_re+P_square_times_tau(i_g)*C_G_eq_0_null_E*(2._SP*f_k_m_q-1)
           else
             Sc_re=Sc_re+P_square_times_tau(i_g)*C_G_eq_0_finite_E*FRACTION(1)
             beta =beta -P_square_times_tau(i_g)*C_G_eq_0_finite_E*FRACTION(2)
           endif
         else
           Sc_re=Sc_re+P_square_times_tau(i_g)*C_G_finite/Photon_E*FRACTION(1)
           beta =beta -P_square_times_tau(i_g)*C_G_finite/Photon_E*FRACTION(2)
         endif
         !
       endif
       !
       ! Imaginary-Part of the Self-Energy.
       !
       if (.not.(i_q_bz==1.and.i_g==1)) cycle
       !
       Sc_im=Sc_im+P_square_times_tau(1)/3._SP/SPEED_OF_LIGHT**2*abs(E_diff)*&
&                  ( QP_time_order_sign*(1._SP-f_k_m_q)*theta_function( E_diff,0.01/HA2EV,0) +&
&                                              f_k_m_q *theta_function(-E_diff,0.01/HA2EV,0) )
  
       !
     enddo G_loop
     !
     QP_Sc(i_qp,1) =QP_Sc(i_qp,1)  +Sc_re+cI*abs(Sc_im)
     !
     ! Let's stick to the OMS for the moment
     !
     QP_dSc(i_qp,1)=QP_dSc(i_qp,1) +beta
     !
   end subroutine EVAL_the_G_loop
   !
   subroutine LOCAL_WF_and_COLLISIONS_allocation( NG , NB )
     !
     integer :: NG,NB
     !
     if (l_use_collisions) return
     !
     ! Elemental scattering allocation
     !=================================
     !
     call elemental_collision_free(I)
     call elemental_collision_free(I_grad_k) 
     call elemental_collision_free(I_grad_p) 
     !
     ! WF loading and further allocation
     !===================================
     !
     call WF_load(WF,NG,maxval(qindx_S(:,:,2)),(/1,max(QP_nb,NB)/),&
&                 (/1,k%nibz/),space='R',title='-[Photon-El]Sigma_c/R space',&
&                 impose_free_and_alloc=.TRUE.)
     !
     ! I_grad_k and I_grad_p need not to be scatterBamp-allocated
     ! since scatterBamp_using_the_gradient allocates its own workspace
     !
     call elemental_collision_alloc(I,NG=NG,TITLE="GW_QED")
     call elemental_collision_alloc(I_grad_k,NG=NG,TITLE="GW_QED",GRADIENT=.TRUE.)
     call elemental_collision_alloc(I_grad_p,NG=NG,TITLE="GW_QED",GRADIENT=.TRUE.)
     !
   end subroutine
   !
   subroutine EVAL_pre_factors( NULL_G )
     logical :: NULL_G
     if (NULL_G) then
       !prefactor for case iii)
       C_G_eq_0_null_E    =-pi*q_weight_power_2(i_q_bz)/DL_vol/SPEED_OF_LIGHT**2/2._SP
       if (i_q_bz==1) then
         !prefactor for case iv)
         C_G_eq_0_finite_E=R0**2/12._SP/pi/SPEED_OF_LIGHT
       else
         !prefactor for case ii)
         C_G_eq_0_finite_E=pi*q_weight_power_1(i_q_bz)/DL_vol/SPEED_OF_LIGHT/2._SP
       endif
       !prefactor for case i)
       C_G_finite         = 0._SP !pi/2._SP/DL_vol/float(nqbz)
     else
       C_G_eq_0_null_E  =0._SP
       C_G_eq_0_finite_E=0._SP
       C_G_finite       =pi/2._SP/DL_vol/float(nqbz)
     endif
   end subroutine EVAL_pre_factors
   !
end subroutine QED_sigma_c

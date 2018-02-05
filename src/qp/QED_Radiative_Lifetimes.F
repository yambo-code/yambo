!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine QED_Radiative_Lifetimes(E,k,q,qp,X)
 !
 use drivers,             ONLY:l_elel_corr,l_elph_corr
 use units,               ONLY:HA2EV,SPEED_OF_LIGHT
 use pars,                ONLY:SP,rZERO,cI
 use QP_m,                ONLY:QP_time_order_sign,QP_table,QP_t,QP_n_G_bands,QP_Sc,QP_states_simmetrize
 use IO_m,                ONLY:io_DIP
 use functions,           ONLY:theta_function
 use electrons,           ONLY:levels,spin,spin_occ
 use R_lattice,           ONLY:bz_samp
 use parallel_m,          ONLY:PAR_IND_QP,PAR_IND_G_b,PAR_IND_DIPk_ibz,PAR_nG_bands,PAR_nQP,PP_redux_wait,&
&                              PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X,PAR_DIPk_ibz_index
 use X_m,                 ONLY:DIP_P,X_t
 use wave_func,           ONLY:wf_ng
 use LIVE_t,              ONLY:live_timing
 use interfaces,          ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute
#if defined _TIMING
 use timing_m,            ONLY:timing
#endif
 !
 implicit none
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(QP_t)    ::qp
 type(X_t)     ::X
 !
 ! Work Space
 !
 integer     :: i_k,i_n,i_spin,i_m,i_qp,i_k_mem
 real(SP)    :: E_diff,f_m,Sc_im,P_square,pre_factor
 !
 if (l_elel_corr.or.l_elph_corr) then
   call section('+','Correlation: Radiative Lifetimes')
 else
   call section('=','Correlation: Radiative Lifetimes')
 endif
 !
#if defined _TIMING
 call timing('Radiative Lifetimes',OPR='start')
#endif
 !
 ! PARALLEL distribution
 !=======================
 !
 call PARALLEL_global_indexes(E,k,q,"Self_Energy",X=X)
 call PARALLEL_WF_distribute(K_index=PAR_IND_DIPk_ibz,&
&                            B_index=PAR_IND_CON_BANDS_X(X%whoami),&
&                            Bp_index=PAR_IND_VAL_BANDS_X(X%whoami),&
&                            CLEAN_UP=.TRUE.)
 !
 ! Dipoles
 !=========
 X%ib  =QP_n_G_bands
 X%ng  =wf_ng
 io_DIP=.FALSE.
 !
 call DIPOLE_driver(E,k,X,X%q0)
 !
 ! Loop
 !======
 !
 call live_timing('Radiative Lifetimes',PAR_nQP*PAR_nG_bands)
 !
 QP_loop: do i_qp=1,qp%n_states
   !
   Sc_im=rZERO
   !
   if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
   !
   i_n     = QP_table(i_qp,1)
   i_k     = QP_table(i_qp,3)
   i_spin  = spin(QP_table(i_qp,:))
   i_k_mem = PAR_DIPk_ibz_index(i_k)
   !
   B_loop: do i_m=QP_n_G_bands(1),QP_n_G_bands(2)
     !
     if (.not.PAR_IND_G_b%element_1D(i_m)) cycle
     !
     E_diff =E%E(i_n,i_k,i_spin)-E%E(i_m,i_k,i_spin)
     f_m    =E%f(i_m,i_k,i_spin)/spin_occ
     !
     ! The (4) prefactor comes from the definition of the \Pi operator in PRB 93, 155109.
     ! Indeed the e.m. interaction is written in terms of \Pi and P (that is not the m.e. of the gradient). 
     !
     ! P_nmk = i [p_nmk+(p_mnk)^*]
     !
     ! with p the quantity calculated by yambo in DIP_P
     !
     P_square=4.*(abs(DIP_P(1,i_m,i_n,i_k_mem,i_spin))**2.+&
&                 abs(DIP_P(2,i_m,i_n,i_k_mem,i_spin))**2.+&
&                 abs(DIP_P(3,i_m,i_n,i_k_mem,i_spin))**2)
     !
     pre_factor=P_square/3._SP/SPEED_OF_LIGHT**2*abs(E_diff)
     !
     Sc_im=Sc_im+pre_factor*&
&                ( QP_time_order_sign*(1._SP-f_m)*theta_function( E_diff,0.01/HA2EV,0) +&
&                                            f_m *theta_function(-E_diff,0.01/HA2EV,0) )
     !
     call live_timing(steps=1)
     !
   enddo B_loop
   !
   QP_Sc(i_qp,1)=cI*abs(Sc_im)
   !
 enddo QP_loop
 !
 call live_timing( )
 !
 call PP_redux_wait(QP_Sc)
 !
 ! Symmetrize
 !
 call QP_states_simmetrize(E,V_complex=QP_Sc(:,1))
 !
 ! Transfer to qp% components (assuming OMS approximation)
 !
 qp%E(:)=QP_sc(:,1)
 !
#if defined _TIMING
 call timing('Radiative Lifetimes',OPR='stop')
#endif
 !
end subroutine QED_Radiative_Lifetimes


!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QED_Radiative_Lifetimes(E,k,q,qp,X,Dip)
 !
 use units,               ONLY:HA2EV,SPEED_OF_LIGHT
 use pars,                ONLY:SP,rZERO,cI
 use QP_m,                ONLY:QP_time_order_sign,QP_table,QP_t,QP_n_G_bands,QP_Sc,QP_states_simmetrize
 use functions,           ONLY:theta_function
 use electrons,           ONLY:levels,spin,spin_occ
 use R_lattice,           ONLY:bz_samp
 use parallel_int,        ONLY:PP_redux_wait,PARALLEL_global_indexes
 use parallel_m,          ONLY:PAR_IND_QP,PAR_IND_G_b,PAR_nG_bands,PAR_nQP,PAR_DIPk_ibz_index
 use DIPOLES,             ONLY:DIP_P,DIPOLE_t
 use X_m,                 ONLY:X_t
 use LIVE_t,              ONLY:live_timing
 use timing_m,            ONLY:timing
 !
 implicit none
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(QP_t)    ::qp
 type(X_t)     ::X
 type(DIPOLE_t)::Dip
 !
 ! Work Space
 !
 integer     :: i_k,i_n,i_spin,i_m,i_qp,i_k_mem,io_err
 real(SP)    :: E_diff,f_m,Sc_im,P_square,pre_factor
 !
 call section('+','Correlation: Radiative Lifetimes')
 !
 call timing('Radiative Lifetimes',OPR='start')
 !
 ! PARALLEL distribution
 !=======================
 !
 call PARALLEL_global_indexes(E,k,q,"Self_Energy",X=X)
 !
 ! Dipoles
 !=========
 !
 call DIPOLE_dimensions(E,Dip,QP_n_G_bands,(/0._SP,0._SP,0._SP/))
 call DIPOLE_IO(k,E,Dip,'read ',io_err,'QED')
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
 call QP_states_simmetrize(E,what="QED Sc",V_complex=QP_Sc(:,1),warn_me=.TRUE.)
 !
 ! Transfer to qp% components (assuming OMS approximation)
 !
 qp%E(:)=QP_sc(:,1)
 !
 call timing('Radiative Lifetimes',OPR='stop')
 !
end subroutine QED_Radiative_Lifetimes


!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM, DS
!
subroutine OPTICS_driver(Xen,Xk,q,wv,X,Dip)
 ! 
 ! Optics driver
 !
 ! Calculates dielectric function for a generic q vector.
 ! 
 use pars,          ONLY:SP,cZERO,pi
 use drivers,       ONLY:l_rpa_IP,l_bs_fxc,l_alda_fxc,l_pf_fxc,l_col_cut
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use DIPOLES,       ONLY:DIPOLE_t
 use X_m,           ONLY:X_t,X_par,X_fxc,X_Double_Grid_setup,&
&                        use_X_DbGd,X_par_lower_triangle,X_OUTPUT_messages,&
&                        X_DbGd_nkpt_at_q,X_DbGd_nkpt_at_q_table,&
&                        i_G_shift,Q_plus_G_sq_modulus
 use parallel_int,  ONLY:PP_wait,PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_index,PARALLEL_WF_distribute
 use parallel_m,    ONLY:PAR_IND_Q_ibz,PAR_COM_Q_A2A,PAR_COM_RL_INDEX,PAR_IND_CON_BANDS_X,&
&                        PAR_IND_Xk_ibz,PAR_IND_VAL_BANDS_X
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use TDDFT,         ONLY:F_xc_gspace,FXC_K_diagonal
 use interfaces,    ONLY:WF_load,WF_free,WF_load
 use matrix,        ONLY:MATRIX_reset
 !
#include<memory.h>
 !
 type(levels)  :: Xen
 type(bz_samp) :: Xk,q
 type(X_t)     :: X
 type(DIPOLE_t):: Dip
 type(w_samp)  :: wv 
 ! 
 ! Work Space
 !
 integer          :: iq
 complex(SP)      :: fac,spectra(wv%n_freqs,2)
 logical          :: l_skip_non_int_eps,Macroscopic_CPU
 !
 integer,external :: OPTICS_select_q_and_G
 !
 call section('*','Optics')
 !
 ! Basic Checks
 call X_pre_setup(Xen,Xk,X)
 !
 allocate(X_par(1))
 !
 do iq=X%iq(1),X%iq(2)
   !
   ! PARALLEL indexes
   !==================
   call PARALLEL_global_indexes(Xen,Xk,q,"Response_G_space",X=X)
   !
   ! WF distribution 
   !=================
   call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_CON_BANDS_X(X%whoami),&
&                              Bp_index=PAR_IND_VAL_BANDS_X(X%whoami),CLEAN_UP=.TRUE.)
   !
   call PARALLEL_WF_index(COMM=PAR_COM_Q_A2A)
   !
   if (WF_buffered_IO) call WF_load(WF_buffer,0,1,X%ib,(/1,Xk%nibz/),space='B',title='-BUFFER')
   !
   ! TDDFT setup
   call TDDFT_do_X_W_typs(iq,X,wv)
   !
   ! DbGd setup
   call X_Double_Grid_setup(iq,Xk,Xk%FGbz)
   !
   ! ALDA (only once) 
   if (l_alda_fxc.and..not.allocated(F_xc_gspace)) then
     !
     call TDDFT_ALDA_G_space(Xen,Xk)
     !
   endif
   !
   ! workaround for possible compiler bug
#if defined _PGI || defined _NV
   if (iq>1.and.size(PAR_IND_Q_ibz%n_of_elements)>1) then
#else
   if (iq>1) then
#endif
     if (.not.PAR_IND_Q_ibz%element_1D(iq)) cycle
   endif
   !
   ! Check if this q is compatible with the user defined direction in chartesian coordinates
   ! and if an additional RL vectors must be added
   i_G_shift=OPTICS_select_q_and_G(iq,q,X)
   if (i_G_shift<0) cycle
   !
   ! In TDDFT with BS based kernel eps0 is non-sense
   ! as shifted of the K diagonal
   l_skip_non_int_eps=l_bs_fxc
   !
   ! Frequencies (AFTER TDDFT SETUP!)
   call FREQUENCIES_setup(wv)
   !
   ! Response function parallel allocation
   call X_half_mat_check(Xen,Xk,wv)
   !
   call X_ALLOC_parallel(X_par(1),X%ng,wv%n_freqs,"XUP")
   !
   Macroscopic_CPU=X_par(1)%rows(1)<=i_G_shift.and.i_G_shift<=X_par(1)%rows(2).and.&
&                  X_par(1)%cols(1)<=i_G_shift.and.i_G_shift<=X_par(1)%cols(2)
   !
   if (iq==1) call PP_wait()
   if (iq> 1) call PP_wait(COMM=PAR_COM_Q_A2A%COMM)
   !
   ! Irredux (Xo) 
   call X_irredux(iq,"Xo",X_par(1),Xen,Xk,wv,X,Dip)
   !
   ! Without LF. I start defining spectra=eps-1
   spectra=cZERO
   if (Macroscopic_CPU) then
     ! I start defining spectra=eps-1
     ! using the definition of epsilon-1 in terms of chi-bar
     fac=4._SP*pi/Q_plus_G_sq_modulus
     spectra(:,1)=-X_par(1)%blc(i_G_shift,i_G_shift,:)*fac
   endif
   call PP_redux_wait(spectra(:,1),COMM=PAR_COM_RL_INDEX%COMM)
   !
   if (.not.l_rpa_IP) then
     !
     ! X Dyson equation solver
     call X_redux(iq,"X",X_par(1),wv,X)
     !
     ! X_redux can change the matrix distribution. Need to recheck who is Macroscopic CPU
     Macroscopic_CPU=X_par(1)%rows(1)<=i_G_shift.and.i_G_shift<=X_par(1)%rows(2).and.&
&                    X_par(1)%cols(1)<=i_G_shift.and.i_G_shift<=X_par(1)%cols(2)
     !
     if (Macroscopic_CPU) then
       !
       ! At variance with the IP case, after the call to X_redux,
       ! X_par now contains the factor 4*pi/bare_qpg**2
       ! constructed with the coulomb cutoff-term
       spectra(:,2)=X_par(1)%blc(i_G_shift,i_G_shift,:)
       !
     endif
     call PP_redux_wait(spectra(:,2),COMM=PAR_COM_RL_INDEX%COMM)
     !
   endif
   !
   if (l_bs_fxc) then
     if (iq==1) call PP_redux_wait(X_fxc)
     if (iq> 1) call PP_redux_wait(X_fxc,COMM=PAR_COM_Q_A2A%COMM)
   endif
   !
   call X_OUTPUT(iq,Xen,Xk,wv,X,spectra)
   !
   ! CLEAN (each q)
   call FREQUENCIES_reset(wv,"all")
   call MATRIX_reset(X_par(1))
   YAMBO_FREE(X_par_lower_triangle%blc)
   if (use_X_DbGd) then
     YAMBO_FREE(X_DbGd_nkpt_at_q_table)
     YAMBO_FREE(X_DbGd_nkpt_at_q)
   endif
   !
 enddo
 !
 ! CLEAN
 !
 call MATRIX_reset(X_par_lower_triangle)
 deallocate(X_par)
 call WF_free(WF)
 call PP_wait()
 if (allocated(F_xc_gspace)) then
   YAMBO_FREE(F_xc_gspace)
 endif
 if (l_bs_fxc) then
   YAMBO_FREE(FXC_K_diagonal)
 endif
 !
end subroutine

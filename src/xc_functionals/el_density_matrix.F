!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine el_density_matrix(G_lesser,en,Xk,rho,lowest_band)
 !
 ! Electronic density matrix including the off-diagonal
 ! elements of the lesser Green function
 !
 use pars,           ONLY:SP,cI,cZERO
 use electrons,      ONLY:levels,n_spinor,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use interfaces,     ONLY:WF_load,WF_free
 use hamiltonian,    ONLY:WF_G_max,WF_Go_indx
 use D_lattice,      ONLY:nsym,i_time_rev
 use FFT_m,          ONLY:fft_size,fft_rot_r
 use wave_func,      ONLY:WF,WAVEs
 use real_time,      ONLY:RT_bands,RTibz
 use parallel_m,     ONLY:PAR_IND_WF_k,PAR_IND_B_mat_ordered,PAR_IND_WF_linear,PAR_G_k_range
 use parallel_int,   ONLY:PP_redux_wait
 use matrix_operate, ONLY:UP_matrix_index
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(bz_samp), intent(in)  :: Xk
 complex(SP),   intent(in)  :: G_lesser(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2),n_sp_pol)
 integer,       intent(in)  :: lowest_band
 type(levels),  intent(in)  :: en       
 real(SP),      intent(out) :: rho(fft_size)
 !
 ! Work Space 
 !
 integer     :: ib1,ib2,ik,ik_RT,i_wf1,rho_syms,i_wf2,ir,i_sp_pol,i_spinor
 real(SP)    :: rho_no_sym(fft_size)
 complex(SP) :: f_occ
 type(WAVEs) :: WF_tmp
 !
 call timing('el_density_matrix',OPR='start')
 !
 rho=0._SP
 rho_no_sym=0._SP
 !
 if(lowest_band<RT_bands(1)) then
   !
   do i_sp_pol=1,n_sp_pol
     do ik=1,Xk%nibz
       if (.not.any(PAR_IND_WF_linear%element_2D(lowest_band:RT_bands(1)-1,ik))) cycle
       call WF_load(WF_tmp,WF_G_max,WF_Go_indx,(/1,RT_bands(1)-1/),(/ik,ik/),(/i_sp_pol,i_sp_pol/),&
&                   space='R',title='-RT',keep_states_to_load=.true.)
       do ib1=lowest_band,RT_bands(1)-1
         !
         if (.not.PAR_IND_WF_linear%element_2D(ib1,ik)) cycle
         !
         f_occ=en%f(ib1,ik,1)
         !
         i_wf1=WF_tmp%index(ib1,ik,i_sp_pol)
         do i_spinor=1,n_spinor         
           rho_no_sym(:)=rho_no_sym(:)+real(f_occ*Xk%weights(ik)*abs(WF_tmp%c(:,i_spinor,i_wf1))**2)
         enddo
         !
       enddo
       call WF_free(WF_tmp,keep_fft=.true.)
     enddo
   enddo
   !
 endif
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,Xk%nibz
     !
#if !defined _YPP_RT
     if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
#endif
     !      
     do ib1=RT_bands(1),RT_bands(2)
       !
#if defined _YPP_RT
       if (.not.PAR_IND_WF_linear%element_2D(ib1,ik)) cycle
#endif
       !
       do ib2=ib1,RT_bands(2)
         !
#if !defined _YPP_RT
         if (.not.PAR_IND_B_mat_ordered%element_1D(  UP_matrix_index(ib1-RT_bands(1)+1,ib2-RT_bands(1)+1)-1 )) cycle
#endif
         !
         f_occ=cZERO
         do ik_RT=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
           if (ib1==ib2) f_occ=f_occ-cI*G_lesser(ib1,ib2,ik_RT,i_sp_pol)
           if (ib1/=ib2) f_occ=f_occ-cI*G_lesser(ib1,ib2,ik_RT,i_sp_pol)*2._SP
           f_occ=f_occ*RTibz%weights(ik_RT)
         enddo
         !
         i_wf1=WF%index(ib1,ik,i_sp_pol)
         i_wf2=WF%index(ib2,ik,i_sp_pol)
         do i_spinor=1,n_spinor
           rho_no_sym(:)=rho_no_sym(:)+real(f_occ*WF%c(:,i_spinor,i_wf1)*conjg(WF%c(:,i_spinor,i_wf2)),SP)
         enddo
         !
       enddo
     enddo
     !
   enddo
 enddo
 !
 call PP_redux_wait(rho_no_sym)
 !
 ! Symmetrization
 !
 rho_syms=nsym/(i_time_rev+1)
 !
 do ir=1,rho_syms
   rho(:)=rho(:)+rho_no_sym(fft_rot_r(:,ir))/real(nsym,SP)
 enddo
 !
 rho(:)=real(1+i_time_rev,SP)*rho(:)
 !
 call timing('el_density_matrix',OPR='stop')
 !
end subroutine

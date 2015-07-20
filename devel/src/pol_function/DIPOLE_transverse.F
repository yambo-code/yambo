!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine Dipole_transverse(Xen,Xk,X)
 !
 ! This routine returns <iR> and <P^2>.
 !
 use pars,                ONLY:SP,schlen,cONE,CZERO
 use drivers,             ONLY:l_real_time,l_sc_run
 use interfaces,          ONLY:WF_load
 use LIVE_t,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spinor,n_sp_pol
 use parallel_m,          ONLY:PP_redux_wait,PAR_IND_CON_BANDS_X,PAR_IND_DIPk_ibz,&
&                              PAR_DIPk_ibz_index,PAR_IND_VAL_BANDS_X,&
&                              PAR_IND_CON_BANDS_X_ID,PAR_IND_VAL_BANDS_X_ID,PAR_IND_CON_BANDS_X_ID,&
&                              PAR_IND_Xk_ibz_ID,PAR_COM_q_for_Xo,PAR_COM_k_for_P,&
&                              PAR_IND_B_mat,PAR_IND_B_mat_ID,PAR_IND_WF_k
 use pseudo,              ONLY:PP_free,pp_table,Vnl,pp_n_l_comp,  &
&                              pp_kbv_dim,pp_factor
 use R_lattice,           ONLY:bz_samp
 use X_m,                 ONLY:X_t,DIP_iR,Dipole_bands_ordered,P_square,DIP_P,&
&                              Dipole_Energy_treshold,use_g_space_approach,force_v_g_space_approach
 use IO_m,                ONLY:io_control,RD_CL,RD
 use memory_m,            ONLY:mem_est
 use wave_func,           ONLY:wf_ng,WF
 use com,                 ONLY:error,msg,warning
#if defined _SC
 use SC,                  ONLY:B_mat_index
 use com,                 ONLY:secnm
#endif
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 character(schlen)            :: live_t_string
 logical                      :: l_eval_DIP_iR,l_eval_DIP_P,l_eval_DIP_P2
 integer                      :: ik,icfft(n_spinor),ivfft(n_spinor),ic,iv,iv_max,ic_min,&
&                                i_sp_pol,i_spinor,i_spin,alloc_err,wf_dim,n_LT_steps,ik_mem
 complex(SP), allocatable     :: kbv(:,:,:,:)
 complex(SP)                  :: rho(4),tmp(3)
 real(SP)                     :: Ev_m_Ec,Ev_m_Ec_QP
#if defined _SC
 logical                      :: l_evaluating_em1s 
#endif
 !
 !I/O and external functions
 !
 integer                      :: ID,io_err(3),io_KB_abinit_err,io_KB_pwscf_err,io_Vnl_err
 integer, external            :: io_KB_abinit,io_KB_pwscf,io_Vnl
 !
 call DIPOLE_kb_init(Xen,X,io_err,ID)
 !
 io_KB_abinit_err= io_err(1)
 io_KB_pwscf_err = io_err(2)
 io_Vnl_err      = io_err(3)
 !
 if(io_KB_abinit_err==0.or.io_KB_pwscf_err==0) then
   allocate(kbv(wf_ng,pp_kbv_dim,4,n_spinor),stat=alloc_err)
   call mem_est("KBV",(/size(kbv)/),errors=(/alloc_err/))
 endif
 !
 !
 Dipole_bands_ordered       = .true.
 !
 ! NOTE: 1 - that in case we will enter here during a SC run to update the
 !           screened interaction all transitions must be considered in order to rotate <P>.
 !       2 - when using internal SC potentials oscilators must be rotated
 !           in the new basis. So all transitions are needed.
 !       3 - In real-time simulations P and P^2 are ALWAYS calculated => no band ordering
 !
#if defined _SC 
 l_evaluating_em1s=index(secnm,"Dielectric")/=0
 if ((l_sc_run.or.l_real_time).and..not.l_evaluating_em1s) Dipole_bands_ordered=.false.
#endif
 !
#if defined _YPP_RT
 Dipole_bands_ordered=.false.
#endif
 !
 ! Set up band limits
 !
 if (Dipole_bands_ordered.or.Dipole_Energy_treshold<0.) then
   iv_max=Xen%nbm
   ic_min=Xen%nbf+1
 else
   iv_max=X%ib(2)
   ic_min=X%ib(1)
 endif
 !
 ! WF load
 ! Here I avoid the deallocation of states_to_load as I ASSUME that AFTER this routine 
 ! there will be other WF_load calls using the present WF distribution. 
 !
 call WF_load(WF,0,1,X%ib,(/1,Xk%nibz/),space='G',title='-Oscillators/G space',&
&             force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
 !
 ! Eo    are always the eigenvalues consistent with the WFs
 ! E     do also include the QP corrections when Eo are associated
 if (associated(Xen%Eo)   ) call msg('rns','[X] Using energies without QP corrections for the dipoles')
 !
 ! GPL_EXCLUDE_START
 !
#if !defined _YPP_RT
 !
 ! GPL_EXCLUDE_END
 !
 if (Dipole_bands_ordered) then
   n_LT_steps=PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)*&
&             PAR_IND_CON_BANDS_X(X%whoami)%n_of_elements(PAR_IND_CON_BANDS_X_ID(X%whoami)+1)*&
&             PAR_IND_VAL_BANDS_X(X%whoami)%n_of_elements(PAR_IND_VAL_BANDS_X_ID(X%whoami)+1)*n_sp_pol
 else
   n_LT_steps=PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)*&
&             PAR_IND_B_mat%n_of_elements(PAR_IND_B_mat_ID+1)*n_sp_pol
 endif
 !
 ! GPL_EXCLUDE_START
 !
#else
 n_LT_steps=Xk%nibz*(X%ib(2)-X%ib(1)+1)**2._SP*n_sp_pol
#endif
 !
 ! GPL_EXCLUDE_END
 !
 l_eval_DIP_iR=use_g_space_approach
 l_eval_DIP_P =use_g_space_approach.or.force_v_g_space_approach
 l_eval_DIP_P2=.false.
 wf_dim=3
 !
#if defined _SC
 l_eval_DIP_P2=l_sc_run.or.l_real_time
 if(l_eval_DIP_P2) wf_dim=4
#endif
 !
 if(l_eval_DIP_P)                    live_t_string='Dipole  P        (T):'
 if(l_eval_DIP_P .and.l_eval_DIP_iR) live_t_string='Dipole  P and iR (T):'
 if(l_eval_DIP_P2.and.l_eval_DIP_P)  live_t_string='Dipole  P and P2 (T):'
 if(l_eval_DIP_P2.and.l_eval_DIP_iR) live_t_string='Dip iR, P and P2 (T):'
 !
 if (n_LT_steps>0) call live_timing(trim(live_t_string),n_LT_steps)
 !
#if !defined _YPP_RT
 call PP_redux_wait(COMM=PAR_COM_q_for_Xo%COMM)
#endif
 !
 ! Main loop over k in IBZ
 !
 do ik=1,Xk%nibz
   !
   if (n_LT_steps==0) cycle
   ! 
   ! [Vnl,r]
   !
   if (io_KB_pwscf_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_KB_pwscf_err=io_KB_pwscf(ID)
   endif
   !
   if (io_KB_abinit_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_KB_abinit_err=io_KB_abinit(ID)
   endif
   ! 
#if !defined _YPP_RT
   if (Dipole_bands_ordered) then
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
   else
     if (.not.PAR_IND_WF_k%element_1D(ik)) cycle 
   endif
#endif
   !
   ik_mem=PAR_DIPk_ibz_index(ik)
   ! 
   do i_sp_pol=1,n_sp_pol
     ! 
     if (io_KB_abinit_err==0) call DIPOLE_kb_abinit_comp(ik,i_sp_pol,Xk,kbv)
     if (io_KB_pwscf_err ==0) call DIPOLE_kb_pwscf_comp(ik,Xk,kbv)
     !
     do iv=X%ib(1),iv_max 
       !
       if (Dipole_bands_ordered) then
         if (.not.PAR_IND_VAL_BANDS_X(X%whoami)%element_1D(iv)) cycle 
       endif
       !
       do ic=ic_min,X%ib(2)
         !
         if (Dipole_bands_ordered) then
           if (.not.PAR_IND_CON_BANDS_X(X%whoami)%element_1D(ic)) cycle 
         else
#if defined _SC && !defined _YPP_RT
           if (.not.PAR_IND_B_mat%element_1D( B_mat_index(ic,iv))) cycle
#endif
         endif
         !
         do i_spinor=1,n_spinor
           !
           i_spin=i_sp_pol+i_spinor-1
           ivfft(i_spinor)=WF%index(iv,ik,i_spin)
           icfft(i_spinor)=WF%index(ic,ik,i_spin)
           !
           if (ivfft(i_spinor)==0) call error(" Error in parallel wf distribution (Dipole iv)")
           if (icfft(i_spinor)==0) call error(" Error in parallel wf distribution (Dipole ic)")
           !
         enddo
         !
         if (associated(Xen%Eo)) then 
           Ev_m_Ec   =Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)
           Ev_m_Ec_QP=Xen%E (iv,ik,i_sp_pol)-Xen%E (ic,ik,i_sp_pol)
         else
           Ev_m_Ec   =Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
           Ev_m_Ec_QP=Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
         endif
         !
         if (any((/-Ev_m_Ec<X%ehe(1).and.X%ehe(1)>0.0_SP,-Ev_m_Ec>X%ehe(2).and.X%ehe(2)>0.0_SP/)).or.&
&            abs(Ev_m_Ec)<=Dipole_Energy_treshold.and..not.l_real_time) then
           !
           call live_timing(steps=1)
           cycle
           !
         endif
         !
         ! Evaluate <ivfft|   p   |icfft>
         call DIPOLE_p_matrix_elements(Xk,ivfft,icfft,ik,wf_dim,rho)
         !
         ! Evaluate <ivfft|[x,Vnl]|icfft>
         if (io_Vnl_err==0) rho(1:3) = rho(1:3) + Vnl(1:3,ic,iv,ik,i_sp_pol)*0.5*real(n_spinor,SP)
         if (io_KB_abinit_err==0) call DIPOLE_kb_sum(icfft,ivfft,rho(:3),kbv,1)
         if (io_KB_pwscf_err ==0) call DIPOLE_kb_sum(icfft,ivfft,rho(:3),kbv,2)
         !
         !=====
         ! <P>
         !=====
         ! P_vc=rho(:3)
         !
         if (l_eval_DIP_P) then
           ! See for this PRB 48, 11789 (1993), definition of p_eff
           DIP_P(:,ic,iv,ik_mem,i_sp_pol)=rho(:3)*Ev_m_Ec_QP/Ev_m_Ec
         endif
         !
         !======
         ! <iR>
         !======
         ! as [x,p_x]=i we get [x,H] = [x,p^2/2]= i p_x. So from rho (that is P_vc) we can extract
         !
         ! DIP_iR(c,v) = i <v|r|c> = i <v|[r,H]|c>/(Ec-Ev) = i <v|i p|c>/(Ec-Ev) =
         !             = - <v | -i grad |c> / (Ec-Ev) = <v|-i grad|c>/(Ev-Ec) = P_vc/(Ev-Ec)
         !
         if (l_eval_DIP_iR) then
           rho(1:3)=rho(1:3)/Ev_m_Ec
           if (abs(Ev_m_Ec)<=Dipole_Energy_treshold) rho(1:3)=cZERO
           DIP_iR(:,ic,iv,ik_mem,i_sp_pol)=rho(:3)
         endif
         !
         !======
         ! <P^2>
         !======
         !
         if (l_eval_DIP_P2) then
           P_square(ic,iv,ik_mem,i_sp_pol)= rho(4)
           ! See for this PRB 48, 11789 (1993), definition of p_eff
           if(associated(Xen%Eo)) P_square(ic,iv,ik_mem,i_sp_pol)=P_square(ic,iv,ik_mem,i_sp_pol)*(Ev_m_Ec_QP/Ev_m_Ec)**2
         endif
         !
         call live_timing(steps=1)
         !
       enddo    ! conduction band loop
       !
     enddo      ! valence band loop
     ! 
   enddo        ! sp_pol loop
   ! 
 enddo          ! k-points
 !
 call live_timing()
 !
 ! CLEAN
 !
 if (io_KB_pwscf_err==0.or.io_KB_abinit_err==0) then
   if (allocated(kbv))          deallocate(kbv)
   if (allocated(pp_n_l_comp))  deallocate(pp_n_l_comp)
   if (allocated(pp_table))     deallocate(pp_table)
   if (allocated(pp_factor))    deallocate(pp_factor)
   call PP_free()
   call mem_est("KBV") 
 else if (io_Vnl_err==0) then
   if (allocated(Vnl)) deallocate(Vnl)
 endif 
 !
 call WF_free(WF)
 !
 do ik=1,Xk%nibz
   !
   if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
   !
   ik_mem=PAR_DIPk_ibz_index(ik)
   ! 
   do i_sp_pol=1,n_sp_pol
#if !defined _YPP_RT
     if (l_eval_DIP_iR ) call PP_redux_wait(DIP_iR(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_k_for_P%COMM)
     if (l_eval_DIP_P  ) call PP_redux_wait( DIP_P(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_k_for_P%COMM)
     if (l_eval_DIP_P2 ) call PP_redux_wait(P_square(:,:,ik_mem,i_sp_pol),COMM=PAR_COM_k_for_P%COMM)
#endif
   enddo
   !
   ! Symmetrization 
   !================
   !
   ! To avoid numerical noise in the real-time evolution we need to impose
   ! P (iR) to be Hermitian (anti-Hermitian)
   !
   if (.not.Dipole_bands_ordered) then
     do i_sp_pol=1,n_sp_pol
       do iv=X%ib(1),X%ib(2)
         do ic=iv+1,X%ib(2)
           if (l_eval_DIP_iR) then
             tmp=( DIP_iR(:,ic,iv,ik_mem,i_sp_pol)-conjg(DIP_iR(:,iv,ic,ik_mem,i_sp_pol)) )/2._SP
             DIP_iR(:,ic,iv,ik_mem,i_sp_pol) =       tmp
             DIP_iR(:,iv,ic,ik_mem,i_sp_pol) =-conjg(tmp)
           endif
           if (l_eval_DIP_P ) then
             tmp=( DIP_P(:,ic,iv,ik_mem,i_sp_pol) + conjg(DIP_P(:,iv,ic,ik_mem,i_sp_pol)) )/2._SP
             DIP_P(:,ic,iv,ik_mem,i_sp_pol) =       tmp
             DIP_P(:,iv,ic,ik_mem,i_sp_pol) = conjg(tmp)
           endif
           if (l_eval_DIP_P2) then
             tmp(1)=( P_square(ic,iv,ik_mem,i_sp_pol) + conjg(P_square(iv,ic,ik_mem,i_sp_pol)) )/2._SP 
             P_square(ic,iv,ik_mem,i_sp_pol) =       tmp(1)
             P_square(iv,ic,ik_mem,i_sp_pol) = conjg(tmp(1))
           endif
         enddo
       enddo
     enddo
   endif
   !
 enddo
 !
end subroutine Dipole_transverse

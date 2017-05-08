!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
 use pars,                ONLY:SP,schlen,cZERO,cI
 use drivers,             ONLY:l_sc_run
#if defined _PL
 use drivers,             ONLY:l_photolum
#endif
 use interfaces,          ONLY:WF_load,WF_free
 use LIVE_t,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spinor,n_sp_pol
 use parallel_m,          ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index
 use pseudo,              ONLY:PP_free,pp_table,Vnl,pp_n_l_comp,pp_kbv_dim,pp_factor
 use R_lattice,           ONLY:bz_samp
 use X_m,                 ONLY:X_t,DIP_iR,P_square,DIP_P,DIP_P_spinor,l_X_terminator,&
&                              Dipole_Energy_treshold,Dipole_Energy_treshold_vv,&
&                              use_g_space_approach,force_v_g_space_approach
 use IO_m,                ONLY:io_control,RD_CL,RD
 use wave_func,           ONLY:wf_ng,WF
 use com,                 ONLY:error,msg
#if defined _RT
 use drivers,             ONLY:l_real_time
#endif
#if defined _SC || defined _RT
 use com,                 ONLY:secnm
#endif
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#include<memory.h>
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 character(schlen)            :: live_t_string
#if defined _SC || defined _RT
 logical                      :: l_evaluating_em1s 
#endif
 logical                      :: l_eval_DIP_iR,l_eval_DIP_P,l_eval_DIP_P2,&
&                                l_mat_eval_DIP(X%ib_lim(2):X%ib(2),X%ib(1):X%ib_lim(1))
 integer                      :: ik,icfft(n_spinor),ivfft(n_spinor),ic,iv,&
&                                i_sp_pol,i_spinor,i_spin,n_LT_steps,ik_mem,ic_min
 complex(SP), allocatable     :: kbv(:,:,:,:)
 complex(SP)                  :: rho_spinor(3,n_spinor,n_spinor),rho(3),P2
 real(SP)                     :: Ev_m_Ec,W_,Dipole_Energy_treshold_
 complex(SP)                  :: Ev_m_Ec_QP
 !
 !I/O and external functions
 !
 integer                      :: ID,io_err(3),io_KB_abinit_err,io_KB_pwscf_err,io_Vnl_err
 integer, external            :: io_KB_abinit,io_KB_pwscf,io_Vnl
 !
#if defined _TIMING
 call timing('DIPOLE_transverse',OPR='start')
#endif
#if defined _SC || defined _RT
 l_evaluating_em1s=index(secnm,"Dielectric")/=0
#endif
 !
 call DIPOLE_kb_init(Xen,X,io_err,ID)
 !
 io_KB_abinit_err= io_err(1)
 io_KB_pwscf_err = io_err(2)
 io_Vnl_err      = io_err(3)
 !
 if(io_KB_abinit_err==0.or.io_KB_pwscf_err==0) then
   YAMBO_ALLOC(kbv,(wf_ng,pp_kbv_dim,4,n_spinor))
 endif
 !
 ! WF load
 ! Here I avoid the deallocation of states_to_load as I ASSUME that AFTER this routine 
 ! there will be other WF_load calls using the present WF distribution. 
 ! This is the case, for example, of the response function calculation.
 !
 call WF_load(WF,0,1,X%ib,(/1,Xk%nibz/),space='G',title='-Oscillators/G space',&
&             force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
 !
 ! Eo    are always the eigenvalues consistent with the WFs
 ! E     do also include the QP corrections when Eo are allocated
 !
 if (allocated(Xen%Eo)   ) call msg('rns','[X] Using energies without QP corrections for the dipoles')
 !
 l_eval_DIP_iR=use_g_space_approach
#if defined _PL
 l_eval_DIP_P =use_g_space_approach.or.force_v_g_space_approach.or.l_photolum
#else
 l_eval_DIP_P =use_g_space_approach.or.force_v_g_space_approach
#endif
 l_eval_DIP_P2=.false.
 !
 ! Parallelization Mask 
 !======================
 ! Depending on the environment DIPOLES are distributed in different ways
 !
 call DIPOLE_build_parallelization_mask(X,l_mat_eval_DIP,n_LT_steps)
 !
#if defined _SC
 l_eval_DIP_P2=l_sc_run
#endif
#if defined _RT
 l_eval_DIP_P2=l_real_time
#endif
 !
 if (l_eval_DIP_P)                    live_t_string='Dipoles: P (T):'
 if (l_eval_DIP_P .and.l_eval_DIP_iR) live_t_string='Dipoles: P and iR (T):'
 if (l_eval_DIP_P2.and.l_eval_DIP_P)  live_t_string='Dipoles: P and P2 (T):'
 if (l_eval_DIP_P2.and.l_eval_DIP_iR) live_t_string='Dipoles: iR, P and P2 (T):'
 !
 if (n_LT_steps>0) call live_timing(trim(live_t_string),n_LT_steps)
 !
 ! Main loop over k in IBZ
 !
 W_=0._SP
 !
 do ik=1,Xk%nibz
   !
   if (n_LT_steps==0) cycle
   ! 
   ! [Vnl,r]
   !
   if (io_KB_pwscf_err==0) then
     if (ik< Xk%nibz ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
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
   if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
   !
   ik_mem=PAR_DIPk_ibz_index(ik)
   ! 
   do i_sp_pol=1,n_sp_pol
     ! 
     if (io_KB_abinit_err==0) call DIPOLE_kb_abinit_comp(ik,i_sp_pol,Xk,kbv)
     if (io_KB_pwscf_err ==0) call DIPOLE_kb_pwscf_comp(ik,Xk,kbv)
     !
     do iv=X%ib(1),X%ib_lim(1) 
       !
       ic_min=max(iv,X%ib_lim(2))
       if (l_X_terminator) ic_min=iv
       !
       do ic=ic_min,X%ib(2)
         !
         if (.not.l_mat_eval_DIP(ic,iv)) cycle
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
         if (allocated(Xen%W )) W_=abs(Xen%W(iv,ik,i_sp_pol))+abs(Xen%W(ic,ik,i_sp_pol))
         !
         Ev_m_Ec   =Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
         Ev_m_Ec_QP=Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)+cI*W_
         !
         if (allocated(Xen%Eo)) Ev_m_Ec=Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)
         !
         ! Evaluate <ivfft|   p   |icfft>
         !================================
         call DIPOLE_p_matrix_elements(Xk,ivfft,icfft,ik,rho,rho_spinor,P2,l_eval_DIP_P2)
         !
         ! Evaluate <ivfft|[x,Vnl]|icfft>
         !================================
         if (io_KB_abinit_err==0) call DIPOLE_kb_sum(icfft,ivfft,rho,kbv)
         if (io_KB_pwscf_err ==0) call DIPOLE_kb_sum(icfft,ivfft,rho,kbv)
         !
         ! define a different thr for val-val transitions
         ! used for XTerm (here Ev_m_Ec may become very large)
         !
         Dipole_Energy_treshold_=Dipole_Energy_treshold 
         if(l_X_terminator.and.ic<=X%ib_lim(1)) Dipole_Energy_treshold_=Dipole_Energy_treshold_vv
         !
         !=====
         ! <P>
         !=====
         ! P_vc=rho
         !
         if (l_eval_DIP_P) then
           !
           ! See for this PRB 48, 11789 (1993), definition of p_eff
           !
           if (.not.(allocated(Xen%Eo).or.allocated(Xen%W))) DIP_P(:,ic,iv,ik_mem,i_sp_pol)=rho
           if (     (allocated(Xen%Eo).or.allocated(Xen%W))) then
             if (abs(Ev_m_Ec)<=Dipole_Energy_treshold_) DIP_P(:,ic,iv,ik_mem,i_sp_pol)=cZERO
             if (abs(Ev_m_Ec)> Dipole_Energy_treshold_) DIP_P(:,ic,iv,ik_mem,i_sp_pol)=rho*Ev_m_Ec_QP/Ev_m_Ec
           endif
           !
#if defined _RT
           if (l_real_time .and. n_spinor>1) then
             if (.not.(allocated(Xen%Eo).or.allocated(Xen%W))) DIP_P_spinor(:,:,:,ic,iv,ik_mem)=rho_spinor
             if (     (allocated(Xen%Eo).or.allocated(Xen%W))) then
               if (abs(Ev_m_Ec)<=Dipole_Energy_treshold_) DIP_P_spinor(:,:,:,ic,iv,ik_mem)=cZERO
               if (abs(Ev_m_Ec)> Dipole_Energy_treshold_) DIP_P_spinor(:,:,:,ic,iv,ik_mem)=rho_spinor*Ev_m_Ec_QP/Ev_m_Ec
             endif
           endif
#endif
           !
         endif
         !
         !======
         ! <iR>
         !======
         ! as 
         ! 
         !  [x,p_x]=i we get [x,H] = [x,p^2/2]+[x,Vnl]
         ! 
         ! we gave that 
         ! 
         ! [x,p^2/2]= i p_x 
         !
         ! So from the local part of rho (that is P_vc) we can extract the corresponding local part of <x>
         !
         ! DIP_iR(c,v) = i <v|r|c> = i <v|[r,H]|c>/(Ec-Ev) = 
         !             = i <v|i p|c>/(Ec-Ev) + i<v|[x,Vnl]|c>/(Ec-Ev) =
         !             = - <v|-i grad|c>/(Ec-Ev) +i<v|[x,Vnl]|c>/(Ec-Ev) =
         !             =   <v|-i grad|c>/(Ev-Ec) -i<v|[x,Vnl]|c>/(Ev-Ec) = 
         !             =   P_vc/(Ev-Ec) -i<v|[x,Vnl]|c>/(Ev-Ec)
         !
         if (l_eval_DIP_iR) then
           if (abs(Ev_m_Ec)> Dipole_Energy_treshold_) DIP_iR(:,ic,iv,ik_mem,i_sp_pol)=rho/Ev_m_Ec
           if (abs(Ev_m_Ec)<=Dipole_Energy_treshold_) DIP_iR(:,ic,iv,ik_mem,i_sp_pol)=cZERO
         endif
         !
         !======
         ! <P^2>
         !======
         !
         if (l_eval_DIP_P2) then
           ! See for this PRB 48, 11789 (1993), definition of p_eff
           if(.not.allocated(Xen%Eo)) P_square(ic,iv,ik_mem,i_sp_pol)= P2
           if(     allocated(Xen%Eo)) then
             if (abs(Ev_m_Ec)<=Dipole_Energy_treshold_) P_square(ic,iv,ik_mem,i_sp_pol)=cZERO
             if (abs(Ev_m_Ec)> Dipole_Energy_treshold_) P_square(ic,iv,ik_mem,i_sp_pol)=P2*(Ev_m_Ec_QP/Ev_m_Ec)**2
           endif
         endif
         !
         call live_timing(steps=1)
         !
       enddo    ! conduction band loop
     enddo      ! valence    band loop
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
   YAMBO_FREE(kbv)
   YAMBO_FREE(pp_n_l_comp)
   YAMBO_FREE(pp_table)
   YAMBO_FREE(pp_factor)
   call PP_free()
 else if (io_Vnl_err==0) then
   YAMBO_FREE(Vnl)
 endif 
 !
 call WF_free(WF)
 !
#if defined _TIMING
 call timing('DIPOLE_transverse',OPR='stop')
#endif
 !
end subroutine Dipole_transverse

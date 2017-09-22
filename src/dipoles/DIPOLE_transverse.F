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
subroutine Dipole_transverse(Xen,Xk,Dip)
 !
 ! This routine returns <iR> and <P^2>.
 !
 use pars,                ONLY:SP,schlen,cZERO,cI
 use drivers,             ONLY:l_sc_run
#if defined _PL
 use drivers,             ONLY:l_photolum
#endif
 use interfaces,          ONLY:WF_load
 use LIVE_t,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spinor,n_sp_pol
 use parallel_m,          ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index,PAR_IND_DIPk_ibz_ID,&
&                              PAR_IND_VAL_BANDS_DIP,PAR_IND_VAL_BANDS_DIP_ID,&
&                              PAR_IND_CON_BANDS_DIP,PAR_IND_CON_BANDS_DIP_ID
 use pseudo,              ONLY:PP_free,pp_table,Vnl,pp_n_l_comp,pp_kbv_dim,pp_factor
 use R_lattice,           ONLY:bz_samp
 use DIPOLES,             ONLY:DIPOLE_t,DIP_iR,P_square,DIP_P,use_g_space_approach
 use X_m,                 ONLY:l_X_terminator
 use IO_m,                ONLY:io_control,RD_CL,RD
 use wave_func,           ONLY:wf_ng,WF
 use com,                 ONLY:error,msg
#if defined _SC || defined _RT
 use drivers,             ONLY:l_real_time
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
 type(DIPOLE_t),intent(inout) :: Dip
 !
 ! Work Space
 !
 character(schlen)            :: live_t_string
 logical                      :: l_eval_DIP_iR,l_eval_DIP_P,l_eval_DIP_P2
 integer                      :: ik,ic_wf,iv_wf,ic,iv,ifrag,&
&                                i_sp_pol,wf_dim,n_LT_steps,ik_mem,ic_min
 complex(SP), allocatable     :: kbv(:,:,:,:)
 complex(SP)                  :: rho(4)
 real(SP)                     :: Ev_m_Ec,Dipole_Energy_treshold
 !
 !I/O and external functions
 !
 integer                      :: ID,io_err(3),io_KB_abinit_err,io_KB_pwscf_err,io_Vnl_err
 integer, external            :: io_KB_abinit,io_KB_pwscf,io_Vnl
 !
#if defined _TIMING
 call timing('DIPOLE_transverse',OPR='start')
#endif
 !
 call DIPOLE_kb_init(Xen,Dip,io_err,ID)
 !
 io_KB_abinit_err= io_err(1)
 io_KB_pwscf_err = io_err(2)
 io_Vnl_err      = io_err(3)
 !
 if(io_KB_abinit_err==0.or.io_KB_pwscf_err==0) then
   YAMBO_ALLOC(kbv,(wf_ng,pp_kbv_dim,4,n_spinor))
 endif
 !
 ! Eo    are always the eigenvalues consistent with the WFs
 ! E     do also include the QP corrections when Eo are associated
 !
 if (allocated(Xen%Eo)   ) call msg('rns','[X] Using energies without QP corrections for the dipoles')
 !
 l_eval_DIP_iR=use_g_space_approach
#if defined _PL
 l_eval_DIP_P =use_g_space_approach.or.Dip%force_v_g_space.or.l_photolum
#else
 l_eval_DIP_P =use_g_space_approach.or.Dip%force_v_g_space
#endif
 l_eval_DIP_P2=.false.
 wf_dim=3
 !
 ! GPL_EXCLUDE_START
 !
#if defined _SC || defined _RT
 l_eval_DIP_P2=l_sc_run.or.l_real_time
 if(l_eval_DIP_P2) wf_dim=4
#endif
 !
 ! GPL_EXCLUDE_END
 !
 if (l_eval_DIP_P)                    live_t_string='Dipoles: P (T):'
 if (l_eval_DIP_P .and.l_eval_DIP_iR) live_t_string='Dipoles: P and iR (T):'
 if (l_eval_DIP_P2.and.l_eval_DIP_P)  live_t_string='Dipoles: P and P2 (T):'
 if (l_eval_DIP_P2.and.l_eval_DIP_iR) live_t_string='Dipoles: iR, P and P2 (T):'
 !
 n_LT_steps=n_sp_pol*PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_DIPk_ibz_ID+1)*&
&                    PAR_IND_VAL_BANDS_DIP%n_of_elements(PAR_IND_VAL_BANDS_DIP_ID+1)*&
&                    PAR_IND_CON_BANDS_DIP%n_of_elements(PAR_IND_CON_BANDS_DIP_ID+1)
 !
 if (n_LT_steps>0) call live_timing(trim(live_t_string),n_LT_steps)
 !
 ! Main loop over k in IBZ
 !
 !
 do i_sp_pol=1,n_sp_pol
   !
   if (n_LT_steps==0) cycle
   !
   do ik=1,Xk%nibz
     ifrag=ik+(i_sp_pol-1)*Xk%nibz
     ! 
     ! [Vnl,r]
     !
     if (io_KB_pwscf_err==0) then
       if (ifrag< n_sp_pol*Xk%nibz) call io_control(ACTION=RD,SEC=(/ifrag+1/),ID=ID)
       if (ifrag==n_sp_pol*Xk%nibz) call io_control(ACTION=RD_CL,SEC=(/ifrag+1/),ID=ID)
       io_KB_pwscf_err=io_KB_pwscf(ID)
     endif
     !
     if (io_KB_abinit_err==0) then
       if (ifrag< n_sp_pol*Xk%nibz) call io_control(ACTION=RD,SEC=(/ifrag+1/),ID=ID)
       if (ifrag==n_sp_pol*Xk%nibz) call io_control(ACTION=RD_CL,SEC=(/ifrag+1/),ID=ID)
       io_KB_abinit_err=io_KB_abinit(ID)
     endif
     ! 
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
     !
     ik_mem=PAR_DIPk_ibz_index(ik)
     !
     ! WF load
     !
     call WF_load(WF,0,1,Dip%ib,(/ik,ik/),sp_pol_to_load=(/i_sp_pol,i_sp_pol/),space='G',title='-Oscillators/G space',&
&               force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
     ! 
     if (io_KB_abinit_err==0) call DIPOLE_kb_abinit_comp(ik,i_sp_pol,Xk,kbv)
     if (io_KB_pwscf_err ==0) call DIPOLE_kb_pwscf_comp(ik,Xk,kbv)
     !
     do iv=Dip%ib(1),Dip%ib_lim(1) 
       !
       if(.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
       !
       ic_min=max(iv,Dip%ib_lim(2))
       if (l_X_terminator) ic_min=iv
       !
       do ic=ic_min,Dip%ib(2)
         !
         if(.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
         !
         iv_wf=WF%index(iv,ik,i_sp_pol)
         ic_wf=WF%index(ic,ik,i_sp_pol)
         !
         if (iv_wf==0) call error(" Error in parallel wf distribution (Dipole iv)")
         if (ic_wf==0) call error(" Error in parallel wf distribution (Dipole ic)")
         !
         Ev_m_Ec   =Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
         if (allocated(Xen%Eo)) Ev_m_Ec=Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)
         !
         ! Evaluate <iv_wf|   p   |ic_wf>
         !================================
         call DIPOLE_p_matrix_elements(Xk,iv_wf,ic_wf,ik,wf_dim,rho)
         !
         ! Evaluate <iv_wf|[x,Vnl]|ic_wf>
         !================================
         if (io_Vnl_err==0) rho(:3) = rho(:3) + Vnl(:3,ic,iv,ik,i_sp_pol)*0.5*real(n_spinor,SP)
         if (io_KB_abinit_err==0) call DIPOLE_kb_sum(ic_wf,iv_wf,rho(:3),kbv)
         if (io_KB_pwscf_err ==0) call DIPOLE_kb_sum(ic_wf,iv_wf,rho(:3),kbv)
         !
         !
         ! define a different thr for val-val transitions
         ! used for XTerm (here Ev_m_Ec may become very large)
         !
         Dipole_Energy_treshold=Dip%Energy_treshold 
         if(l_X_terminator.and.ic<=Dip%ib_lim(1)) Dipole_Energy_treshold=Dip%Energy_treshold_vv
         !
         !=====
         ! <P>
         !=====
         ! P_vc=rho(:3)
         !
         if (l_eval_DIP_P) then
           !
           DIP_P(:,ic,iv,ik_mem,i_sp_pol)=rho(:3)
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
           if (abs(Ev_m_Ec)> Dipole_Energy_treshold) DIP_iR(:,ic,iv,ik_mem,i_sp_pol)=rho(:3)/Ev_m_Ec
           if (abs(Ev_m_Ec)<=Dipole_Energy_treshold) DIP_iR(:,ic,iv,ik_mem,i_sp_pol)=cZERO
         endif
         !
         ! GPL_EXCLUDE_START
         !
         !======
         ! <P^2>
         !======
         !
         if (l_eval_DIP_P2) then
           !
           P_square(ic,iv,ik_mem,i_sp_pol)= rho(4)
           !
         endif
         !
         ! GPL_EXCLUDE_END
         !
         call live_timing(steps=1)
         !
       enddo    ! conduction band loop
     enddo      ! valence    band loop
     !
     call WF_free(WF)
     ! 
   enddo        ! k-points loop
 enddo          ! sp_pol   loop
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
#if defined _TIMING
 call timing('DIPOLE_transverse',OPR='stop')
#endif
 !
end subroutine Dipole_transverse

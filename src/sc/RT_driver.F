!
! Copyright (C) 2000-2009 A. Marini, C. Attaccalite and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_driver(en,Xen,X,k,xk,q,Xw)
 !
 use pars,           ONLY:SP,lchlen,schlen
 use drivers,        ONLY:l_sc_hf,l_sc_chosex
 use stderr,         ONLY:set_real_printed_length
 use units,          ONLY:FS2AUT
 use com,            ONLY:msg,of_open_close
 use timing,         ONLY:live_timing 
 use electrons,      ONLY:levels
 use X_m,            ONLY:X_t,Eval_P_and_P2_only,X_alloc
 use wave_func,      ONLY:wf,WF_load,wf_ng,WF_free
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:bz_samp,qindx_S,G_m_G
 use QP_m,           ONLY:QP_nb,QP_t,QP_ng_Sx,QP_n_G_bands,CHOSEX_use_empties
 use SC,             ONLY:SC_potential,SC_bands,Ho,E_bare,it_now,local_V
 use real_time,      ONLY:Thermal_steps,NE_steps,RT_step,G_lesser,l_NE_simulate_IP, &
&                         G_lesser_previous,l_NE_with_fields,RT_alloc,RT_free,      &
&                         i_time,Integrator_name,j_current,l_RT_diagonalize,        &
&                         G_lesser_reference,LifeTime,RT_diagonalization_steps,     &
&                         A_vecpot,A_vecpot_previous
 use oscillators_CACHE, ONLY:free_RHO_VX_RHO
 use oscillators_IO,    ONLY:oscillators_IO_alloc
 !
 implicit none
 !
 ! if l_qp_analyze=.true.  "en"  will contains new energies and occupations
 ! otherwise it will not be used
 !
 ! Notice that Xen is never changed!
 !
 type(levels)  :: en,Xen
 type(w_samp)  :: Xw(2) 
 type(bz_samp) :: k,Xk,q
 type(X_t):: X(4)
 integer, external :: eval_G_minus_G
 ! 
 ! Work Space
 !
 character(lchlen)        :: file_name(2)
 character(schlen)        :: sec_mode_and_message(2)
 integer                  :: WF_G_max,WF_Go_indx,it,i1,ik
 real(SP)                 :: values(9), RT_time  ! The real time on the diagonal RTime = it*RT_step    
 complex(SP)              :: Trace_Gf,E_kin,E_ion,E_h,E_x,E_xc,E_mg 
 type(QP_t)               :: qp_dummy
 !
 call section('*',trim(SC_potential)//' Real-Time Dynamics')
 !##########################################################
 !
 call set_real_printed_length(f_length=15,g_length=15)  
 !
 X(1)%ib(2)=SC_bands
 X(1)%ng   =wf_ng
 Eval_P_and_P2_only=.true.
 call Dipole_driver(Xen, Xk, X(1), X(1)%q0)
 !
 call XC_switch()
 !
 ! WFs loading
 !
 WF_G_max=wf_ng
 if(l_sc_hf.or.l_sc_chosex) then                                  
   !
   QP_nb=SC_bands
   WF_Go_indx=maxval(qindx_S(:,:,2))
   call oscillators_IO_alloc()
   !
   if (l_sc_chosex) then
     X(2)%ng=eval_G_minus_G(X(2)%ng,0)
     WF_G_max=max(QP_ng_Sx,maxval(G_m_G))
     QP_n_G_bands(2)=SC_bands
     CHOSEX_use_empties=.FALSE.
   endif
   !
   sec_mode_and_message(1)='*'
   sec_mode_and_message(2)='Hartree-Fock extended oscillators'  
   call section(trim(sec_mode_and_message(1)),trim(sec_mode_and_message(2)))
   call QP_state_table_setup(Xen)
   call XCo_Hartree_Fock(Xen, k, Xk, q)
   if(l_sc_chosex) call QP_ppa_chosex(X(2),Xk,en,k,q,qp_dummy,Xw(2),-1)
 else
   WF_Go_indx=1  
 endif
 !
 call WF_load(WF_G_max,WF_Go_indx,(/1,SC_bands/),(/1,k%nibz/),space='R',title=' ')  
 !
 ! Core initialization
 !
 call RT_initialize()
 !
 ! Core allocations
 !
 call RT_alloc(Xen)
 !
 ! Restart from a previous RT run or from a SC calculation
 ! 
 call RT_start_and_restart(Xen,Xk,k)
 !
 ! Calculation already done
 !
 if(it_now>=Thermal_steps+NE_steps) then
   !      
   call RT_free()
   !
   call WF_free()
   !
   call X_alloc("P_square")
   !
   call RT_report(file_name,.false.)
   !
   return
   !
 endif
 !
 if(.not.local_V) call load_RHO_VX_RHO()
 !
 call RT_report(file_name,.true.)
 !
 if(Thermal_steps/=0.and.it_now<=Thermal_steps) then
   ! 
   call live_timing('[RT] Thermalization :',Thermal_steps-it_now+1,SERIAL=.true.)
   !
 elseif(it_now>Thermal_steps+1) then
   !
   call live_timing('[RT] Iteration :',NE_steps-it_now+1,SERIAL=.true.)
   !
 endif
 !
 ! loop on time
 !
 do it=it_now,Thermal_steps+NE_steps
   !
   ! i_time: integer index of the time staring from RT_time>0 
   !         after the Thermalization, used for A_t and J_current
   !
   i_time=it-Thermal_steps
   RT_time=(i_time-1)*RT_step
   !
   if(it==Thermal_steps+1) then
     call live_timing('[RT] Iteration :',NE_steps,SERIAL=.true.)
     if(LifeTime/=0.0) G_lesser_reference=G_lesser
   endif
   !
   call live_timing(steps=1)
   !
   if(it==Thermal_steps) call live_timing
   !
   ! Copy G_lesser in G_old before start collisin
   !
   ! Simmetrization
   !
   G_lesser_previous=G_lesser
   forall( i1=1:SC_bands )  G_lesser_previous(i1,i1,:)=(0._SP,1._SP)*aimag(G_lesser(i1,i1,:))
   !
   call RT_Collision(G_lesser_previous,Xen,k,q,RT_time)
   !
   if(l_NE_with_fields.and.i_time>0)  then
     A_vecpot_previous=A_vecpot
     call RT_current(xK,Xen,G_lesser_previous,RT_time)
   endif
   !
   call RT_Energy(Xen,k,E_kin,E_ion,E_h,E_x,E_xc,E_mg,Trace_Gf)
   !
   ! Now print the Energy and other stuff
   !
   values(1:8)=(/RT_time/FS2AUT,real(E_kin,SP),real(E_ion,SP),real(E_h,SP),&
&              real(E_x,SP),real(E_xc,SP),real(E_mg,SP),real(Trace_Gf,SP)/)
   call msg('o dynamics','',values(1:8),USE_TABS=.TRUE.)
   !
   if(l_RT_diagonalize) call RT_qp_analyze(it,en,k,Xk,G_lesser)
   !
   if(LifeTime/=0.0.and.it<Thermal_steps.and.mod(it,RT_diagonalization_steps)==0) then
     call RT_update_G_reference(Xen,G_lesser_reference)
   endif   
   !
   ! Integrate equation of motion
   !
   call RT_Integrator(G_lesser,G_lesser_previous,A_vecpot,A_vecpot_previous,RT_step,Xen,k,q,Integrator_name,RT_time)
   !
   ! Write last G_lesser on disk
   !
   it_now=it ! used in RESTART
   !
   call RT_write_dbs(Xen,it)
   !
 enddo
 !
 if(NE_steps/=0) call live_timing
 !
 call of_open_close(file_name(1))
 if(l_RT_diagonalize) call of_open_close(file_name(2)) 
 !
 ! Finalize
 !-----------
 !
 call RT_free()
 !
 call WF_free()
 !
 if(.not.local_V) call free_RHO_VX_RHO()
 !
 call X_alloc("P_square")
 !
 call set_real_printed_length()
 !
end subroutine RT_driver

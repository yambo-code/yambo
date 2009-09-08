!
! Copyright (C) 2000-2008 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_driver(en,Xen,X,k,xk,q)
 !
 use pars,           ONLY:SP,lchlen,schlen
 use drivers,        ONLY:l_sc_hf
 use stderr,         ONLY:set_real_printed_length
 use units,          ONLY:FS2AUT
 use com,            ONLY:msg,of_open_close
 use timing,         ONLY:live_timing 
 use electrons,      ONLY:levels
 use X_m,            ONLY:X_t,Eval_P_and_P2_only,X_alloc
 use wave_func,      ONLY:wf,wf_load,wf_ng,WF_free
 use R_lattice,      ONLY:bz_samp,qindx_S
 use QP_m,           ONLY:QP_nk
 use SC,             ONLY:SC_potential,SC_bands,Ho,E_bare,it_now
 use IO_m,           ONLY:io_control,OP_WR,WR_CL_IF_END,NONE,WR,REP
 use real_time,      ONLY:Thermal_steps,Tot_steps,RT_step,G_lesser,l_RT_no_updates, &
&                         G_lesser_previous,l_rt_efield,RT_alloc,RT_free,io_G,ID,   &
&                         i_time,Integrator_name,j_current,l_qp_analyze,     &
&                         G_lesser_reference,LifeTime
 !
 implicit none
 !
 ! if l_qp_analyze=.true.  "en"  will contains new energies and occupations
 ! otherwise it will not be used
 !
 ! Notice that Xen is never changed!
 !
 type(levels)  :: en,Xen
 !
 type(bz_samp) :: k,Xk,q
 type(X_t):: X(4)
 integer, external :: io_SC_components
 ! 
 ! Work Space
 !
 character(lchlen)        :: file_name(2)
 character(schlen)        :: sec_mode_and_message(2)
 integer                  :: WF_G_max,WF_Go_indx,it,i1,ik
 real(SP)                 :: values(9), E_xc, RT_time  ! The real time on the diagonal RTime = it*RT_step    
 complex(SP)              :: Trace_Gf,E_kin,E_ion,E_h 
 logical                  :: XC_switch
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
 l_RT_no_updates=.not.XC_switch()
 !
 ! WFs loading
 !
 WF_G_max=wf_ng
 if(l_sc_hf) then
   WF_Go_indx=maxval(qindx_S(:,:,2))
   sec_mode_and_message(1)='*'
   sec_mode_and_message(2)='Hartree-Fock extended oscillators'  
   call section(trim(sec_mode_and_message(1)),trim(sec_mode_and_message(2)))
   call QP_state_table_setup(Xen)
   call XCo_Hartree_Fock(Xen, k, Xk, q)
 else
   WF_Go_indx=1  
 endif
 !
 call wf_load(WF_G_max,WF_Go_indx,(/1,SC_bands/),(/1,k%nibz/),space='R',title=' ')  
 !
 ! Core initialization
 !
 call RT_initialize()
 !
 ! Core allocations
 !
 call RT_alloc(Xen)
 !
 ! Initialize the Gf and potentials
 !----------------------------------
 ! G_lesser(T=0,T=0)=Go and G_lesser_old(T=0,T=0)=Go
 !
 G_lesser=(0._SP,0._SP)
 ! 
 forall(i1=1:SC_bands,ik=1:Xen%nk)
   G_lesser(i1,i1,ik)=(0._SP,1._SP)*Xen%f(i1,ik,1)
 end forall
 !
 ! Build Ho
 !
 E_bare=en%E(:SC_bands,:,:)
 call SC_common_build_Ho(Xen,Xk,k)
 !
 ! Restart from a previous RT run 
 ! or from a SC calculation and fill 
 ! G_lesser,G_lesser_previous,G_lesser_reference
 ! 
 call RT_restart(Xen)
 !
 ! Calculation already done
 !
 if(it_now>=Thermal_steps+Tot_steps) then
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
 call RT_report(file_name,.true.)
 !
 if(l_RT_no_updates) call wf_free()
 !
 if(Thermal_steps/=0.and.it_now<=Thermal_steps) then
   ! 
   call live_timing('[RT] Thermalization :',Thermal_steps-it_now+1,SERIAL=.true.)
   !
 elseif(it_now>Thermal_steps+1) then
   !
   call live_timing('[RT] Iteration :',Tot_steps-it_now+1,SERIAL=.true.)
   !
 endif
 !
 ! loop on time
 !
 do it=it_now,Thermal_steps+Tot_steps
   !
   ! i_time: integer index of the time staring from RT_time>0 
   !         after the Thermalization, used for A_t and J_current
   !
   if(it>Thermal_steps) i_time=it-Thermal_steps  
   !
   RT_time=(it-Thermal_steps)*RT_step
   !
   if(it==Thermal_steps+1) call live_timing('[RT] Iteration :',Tot_steps,SERIAL=.true.)
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
   call RT_Energy(Xen,k,E_kin,E_ion,E_h,E_xc,Trace_Gf)
   !
   ! Now print the Energy and other stuff
   !
   values(1:6)=(/RT_time/FS2AUT,real(E_kin),real(E_ion),real(E_h) , &
&                E_xc,& ! calculated in xc_lda_driver()
&                real(Trace_Gf)/)
   call msg('o dynamics','',values(1:6),USE_TABS=.TRUE.)
   !
   if(l_qp_analyze) call RT_qp_analyze(it,en,k,Xk,G_lesser)
   !
   if(it==Thermal_steps.and.Thermal_steps/=0.and.LifeTime/=0.0) &
     &  G_lesser_reference=G_lesser
   !
   if(l_rt_efield.and.i_time>0)  call RT_current(j_current,xK,Xen,G_lesser)
   !
   ! Integrate equation of motion
   !
   call RT_Integrator(G_lesser,G_lesser_previous,RT_step,Xen,k,q,Integrator_name,RT_time)
   !
   ! Write last G_lesser on disk
   !
   it_now=it ! used in RESTART
   !
   if (it==1) call io_control(ACTION=OP_WR,COM=REP, SEC=(/1/),ID=ID(5))
   if (it> 1) call io_control(ACTION=OP_WR,COM=NONE,SEC=(/1/),ID=ID(5))
   io_G=io_SC_components('G' ,Xen,ID(5))
   !
   do ik = 1, QP_nk 
     call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID(5))
     io_G=io_SC_components('G',Xen,ID(5))
   enddo
   !
 enddo
 !
 if(Tot_steps/=0) call live_timing
 !
 call of_open_close(file_name(1))
 if(l_qp_analyze) call of_open_close(file_name(2)) 
 !
 ! Finalize
 !-----------
 !
 call RT_free()
 !
 call WF_free()
 !
 call X_alloc("P_square")
 !
 call set_real_printed_length()
 !
end subroutine RT_driver

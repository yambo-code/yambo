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
 use pars,           ONLY:SP,lchlen
 use drivers,        ONLY:l_sc_hf
 use stderr,         ONLY:set_real_printed_length
 use units,          ONLY:FS2AUT,HARTREE
 use com,            ONLY:msg,of_open_close,warning
 use timing,         ONLY:live_timing 
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:levels,n_sp_pol
 use X_m,            ONLY:X_t,Eval_P_and_P2_only,X_alloc
 use wave_func,      ONLY:wf,wf_load,wf_ng,WF_free
 use R_lattice,      ONLY:bz_samp,qindx_S
 use QP_m,           ONLY:QP_nb,QP_nk,QP_ng_Sx
 use SC,             ONLY:SC_potential,SC_bands,Ho,E_bare,SC_R,E_convergence,&
&                         rho_convergence,H_rotate
 use real_time,      ONLY:Thermal_steps,Tot_steps,RT_step,G_lesser,l_RT_no_updates, &
&                         G_lesser_previous,l_rt_efield,RT_alloc,RT_free, &
&                         A_vec_pot,A_norm,l_eps_analyze,i_time,Integrator_name,&
&                         j_current,l_qp_analyze,I1_lesser,rho_rho_gamp, &
&                         G_lesser_reference,QP_Hartree,l_fast_exchange
 use IO_m,           ONLY:OP_RD,OP_RD_CL,RD_CL_IF_END,io_control,REP,VERIFY,&
&                         OP_WR,WR_CL_IF_END,NONE,WR,OP_WR_CL   
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
 character(lchlen)        :: file_name(3)
 integer                  :: WF_G_max,WF_Go_indx,it,i1,ik
 real(SP)                 :: values(9), E_xc, RT_time  ! The real time on the diagonal RTime = it*RT_step    
 complex(SP)              :: Trace_Gf,E_kin,E_ion,E_h 
 logical                  :: XC_switch
 integer                  :: ID(5),io_E,io_V,io_WF,io_J,io_G
 logical                  :: RESTART,RESTART_FROM_SC
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
 else
   WF_Go_indx=1  
 endif
 call wf_load(WF_G_max,WF_Go_indx,(/1,SC_bands/),(/1,k%nibz/),space='R',title=' ')  
 !
 ! Core initialization
 !
 call RT_initialize(file_name,Xen)
 !
 ! Core allocations
 !
 call RT_alloc(Xen)
 !
 ! Perform a first I/O to check which DBs are present. 
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(1)) ! Energies
 io_E=io_SC_components('E' ,Xen,ID(1))
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2)) ! WFs
 io_WF=io_SC_components('WF',Xen,ID(2))
 call io_control(ACTION=OP_RD_CL,COM=REP ,SEC=(/1/),MODE=VERIFY,ID=ID(3)) ! Local potential
 io_V=io_SC_components('V' ,Xen,ID(3))
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(4)) ! Current response
 io_J=io_SC_components('J' ,Xen,ID(4))
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(5)) ! G_lesser
 io_G=io_SC_components('G' ,Xen,ID(5))
 !
 ! To restart from RT only the G_lesser is necessary
 !
 if(l_rt_efield) then
   RESTART        = all((/io_G==0, io_J==0/))
 else
   RESTART        = io_G==0
 endif
 !
 RESTART_FROM_SC=all((/io_E==0,io_WF==0,io_V==0,io_G/=0,io_J/=0/))
 !
 ! Initialize the Gf and potentials
 !----------------------------------
 ! G_lesser(T=0,T=0)=Go and G_lesser_old(T=0,T=0)=Go
 !
 G_lesser=(0._SP,0._SP)
 G_lesser_previous =(0._SP,0._SP)
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
 if(RESTART_FROM_SC) then
   !
   !  I only need SC_R than I can reconstruct G_lesser(:,:,:)
   !  rotating the non-integrating G_lesser
   !
   allocate(SC_R(SC_bands,SC_bands,k%nibz,n_sp_pol))
   call mem_est("SC_R ",(/size(SC_R)/))
   !
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2))
   io_WF=io_SC_components('WF',Xen,ID(2))
   !
   do ik=1,QP_nk
     call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID(2))
     io_WF=io_SC_components('WF',Xen,ID(2))
   enddo
   !
   call msg('nr','[RT] Restart from a Self-Consistent Calculation ')
   call msg('nr','[RT] Found   accuracy on density      :',rho_convergence)
   call msg('rn','[RT]                  on energies [ev]:',E_convergence*HARTREE)
   !
   if ( E_convergence>0.01.and.rho_convergence>1e-4) &
&    call warning(" Restart from a non-converged Self-Consistent calculation! ")
   !
   if ( Thermal_steps/=0) &
&    call warning(" Restart from a SC could not work with thermalization! ")
   !
   ! Now I build the G_lesser rotating the non interaction one
   ! G_lesser(i1,i1,ik)=(0.,1)*en%f(i1,ik)
   !
   do ik=1,QP_nk
     call H_rotate(SC_R(:,:,ik,1),G_lesser(:,:,ik),SC_bands,-1)
   enddo
   !
 endif
 !
 if(RESTART) then
   !
   !  I use G_lesser(:,:,:) to reconstruct everything
   !
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(5))
   io_G=io_SC_components('G',Xen,ID(5))
   !
   do ik=1,QP_nk
     call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID(5))
     io_G=io_SC_components('G',Xen,ID(5))
   enddo
   !
 endif 
 !
 ! Initialize G_lesser_previous and G_lesser_reference
 !
 G_lesser_previous =G_lesser   
 G_lesser_reference=G_lesser
 !
 if(l_sc_hf) then
   !
   ! Build rho_rho_gamp(:,:,:,:,nk)
   !
   if(QP_ng_Sx>wf_ng) QP_ng_Sx=wf_ng 
   !
   call IO_mute("report io_in io_out")
   call QP_XX_Vxc(Xen,k,Xk,q,G_lesser,.true.)
   call IO_mute("  ")
   !
 endif
 !
 if(l_RT_no_updates) call wf_free()
 !
 ! Real zero of the time                   
 !
 if(Thermal_steps/=0) call live_timing('[RT] Thermalization :',Thermal_steps)
 !
 do it=1,Thermal_steps+Tot_steps
   !
   ! i_time: integer index of the time staring from RT_time>0 
   !         after the Thermalization, used for A_t and J_current
   !
   if(it>Thermal_steps) i_time=it-Thermal_steps  
   !
   RT_time=(it-Thermal_steps)*RT_step
   !
   if(it==Thermal_steps+1) call live_timing('[RT] Iteration :',Tot_steps)
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
   if(it==Thermal_steps.and.Thermal_steps/=0) G_lesser_reference=G_lesser
   !
   if(l_rt_efield.and.i_time>0) then
     !
     call RT_current(j_current(i_time,:),xK,A_vec_pot(i_time,:),G_lesser)
     !
     values(2:3)=(/real(A_norm(i_time)),aimag(A_norm(i_time))/)
     values(4:6)=real(j_current(i_time,1:3))
     !
     call msg('o response','',values(1:6),USE_TABS=.TRUE.)
     !
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(4)) ! Current response
     io_J=io_SC_components('J' ,Xen,ID(4))
     !
   endif
   !
   call RT_Integrator(G_lesser,G_lesser_previous,RT_step,Xen,k,q,Integrator_name,RT_time)
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(4)) ! G_lesser
   io_G=io_SC_components('G' ,Xen,ID(5))
   !
 enddo
 !
 call of_open_close(file_name(1))
 !
 if(l_rt_efield)  call of_open_close(file_name(2))
 !        
 if(l_qp_analyze) call of_open_close(file_name(3)) 
 !
 ! Dielectric Function
 !---------------------
 !
 if (l_eps_analyze) call RT_eps_analyze(j_current,A_norm) 
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

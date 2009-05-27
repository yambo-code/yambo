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
 use stderr,         ONLY:set_real_printed_length
 use units,          ONLY:FS2AUT
 use com,            ONLY:msg,of_open_close
 use timing,         ONLY:live_timing 
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:levels,n_sp_pol
 use X_m,            ONLY:X_t,Eval_P_and_P2_only,X_alloc
 use wave_func,      ONLY:wf,wf_load,wf_ng,WF_free
 use R_lattice,      ONLY:bz_samp
 use QP_m,           ONLY:QP_nb,QP_nk
 use SC,             ONLY:SC_potential,SC_bands,Ho,E_bare
 use real_time,      ONLY:Thermal_steps,Tot_steps,RT_step,G_lesser, &
&                         G_lesser_previous,l_rt_efield,RT_alloc,RT_free, &
&                         A_vec_pot,A_norm,l_eps_analyze,i_time,Integrator_name,&
&                         j_current,l_qp_analyze,I1_lesser
 !
 implicit none
 !
 ! Notice that "en" never changes, it contains
 ! the bare occupation and bare energies
 !
 type(levels)  :: en
 type(levels)  :: Xen
 !
 type(bz_samp) :: k,Xk,q
 type(X_t):: X(4)
 ! 
 ! Work Space
 !
 character(lchlen)        :: file_name(3)
 integer                  :: WF_G_max,WF_Go_indx,it,i1,ik
 real(SP)                 :: values(9), E_xc, RT_time  ! The real time on the diagonal RTime = it*RT_step    
 complex(SP)              :: Trace_Gf,E_kin,E_ion,E_h 
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
 ! WFs loading
 !
 WF_G_max=wf_ng
 WF_Go_indx=1  
 call wf_load(WF_G_max,WF_Go_indx,(/1,SC_bands/),(/1,k%nibz/),space='R',title=' ')  
 !
 ! 2 CHANGE !!!!!!!!!!!!!!!!!!!!!!
 !
 Xen%nbm=SC_bands
 !
 ! Core initialization
 !
 call RT_initialize(file_name,Xen)
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
 G_lesser_previous =(0._SP,0._SP)
 ! 
 forall(i1=1:SC_bands,ik=1:Xen%nk)
   G_lesser(i1,i1,ik)=(0._SP,1._SP)*Xen%f(i1,ik,1)
 end forall
 !
 G_lesser_previous=G_lesser   
 !
 ! Build Ho
 !
 E_bare=en%E(:SC_bands,:,:)
 call SC_common_build_Ho(Xen,Xk,k)
 !
 ! Real zero of the time                   
 !
 if(Thermal_steps/=0) call live_timing('[RT] Thermalization :',Thermal_steps)
 !
 do it=1,Thermal_steps+Tot_steps
   !
   i_time=it
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
   if(l_qp_analyze) call RT_qp_analyze()
   !
   if(l_rt_efield) then
     !
     call RT_current(j_current(i_time,:),Xen,xK,A_vec_pot(i_time,:),G_lesser_previous)
     !
     values(2:3)=(/real(A_norm(i_time)),aimag(A_norm(i_time))/)
     values(4:6)=real(j_current(i_time,1:3))
     !
     call msg('o response','',values(1:6),USE_TABS=.TRUE.)
     !
   endif
   !
   call RT_Integrator(G_lesser,G_lesser_previous,RT_step,Xen,k,q,Integrator_name,RT_time)
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

!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
 use pars,           ONLY:SP
 use stderr,         ONLY:set_real_printed_length
 use units,          ONLY:FS2AUT
 use timing,         ONLY:live_timing 
 use electrons,      ONLY:levels
 use X_m,            ONLY:X_t,X_alloc
 use wave_func,      ONLY:WF_load,WF_free
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_potential,SC_bands,it_now,local_V,WF_G_max,WF_Go_indx
 use real_time,      ONLY:Thermal_steps,NE_steps,RT_step,G_lesser, &
&                         G_lesser_previous,l_NE_with_fields,RT_alloc,RT_free,      &
&                         i_time,Integrator_name,l_RT_diagonalize,        &
&                         G_lesser_reference,LifeTime,RT_diagonalization_steps,     &
&                         A_vecpot,A_vecpot_previous
 use collisions_CACHE, ONLY:free_COLLISIONS_v
 !
 implicit none
 !
 ! if l_RT_diagonalize=.true.  "en"  will contains new energies and occupations
 ! otherwise it will not be used
 !
 ! Notice that Xen is never changed!
 !
 type(levels)  :: en,Xen
 type(w_samp)  :: Xw(2) 
 type(bz_samp) :: k,Xk,q
 type(X_t):: X(4)
 ! 
 ! Work Space
 !
 integer                  :: it,i1
 real(SP)                 :: RT_time  ! The real time on the diagonal RTime = it*RT_step    
 complex(SP)              :: Trace_Gf,E_kin,E_ion,E_h,E_x,E_xc,E_mg 
 !
 call section('*',trim(SC_potential)//' Real-Time Dynamics')
 !##########################################################
 !
 call set_real_printed_length(f_length=10,g_length=10)  
 !
 ! Dipoles & WF dimensions
 !=========================
 ! 
 call SC_WF_and_dipole_dimensions(X(:2))
 !
 ! Dipoles
 !========
 !
 ! before loading the WF in R-space I check and/or calculate
 ! the optical collisions that are done in G-space and would
 ! force the reloading of the WFs
 !
 call Dipole_driver(Xen, Xk, X(1), X(1)%q0)
 !
 ! Switch 
 !=======
 !
 call XC_switch()
 !
 ! WFs loading
 !=============
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
   return
   !
 endif
 !
 if(.not.local_V) call load_COLLISIONS()
 !
 if(Thermal_steps/=0.and.it_now<=Thermal_steps) then
   ! 
   call live_timing('[RT] Thermalization :',Thermal_steps-it_now+1,SERIAL=.true.)
   !
 elseif(it_now>Thermal_steps+1) then
   !
   call live_timing('[RT] Dynamics :',NE_steps-it_now+1,SERIAL=.true.)
   !
 endif
 !
 ! Open the output files
 !
 call RT_report(0,spread(0.,1,4))
 if (l_RT_diagonalize) call SC_History(1,en)
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
     call live_timing('[RT] Dynamics :',NE_steps,SERIAL=.true.)
     if(LifeTime/=0.0) G_lesser_reference=G_lesser
   endif
   !
   call live_timing(steps=1)
   !
   if(it==Thermal_steps) call live_timing()
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
     call RT_current(xK,G_lesser_previous,A_vecpot)
   endif
   !
   call RT_Energy(Xen,k,E_kin,E_ion,E_h,E_x,E_xc,E_mg,Trace_Gf)
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
   ! Now print the Energy and other stuff
   !
   call RT_report(it,(/RT_time/FS2AUT,real(E_kin,SP),real(E_ion,SP),real(E_h,SP),&
&                    real(E_x,SP),real(E_xc,SP),real(E_mg,SP),real(Trace_Gf,SP)/))
   !
 enddo
 !
 if(NE_steps/=0) call live_timing
 !
 call RT_report(-1,spread(0.,1,4))
 if (l_RT_diagonalize) call SC_History(-1,en)
 !
 ! Finalize
 !-----------
 !
 call RT_free()
 !
 call WF_free()
 !
 if(.not.local_V) call free_COLLISIONS_v()
 !
 call X_alloc("P_square")
 !
 call set_real_printed_length()
 !
end subroutine RT_driver

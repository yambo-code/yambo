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
subroutine RT_start_and_restart(Xen,Xk,k)
 !
 ! This subroutine fills: 
 ! G_lesser,G_lesser_previsous,G_lesser_reference,
 ! XC0,V_xc_sc,V_hartree_sc
 !
 use pars,           ONLY:SP
 use com,            ONLY:msg,warning
 use memory_m,       ONLY:mem_est
 use drivers,        ONLY:l_sc_contains_lda
 use QP_m,           ONLY:QP_nk
 use units,          ONLY:HARTREE
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:G_lesser,ID,ID,NE_steps,G_lesser_previous,j_current,&
&                         G_lesser_reference,Thermal_steps,LifeTime,XC0,      &
&                         l_RT_needs_XC0,l_NE_simulate_IP,Hfrozen,XCfrozen,   &
&                         A_vecpot,A_vecpot_previous,Efield,l_NE_with_fields
 use SC,             ONLY:SC_bands,SC_R,E_convergence,it_now,H_nl_sc,local_V, &
&                         rho_convergence,H_rotate,local_V,V_hartree_sc,      &
&                         E_bare,V_xc_sc,rho_n
 use IO_m,           ONLY:OP_RD,OP_RD_CL,RD_CL_IF_END,io_control,REP,VERIFY,NONE
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 use wave_func,      ONLY:wf
 use xc_functionals, ONLY:V_xc
 implicit none
 !
 type(levels)      :: Xen
 type(bz_samp)     :: Xk,k
 integer, external :: io_SC_components
 !
 ! Work Space
 !
 integer :: ik,it,i1
 integer :: io_E,io_V,io_WF,io_G,io_J
 logical :: RESTART
 logical :: LOAD_SC_XC,SC_DB_present
 !
 ! Initialize the Gf and potentials
 !----------------------------------
 ! G_lesser(T=0,T=0)=Go and G_lesser_old(T=0,T=0)=Go
 !
 G_lesser          =(0._SP,0._SP)
 V_xc_sc           =(0._SP)
 V_hartree_sc      =(0._SP)
 !
 ! Set to zero current and fields
 !
 j_current          =(0._SP,0._SP)
 A_vecpot%vecpot    =(0._SP,0._SP)
 A_vecpot%vecpot_vel=(0._SP,0._SP)
 A_vecpot%vecpot_acc=(0._SP,0._SP)
 ! 
 forall(i1=1:SC_bands,ik=1:Xen%nk)
   G_lesser(i1,i1,ik)=(0._SP,1._SP)*Xen%f(i1,ik,1)
 end forall
 !
 ! Build Ho and save initial energies
 !
 E_bare=Xen%E(:SC_bands,:,:)
 call SC_bare_Hamiltonian(Xen,Xk,k)
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
 RESTART       = io_G ==0
 !
 SC_DB_present =  all((/io_E==0,io_WF==0,io_V==0/))
 !
 if(SC_DB_present) then
   !
   call msg('snr','[RT] Restart from a Self-Consistent Calculation ')
   call msg('nr' ,'[RT] Found   accuracy on density      :',rho_convergence)
   call msg('rn' ,'[RT]                  on energies [ev]:',E_convergence*HARTREE)
   !
   if ( E_convergence>0.01.and.rho_convergence>1e-4) &
&    call warning(" Restart from a non-converged Self-Consistent calculation! ")
   !
   !  I only need SC_R than I can reconstruct G_lesser(:,:,:)
   !  rotating the non-integrating G_lesser
   !
   allocate(SC_R(SC_bands,SC_bands,QP_nk,n_sp_pol))
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
   ! Now I build the G_lesser rotating the non interaction one
   ! G_lesser(i1,i1,ik)=(0.,1)*en%f(i1,ik)
   !
   do ik=1,QP_nk
     call H_rotate(SC_R(:,:,ik,1),G_lesser(:,:,ik),SC_bands,-1)
   enddo
   !
   deallocate(SC_R)
   call mem_est("SC_R ")
   !
   if(Hfrozen) call el_density(G_lesser,k,rho_n,.false.)
   if(Hfrozen) call V_Hartree(rho_n,V_hartree_sc)
   !
   V_xc_sc=(0._SP)
   !
   if(l_RT_needs_XC0) then
     !
     XC0=(0._SP,0._SP)
     !
     if (local_V) then 
       call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID(3))  ! Local potential
       io_V=io_SC_components('V' ,Xen,ID(3))
     else
       call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID(3))  ! Non Local potential
       io_V=io_SC_components('V' ,Xen,ID(3))
     endif
     !
     do ik=1,QP_nk
       if (local_V) then
         call V_real_space_to_H(ik,V_hartree_sc+V_xc,XC0(:,:,ik,:),wf)
       else
         call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID(3))
         io_V=io_SC_components('V',Xen,ID(3))
         XC0(:,:,ik,:)=H_nl_sc(:,:,:)
       endif
     enddo
   endif
   !
 else if (l_RT_needs_XC0) then
   !
   XC0=(0._SP,0._SP)
   !
   call xc_lda_driver(Xen,Xk,WF_KIND,WF_xc_functional,1,G_lesser)
   do ik=1,QP_nk
     call V_real_space_to_H(ik,V_xc,XC0(:,:,ik,:),wf) 
   enddo
   !
   V_xc_sc=(0._SP)
   !
 endif
 !
 ! Starting point 
 !
 it_now=1
 !
 if(RESTART) then
   !
   !  I use G_lesser(:,:,:) to reconstruct everything
   !  and I read it_now from the databases
   !
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(5))
   io_G=io_SC_components('G',Xen,ID(5))
   !
   do ik=1,QP_nk
     call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),MODE=VERIFY,ID=ID(5))
     io_G=io_SC_components('G',Xen,ID(5))
   enddo
   !
   if(io_G==0.and.it_now<NE_steps+Thermal_steps) then
     !
     call msg('snr','[RT] Restart from a Real-Time Calculation ')
     call msg('snr','[RT] Number of steps done         :',it_now)
     !
   endif
   !
   it_now=it_now+1
   !
 else
   !
   if(l_NE_with_fields) call RT_init_vecpot()
   !
 endif
 !
 ! Initialize G_lesser_previous and G_lesser_reference
 !
 A_vecpot_previous =A_vecpot
 G_lesser_previous =G_lesser   
 if(LifeTime/=0.0) G_lesser_reference=G_lesser   
 !
end subroutine RT_start_and_restart

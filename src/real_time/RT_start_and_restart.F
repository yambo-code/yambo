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
subroutine RT_start_and_restart(Xen,Xk,k,q,X)
 !
 ! This subroutine initialize some variables and restart from SC
 ! or from a previous RT calculation:
 !
 ! [1] Initialize the Gf and potentials
 ! [2] Build Ho
 ! [3] Check for SC and RESTART DBs
 !  |
 !  .> Use SC as a starting point
 !  |
 !  .> RESTART from a previous run
 ! [4] Initialize the bare Hartree and XC
 ! [5] Initialize the fields
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_sc_cohsex,l_sc_hf
 use com,            ONLY:msg,warning,error
 use QP_m,           ONLY:QP_nk,QP_Sc,QP_Vnl_xc,COHSEX_use_empties
 use units,          ONLY:HARTREE
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:G_lesser,ID,ID,NE_steps,G_lesser_previous,RT_ind_J,&
&                         G_lesser_reference,Thermal_steps,l_NE_simulate_IP,&
&                         l_RT_needs_XC0,Hfrozen,it_start,l_NE_with_fields,XC0_and_HARTREE0,&
&                         l_NE_simulate_BSE,RT_P,RT_step,RT_update,RT_P_2levels
 use fields,         ONLY:A_vecpot,A_vecpot_previous
 use SC,             ONLY:SC_bands,SC_R,E_convergence,it_now,H_nl_sc,local_V, &
&                         rho_convergence,H_rotate,local_V,V_hartree_sc,      &
&                         E_bare,V_xc_sc,rho_n
 use IO_m,           ONLY:OP_RD,OP_RD_CL,RD_CL_IF_END,io_control,REP,VERIFY,NONE
 use X_m,            ONLY:X_t
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 use wave_func,      ONLY:wf
 use xc_functionals, ONLY:V_xc
 implicit none
 !
 type(levels)      :: Xen
 type(bz_samp)     :: Xk,k,q
 type(X_t)         :: X
 integer, external :: io_SC_components
 !
 ! Work Space
 !
 integer :: ik,it,i1,COM_
 integer :: io_E,io_V,io_WF,io_G,io_J_and_P
 logical :: RESTART
 logical :: SC_DB_present
 !
 ! Initialize the Gf and potentials
 !==================================
 !
 ! G_lesser(T=0,T=0)=Go and G_lesser_old(T=0,T=0)=Go
 !
 G_lesser          =(0._SP,0._SP)
 V_xc_sc           =(0._SP)
 V_hartree_sc      =(0._SP)
 H_nl_sc           =(0._SP,0._SP)
 !
 ! Set to zero current and fields
 !
 RT_P               =(0._SP,0._SP)
 RT_P_2levels       =(0._SP,0._SP)
 RT_ind_J           =(0._SP,0._SP)
 ! 
 forall(i1=1:SC_bands,ik=1:Xen%nk)
   G_lesser(i1,i1,ik)=(0._SP,1._SP)*Xen%f(i1,ik,1)
 end forall
 !
 ! Build Ho and save initial energies 
 !==========
 !
 E_bare=Xen%E(:SC_bands,:,:)
 !
 call SC_bare_Hamiltonian(Xen,Xk,k)
 !
 ! Perform a first I/O to check which DBs are present. 
 !=====================
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(1)) ! Energies
 io_E=io_SC_components('E' ,Xen,ID(1))
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2)) ! WFs
 io_WF=io_SC_components('WF',Xen,ID(2))
 call io_control(ACTION=OP_RD_CL,COM=REP ,SEC=(/1/),MODE=VERIFY,ID=ID(3)) ! Local potential
 io_V=io_SC_components('V' ,Xen,ID(3))
 !
 COM_=REP
 call io_control(ACTION=OP_RD_CL,COM=COM_,SEC=(/1/),MODE=VERIFY,ID=ID(4)) ! Current response
 io_J_and_P=io_SC_components('J_and_P' ,Xen,ID(4))
 if (io_J_and_P==0) COM_=NONE
 call io_control(ACTION=OP_RD_CL,COM=COM_,SEC=(/1/),MODE=VERIFY,ID=ID(5)) ! G_lesser
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
   ! RESTART FROM SC
   !=================
   !
   call msg('snr','[RT] Restart from a Self-Consistent Calculation ')
   call msg('nr' ,'[RT] Found   accuracy on density      :',rho_convergence)
   call msg('rn' ,'[RT]                  on energies [ev]:',E_convergence*HARTREE)
   !
   if ( E_convergence>0.01.and.rho_convergence>1e-4) &
&    call warning(" Restart from a non-converged Self-Consistent calculation! ")
   !
   if(.not.COHSEX_use_empties) &
&    call warning(" Set UseEbands=.true. to be full compatible with RT!! ")
   !
   !  I only need SC_R than I can reconstruct G_lesser(:,:,:)
   !  rotating the non-integrating G_lesser
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
     call H_rotate(SC_R(1:SC_bands,1:SC_bands,ik,1),G_lesser(1:SC_bands,1:SC_bands,ik),SC_bands,-1)
   enddo
   !
   if (Hfrozen) call el_density(G_lesser,k,rho_n,.false.)
   if (Hfrozen) call V_Hartree(rho_n,V_hartree_sc)
   !
   V_xc_sc=(0._SP)
   !
   if(l_RT_needs_XC0) then
     !
     XC0_and_HARTREE0=(0._SP,0._SP)
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
         call V_real_space_to_H(ik,V_hartree_sc+V_xc,XC0_and_HARTREE0(1:SC_bands,1:SC_bands,ik,1:n_sp_pol),wf)
       else
         call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID(3))
         io_V=io_SC_components('V',Xen,ID(3))
         XC0_and_HARTREE0(1:SC_bands,1:SC_bands,ik,1:n_sp_pol)=H_nl_sc(1:SC_bands,1:SC_bands,1:n_sp_pol)
       endif
     enddo
   endif
   !
   ! Now I update <i|r|j> dipoles using G_lesser_reference as
   ! Green function to construct the Hamiltonian
   !
   if(.not.l_NE_simulate_BSE) call RT_update(G_lesser,SC_R,-1,Xen,k,Xk,X,up_DIP=.true.)
   call RT_update(G_lesser_reference,SC_R,-1,Xen,k,Xk,X,up_Gref=.true.)
   !
   if(l_NE_simulate_IP.or.l_NE_simulate_BSE) then 
     !
     do ik=1,QP_nk
       call V_real_space_to_H(ik,V_hartree_sc,XC0_and_HARTREE0(1:SC_bands,1:SC_bands,ik,1:n_sp_pol),wf) 
     enddo
     !
   endif
   !
   if(l_NE_simulate_IP) then 
     !
     V_xc_sc=(0._SP)
     V_hartree_sc=(0._SP)
     !
   endif
   !
 endif
 !
 if(.not.SC_DB_present.and.l_RT_needs_XC0) then
   !
   ! Build Hartree and XCo 
   !=======================
   !
   XC0_and_HARTREE0=(0._SP,0._SP)
   ! 
   call el_density(G_lesser,Xk,rho_n,.false.)
   !
   call IO_and_Messaging_switch("-report -log")
   call XC_potential_driver(Xen,Xk,WF_KIND,WF_xc_functional,1)
   call IO_and_Messaging_switch("+report +log")
   !
   ! In the IP  case XC0_and_HARTREE0  contains <mk | Vxc(rho_0) + Vhartree(rho_0) | nk>
   !
   ! in the RPA case XC0_and_HARTREE0  contains only  <mk | Vxc(rho_0) | nk>
   !
   do ik=1,QP_nk
     if(l_NE_simulate_IP) then   
       call V_real_space_to_H(ik,V_xc,XC0_and_HARTREE0(1:SC_bands,1:SC_bands,ik,1:n_sp_pol),wf) 
       call V_real_space_to_H(ik,V_hartree_sc,XC0_and_HARTREE0(1:SC_bands,1:SC_bands,ik,1:n_sp_pol),wf) 
     else
       call V_real_space_to_H(ik,V_xc,XC0_and_HARTREE0(1:SC_bands,1:SC_bands,ik,1:n_sp_pol),wf) 
     endif
   enddo
   !
   V_xc_sc=(0._SP)
   if(l_NE_simulate_IP) V_hartree_sc=(0._SP)
   !
   if(l_NE_simulate_BSE) then
     !
     if(.not.l_sc_cohsex.and..not.l_sc_hf) call error(" RTBSEOnly works only with Potantial=COHSEX or HARTREE-FOCK !! ")
     !
     call COLLISIONS_IO_compose(G_lesser) 
     !
     do ik=1,QP_nk
       call V_qp_basis_to_H(ik,-QP_Vnl_xc,XC0_and_HARTREE0(1:SC_bands,1:SC_bands,ik,1:n_sp_pol)) 
       if(l_sc_cohsex) call V_qp_basis_to_H(ik,-QP_Sc,XC0_and_HARTREE0(1:SC_bands,1:SC_bands,ik,1:n_sp_pol))
     enddo
     !
   endif
   !
 endif
 !
 ! Starting point 
 !
 it_start=1
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
     call msg('snr','[RT] Number of steps done    [o/o]:',real(it_now)/real(NE_steps)*100.)
   endif
   !
   it_start=it_now+1
   !
 endif
 !
 ! Initialize G_lesser_previous and G_lesser_reference
 !
 if (l_NE_with_fields) call RT_Ext_fields(A_vecpot,(it_start-1)*RT_step)
 !
 A_vecpot_previous =A_vecpot
 G_lesser_previous =G_lesser   
 G_lesser_reference=G_lesser   
 !
 call msg('r','')
 !
 ! Two Levels related initializations
 !====================================
#if defined _TWO_LEVELS
 if(RESTART) RESTART=.FALSE.
 call RT_init_two_levels(Xen)
#endif
 !
end subroutine RT_start_and_restart

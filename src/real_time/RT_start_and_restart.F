!
! Copyright (C) 2000-2012 A. Marini, C. Attaccalite and the YAMBO team 
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
 use drivers,        ONLY:l_sc_contains_lda
 use com,            ONLY:msg,warning,error
 use QP_m,           ONLY:QP_nk,COHSEX_use_empties
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:G_lesser,ID,ID,NE_steps,RT_ind_J,    &
&                         G_lesser_reference,Thermal_steps,l_NE_simulate_IP,rho_reference,  &
&                         l_RT_needs_XC0,Hfrozen,it_start,l_NE_with_fields,XC0_and_HARTREE0,&
&                         l_NE_simulate_BSE,RT_P,RT_step,RT_update,full_rho,   &
&                         l_NE_simulate_Irredux,l_NE_simulate_TDDFT,NE_i_time
 use fields,         ONLY:A_vecpot,A_vecpot_previous
 use SC,             ONLY:SC_bands,SC_R,E_convergence,it_now,H_nl_sc,local_V,       &
&                         rho_convergence,H_rotate,local_V,V_hartree_sc,&
&                         E_bare,V_xc_sc,rho_n,Ho,SC_xc_functional,&
&                         RT_nk,RT_k,RT_occupations,SC_nbands
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
 integer :: ik,i1,COM_,ib,io_E,io_V,io_WF,io_G,io_J_and_P
 logical :: RESTART,SC_DB_present
 !
 ! Initialize the Gf and potentials
 !==================================
 !
 ! G_lesser(T=0,T=0)=Go and G_lesser_old(T=0,T=0)=Go
 !
 G_lesser          =(0._SP,0._SP)
 G_lesser_reference=(0._SP,0._SP)
 rho_reference     =(0._SP)
 if(allocated(full_rho)) full_rho=(0._SP,0._SP)
 V_xc_sc           =(0._SP)
 V_hartree_sc      =(0._SP)
 H_nl_sc           =(0._SP,0._SP)
 !
 ! Set to zero current and fields
 !
 RT_P               =(0._SP,0._SP)
 RT_ind_J           =(0._SP,0._SP)
 ! 
 forall(i1=SC_bands(1):SC_bands(2),ik=1:Xen%nk)
   G_lesser(i1,i1,ik,1) =(0._SP,1._SP)*Xen%f(i1,ik,1)
   RT_occupations(i1,ik)=Xen%f(i1,ik,1)
 end forall
 !
 ! Build Ho and save initial energies 
 !====================================
 !
 E_bare=Xen%E(:SC_bands(2),:,:)
 !
 call SC_bare_Hamiltonian(Xen,Xk,k)
 !
 ! Save a copy of the k-grid
 !===========================
 ! 
 RT_nk=Xk%nibz
 allocate(RT_k(RT_nk,3))
 RT_k=Xk%pt
 !
 ! Perform a first I/O to check which DBs are present. 
 !=====================================================
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
   ! BEGIN RESTART FROM SC
   !=======================
   !
   call msg('snr','[RT] Restart from a Self-Consistent Calculation ')
   call msg('nr' ,'[RT] Found   accuracy on density      :',rho_convergence)
   call msg('rn' ,'[RT]                  on energies [ev]:',E_convergence*HA2EV)
   !
   if ( E_convergence>0.01.and.rho_convergence>1e-4) &
&    call warning(" Restart from a non-converged Self-Consistent calculation! ")
   !
   if(.not.COHSEX_use_empties) &
&    call error(" Set UseEbands=.true. in self-consistent GW to be full compatible with RT!! ")
   !
   if(l_NE_simulate_BSE) call error(" Real-time BSE not compatible with SC restart yet! ")
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
     call H_rotate(SC_R(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1),G_lesser(:,:,ik,1),SC_nbands,-1)
   enddo
   !
   if (Hfrozen) call el_density(G_lesser(:,:,:,1),Xen,k,rho_n,1,.false.)
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
       if (local_V.and..not.l_NE_simulate_Irredux) then
         call V_real_space_to_H(ik,V_xc,XC0_and_HARTREE0(1:SC_bands(2),1:SC_bands(2),ik,1:n_sp_pol),wf)
       elseif(.not.l_NE_simulate_Irredux) then
         call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID(3))
         io_V=io_SC_components('V',Xen,ID(3))
         XC0_and_HARTREE0(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1:n_sp_pol)=  &
&                         H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1:n_sp_pol)
       endif
       !
       ! If IP or Irreduxible the hartree term is fixed and stored in XC0_and_HARTREE0
       ! if I simulate BSE I have to remove the self-energy sigma(G_0)
       !
       if(l_NE_simulate_IP.or.l_NE_simulate_Irredux.or.l_NE_simulate_BSE) &
&         call V_real_space_to_H(ik,V_hartree_sc,XC0_and_HARTREE0(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2), &
&                                ik,1:n_sp_pol),wf) 
     enddo
     !
   endif
   !
   ! Now I update <i|r|j> dipoles using G_lesser_reference as
   ! Green function to construct the Hamiltonian
   !
   call RT_update(G_lesser(:,:,:,1),SC_R,-1,Xen,k,Xk,X,up_DIP=.true.)
   call RT_update(G_lesser_reference,SC_R,-1,Xen,k,Xk,X,up_Gref=.true.)
   !
   V_xc_sc=(0._SP)
   if(l_NE_simulate_IP.or.l_NE_simulate_Irredux) V_hartree_sc=(0._SP) 
   !
   ! END RESTART FROM SC
   !=====================
   !
 else if (Thermal_steps==0) then
   !
   ! During Thermalization G_lesser_reference must be zero
   !
   G_lesser_reference=G_lesser(:,:,:,1)
   !
 endif
 !
 if(.not.SC_DB_present.and.l_RT_needs_XC0) then
   !
   ! Build Hartree and XCo 
   !=======================
   !
   ! Note that 
   !  1. l_RT_needs_XC0 is .TRUE. only when we are starting a NE loop
   !  2. V_hartree and rho_full have been already calculated in SC_bare_Hamiltonian
   !
   ! The time evolution is done updating only the contribution
   ! coming from the bands above SC_lw_band in the density.
   ! Therefore we calculate:
   !
   ! (1) a density containing ALL levels (full_rho)
   ! (2) a density containing only levels above SC_lw_band (rho_reference)
   !
   ! in RT_Collision 
   !   V_h/xc[n]=V_h/xc[full_rho]+V_h/xc[rho-rho_reference]
   !
   ! When SC_lw_band=1 full_rho=rho_reference
   !
   ! The first term is stored here in XC0_and_HARTREE0
   !
   XC0_and_HARTREE0=(0._SP,0._SP)
   !
   ! Rho_reference. Note that altough rh_n has been already calcualted 
   ! in sc_bare_Hamiltonian I need to recalculate it here when SC_lw_band>1
   ! as it it is also used in XC_potential_driver
   ! 
   call el_density(G_lesser(:,:,:,1),Xen,Xk,rho_n,SC_bands(1),.false.)
   rho_reference=rho_n
   !
   if (allocated(full_rho)) then
     call el_density(G_lesser(:,:,:,1),Xen,Xk,rho_n,1,.false.)
     full_rho=rho_n
   endif
   !
   ! Calculate V_xc
   !
   if(l_sc_contains_lda.and.SC_xc_functional/=0) then
     call IO_and_Messaging_switch("-report -log")
     call XC_potential_driver(Xen,Xk,WF_KIND,WF_xc_functional,1)
     call IO_and_Messaging_switch("+report +log")
   endif
   !
   ! In all cases where Vxc is frozen and we are not doing BSE
   !
   !   XC0_and_HARTREE0  contains <mk | Vxc(rho_full) + Vhartree(rho_full) | nk>
   !
   ! in all other cases (IRREDUX and TDDFT)
   !
   !   XC0_and_HARTREE0  contains <mk | Vhartree(rho_full) | nk>
   !
   ! ****NOTE**** This procedure is restruicted to the use of the same Vxc (the one
   ! corresponding to the GS) in the basis and in the time-evolution. To do a TDDFT
   ! using a different Vxc in the GS and in the evolution Vxc should be always stored 
   ! in XC0_and_HARTREE0
   !
   do ik=1,QP_nk
     !
     if(.not.l_NE_simulate_Irredux.and..not.l_NE_simulate_TDDFT) then
       !
       call V_real_space_to_H(ik,V_xc,XC0_and_HARTREE0(1:SC_bands(2),1:SC_bands(2),ik,1:n_sp_pol),wf) 
       !
     endif
     !
     call V_real_space_to_H(ik,V_hartree_sc,XC0_and_HARTREE0(1:SC_bands(2),1:SC_bands(2),ik,1:n_sp_pol),wf) 
     !
   enddo
   !
   V_xc_sc=(0._SP)
   if (l_NE_simulate_IP.or.l_NE_simulate_Irredux) V_hartree_sc=(0._SP)
   !
 endif
 !
 ! Starting point 
 !
 it_start=1
 !
 if(RESTART) then
   !
   ! I use G_lesser(:,:,:) to reconstruct everything
   ! and I read it_now from the databases. 
   !
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(5))
   io_G=io_SC_components('G',Xen,ID(5))
   !
   !Note that the G_lesser must be loaded at the correct time:
   !
   NE_i_time=it_now
   !
   do ik=1,QP_nk
     call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),MODE=VERIFY,ID=ID(5))
     io_G=io_SC_components('G',Xen,ID(5))
   enddo
   !
   if(io_G==0.and.it_now<NE_steps+Thermal_steps) then
     call msg('snr','[RT] Number of steps done    [o/o]:',real(it_now)/real(NE_steps)*100._SP)
   endif
   !
   ! [Andrea 23/9/2011] The RESTART value for it_start is tricky.
   ! The order in which DBs are written in RT_driver makes in such a way
   ! that the latest step written i it_now-1. This is beacause RT_databases_IO is called BEFORE RT_Collision.
   !
   ! it_start is used here to call RT_Ext_fields that must evalute the vector potential@it_now-1. 
   ! This MUST be consistent with the restart density and G_lesser that are known@it_now-1.
   !
   ! This is way it_start=it_now. In RT_driver the step is done from it_now to it_now+1.
   !
   it_start=it_now
   !
   ! If I'm doing the response to an external field I need rho_reference and G_lesser_reference
   !
   if(it_start>Thermal_steps.and..not.l_NE_simulate_IP) & 
&     call el_density(G_lesser_reference,Xen,Xk,rho_reference,SC_bands(1),.false.)
   !
 endif
 !
 if (l_NE_with_fields) then
   !
   NE_i_time=it_start-Thermal_steps
   !
   call RT_Ext_fields(A_vecpot,(it_start-1)*RT_step)
   !
 endif
 !
 A_vecpot_previous =A_vecpot
 !
 call msg('r','')
 !
 ! Apply QP-corrections 
 !======================
 !
 do ib=SC_bands(1),SC_bands(2)
   Ho(ib,ib,1:QP_nk,1:n_sp_pol)=Ho(ib,ib,1:QP_nk,1:n_sp_pol) &
&                              -E_bare(ib,1:QP_nk,1:n_sp_pol)+Xen%E(ib,1:QP_nk,1:n_sp_pol)
 enddo
 !
 do ib=1,SC_bands(1)-1
   Ho(ib,ib,1:QP_nk,1:n_sp_pol)=Ho(ib,ib,1:QP_nk,1:n_sp_pol) &
&                              -E_bare(SC_bands(1),1:QP_nk,1:n_sp_pol)+Xen%E(SC_bands(1),1:QP_nk,1:n_sp_pol)
 enddo
 !
 ! Store the starting Eigenvalues in Eo
 !======================================
 !
 if(.not.associated(Xen%Eo)) allocate(Xen%Eo(SC_bands(2),QP_nk,n_sp_pol))
 Xen%Eo(1:SC_bands(2),1:QP_nk,1:n_sp_pol)=Xen%E(1:SC_bands(2),1:QP_nk,1:n_sp_pol)
 !
end subroutine RT_start_and_restart

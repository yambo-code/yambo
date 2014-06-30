!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine RT_start_and_restart(E,k,q,X)
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
 ! [6] (Eventually) load the retarded Green Functions 
 !
 use pars,           ONLY:SP,cI,cZERO,rZERO
 use units,          ONLY:HA2EV
 use drivers,        ONLY:l_sc_contains_lda,l_sc_ip,l_elph_scatt,l_elel_scatt
 use com,            ONLY:msg
 use QP_m,           ONLY:QP_nk,QP_ctl_Wc_E_ref,QP_ctl_Wv_E_ref,QP_ctl_Wc,QP_ctl_Wv
 use electrons,      ONLY:levels,n_sp_pol,spin_occ
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:ID,NE_steps,RT_ind_J,l_NE_dynamics,RT_E_occupations,RT_H_occupations,&
&                         G_lesser_reference,Thermal_steps,rho_reference,  &
&                         l_RT_needs_XC0,it_start,l_NE_with_fields,XC0_and_HARTREE0,&
&                         RT_P,RT_step,rho_full,l_RT_CCA_Kernel,dG_lesser, &
&                         l_NE_simulate_Irredux,NE_i_time,&
&                         Gr_kind,Gr_description,l_RT_uses_E_RIM,N_Gr_descs,RT_IO_G_steps,&
&                         l_initial_equilibrium,RT_E_occupations_reference,&
&                         RT_H_occupations_reference,RT_delta_occupations,RT_delta0_occupations,&
&                         RIM_ibz_coo,RT_eh_en,REF_Lifetime,G_MEM_steps,MEM_index
 use fields,         ONLY:A_vecpot,A_vecpot_previous
 use SC,             ONLY:SC_bands,it_now,H_nl_sc,RT_ibz_coo,&
&                         V_hartree_sc,RT_nk,G_lesser,RT_ibz_coo,&
&                         E_bare,V_xc_sc,rho_n,Ho,SC_xc_functional,SC_nbands
 use IO_m,           ONLY:OP_RD,OP_RD_CL,RD_CL_IF_END,io_control,REP,VERIFY,NONE,IO_and_Messaging_switch
 use X_m,            ONLY:X_t
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 use wave_func,      ONLY:WF
 use xc_functionals, ONLY:V_xc,XC_potential_driver
 use parallel_m,     ONLY:PP_redux_wait,master_cpu
 use stderr,         ONLY:real2ch
 implicit none
 !
 type(levels)      :: E
 type(bz_samp)     :: k,q
 type(X_t)         :: X
 !
 ! Work Space
 !
 integer :: ik,ik_RT_rim,ik_E_rim,COM_,ib,io_E,io_V,io_WF,io_G,io_J_and_P,io_THETA,io_REF,&
&           io_OCCUPATIONS,ID_OCCUPATIONS,it,nt
 integer, external :: io_SC_components,io_RT_components,RT_Lifetimes_evaluation_plan
 logical :: RESTART
 !
 ! Initialize the Gf and potentials
 !==================================
 !
 ! G_lesser(T=0,T=0)=Go and G_lesser_old(T=0,T=0)=Go
 !
 G_lesser          =cZERO
 dG_lesser         =cZERO
 rho_reference     =rZERO
 if(allocated(rho_full)) rho_full=cZERO
 V_xc_sc           =rZERO
 V_hartree_sc      =rZERO
 H_nl_sc           =cZERO
 !
 ! Set to zero occupations
 !
 RT_E_occupations=rzero
 RT_H_occupations=rzero
 RT_E_occupations_reference=rzero
 RT_H_occupations_reference=rzero
 RT_delta0_occupations=rzero
 RT_delta_occupations=rzero
 !
 ! Set to zero current and fields
 !
 RT_P              =cZERO
 RT_ind_J          =cZERO
 ! 
 do ib=SC_bands(1),SC_bands(2)
   do ik=1,k%nibz
     do ik_RT_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       ik_E_rim=ik_RT_rim-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
       if (l_RT_uses_E_RIM) then
         G_lesser(ib,ib,ik_RT_rim,1) =cI*E%f_RIM(ib,ik_E_rim,1)
         if(associated(E%W)) E%W_RIM(ib,ik_E_rim,1)=E%W(ib,ik,1)
       else
         G_lesser(ib,ib,ik,1) =cI*E%f(ib,ik,1)
       endif
       RT_E_occupations(ib,ik_RT_rim)=         aimag(G_lesser(ib,ib,ik_RT_rim,1))
       RT_H_occupations(ib,ik_RT_rim)=spin_occ-aimag(G_lesser(ib,ib,ik_RT_rim,1))
     enddo
   enddo
 enddo
 !
 ! Reference occupations and G
 !
 G_lesser_reference=G_lesser(:,:,:,1)
 do ib=SC_bands(1),SC_bands(2)
   RT_E_occupations_reference(ib,:)=         aimag(G_lesser_reference(ib,ib,:))
   RT_H_occupations_reference(ib,:)=spin_occ-aimag(G_lesser_reference(ib,ib,:))
 enddo
 !
 ! Build Ho and save initial energies 
 !====================================
 !
 if(associated(E%Eo)) then
   E_bare=E%Eo(:SC_bands(2),:,:)
 else
   E_bare=E%E(:SC_bands(2),:,:)
 endif
 !
 call SC_bare_Hamiltonian(E,k,k)
 !
 ! Perform a first I/O to check which DBs are present. 
 !=====================================================
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(1)) ! Energies
 io_E=io_SC_components('E' ,E,ID(1))
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2)) ! WFs
 io_WF=io_SC_components('WF',E,ID(2))
 call io_control(ACTION=OP_RD_CL,COM=REP ,SEC=(/1/),MODE=VERIFY,ID=ID(3)) ! Local potential
 io_V=io_SC_components('V' ,E,ID(3))
 !
 COM_=REP
 call io_control(ACTION=OP_RD_CL,COM=COM_,SEC=(/1/),MODE=VERIFY,ID=ID(4)) ! Current response
 io_J_and_P=io_RT_components('J_and_P',ID(4))
 if (io_J_and_P==0) COM_=NONE
 call io_control(ACTION=OP_RD_CL,COM=COM_,SEC=(/1/),MODE=VERIFY,ID=ID(5)) ! G_lesser
 io_G=io_RT_components('G',ID(5))
 !
 ! Reference G_lesser and Lifetimes
 !
 call io_control(ACTION=OP_RD_CL,COM=COM_,SEC=(/1/),MODE=VERIFY,ID=ID(8)) ! Reference
 io_REF=io_RT_components('REF',ID(8))
 !
 ! ELPH Theta matrix
 !
 call io_control(ACTION=OP_RD_CL,COM=COM_,SEC=(/1/),MODE=VERIFY,ID=ID(7)) ! THETA matrix
 io_THETA=io_RT_components('THETA',ID(7))
 !
 ! To restart from RT only the G_lesser is necessary (latest and reference)
 !
 RESTART = (io_G ==0).and.(io_REF==0)
 if (l_elph_scatt.and..not.l_RT_CCA_Kernel) RESTART = RESTART.and.(io_THETA==0)
 !
 if (l_RT_needs_XC0) then
   !
   ! Build Hartree and XCo 
   !=======================
   !
   ! Note that 
   !  1. l_RT_needs_XC0 is .TRUE. only when we are starting a NE loop
   !  2. V_hartree and rho_full have been already calculated in SC_bare_Hamiltonian
   !
   ! The time evolution is done updating only the contribution
   ! coming from the bands above SC_bands(1) in the density.
   ! Therefore we calculate:
   !
   ! (1) a density containing ALL levels (rho_full)
   ! (2) a density containing only levels above SC_bands(1) (rho_reference)
   !
   ! in RT_Collision 
   !   V_h/xc[n]=V_h/xc[rho_full]+V_h/xc[rho-rho_reference]
   !
   ! When SC_bands(1)=1 rho_full=rho_reference
   !
   ! The first term is stored here in XC0_and_HARTREE0
   !
   XC0_and_HARTREE0=(0._SP,0._SP)
   !
   ! Rho_reference. Note that altough rho_n has been already calculated 
   ! in sc_bare_Hamiltonian I need to recalculate it here when SC_bands(1)>1
   ! as it is also used in XC_potential_driver
   ! 
   call el_density_matrix(G_lesser_reference(:,:,RT_ibz_coo(:,1)),E,k,rho_n,SC_bands(1))
   rho_reference=rho_n
   !
   if (allocated(rho_full)) then ! rho_full is allocated if l_sc_contains_lda.and.SC_xc_functional/=0.and.SC_bands(1)>1
     call el_density_matrix(G_lesser_reference(:,:,RT_ibz_coo(:,1)),E,k,rho_n,1)
     rho_full=rho_n
   endif
   !
   ! Calculate V_xc
   !
   if(l_sc_contains_lda.and.SC_xc_functional/=0) then
     call XC_potential_driver(E,k,WF_KIND,WF_xc_functional,1,MUTE="-report -log",unMUTE=master_cpu)
     call IO_and_Messaging_switch("+log")
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
   ! ****NOTE**** This procedure is restricted to the use of the same Vxc (the one
   ! corresponding to the GS) in the basis and in the time-evolution. To do a TDDFT
   ! using a different Vxc in the GS and in the evolution Vxc should be always stored 
   ! in XC0_and_HARTREE0
   !
   do ik=1,QP_nk
     !
     if (.not.l_NE_simulate_Irredux.and..not.l_sc_contains_lda) then
       !
       call V_real_space_to_H(ik,V_xc,XC0_and_HARTREE0(1:SC_bands(2),1:SC_bands(2),ik,1:n_sp_pol),WF) 
       !
     endif
     !
     call V_real_space_to_H(ik,V_hartree_sc,XC0_and_HARTREE0(1:SC_bands(2),1:SC_bands(2),ik,1:n_sp_pol),WF) 
     !
   enddo
   !
   call PP_redux_wait(XC0_and_HARTREE0)
   !
   V_xc_sc=(0._SP)
   !
   V_hartree_sc=(0._SP)
   !
 endif
 !
 ! Starting point 
 !
 if (RESTART) then
   !
   ! I use G_lesser(:,:,:) to reconstruct everything
   ! and I read it_now from the databases. 
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=VERIFY,ID=ID(8))
   io_REF=io_RT_components('REF',ID(8))
   !
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(5))
   io_G=io_RT_components('G',ID(5))
   !
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID_OCCUPATIONS)
   io_OCCUPATIONS=io_RT_components('occupations',ID_OCCUPATIONS)
   !
   if (l_elph_scatt.and..not.l_RT_CCA_Kernel) then
     call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(7))
     io_THETA=io_RT_components('THETA',ID(7))
   endif
   !
   ! Note that the databases must be loaded at the correct time:
   !
   NE_i_time=((it_now-1)/RT_IO_G_steps)*RT_IO_G_steps+1
   it_now=NE_i_time
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/NE_i_time+1/),MODE=VERIFY,ID=ID_OCCUPATIONS)
   io_OCCUPATIONS=io_RT_components('occupations',ID_OCCUPATIONS)
   !
   do ik=1,RT_nk
     !
     call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),MODE=VERIFY,ID=ID(5))
     io_G=io_RT_components('G',ID(5))
     !
     if (l_elph_scatt.and..not.l_RT_CCA_Kernel) then
       call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),MODE=VERIFY,ID=ID(7))
       io_THETA=io_RT_components('THETA',ID(7))
     endif
     !
     ! The G_lesser must be now rebuild from the reference (T=0) and latest dG components
     !
     G_lesser(:,:,ik,MEM_index(NE_i_time,G_MEM_steps))=&
&            G_lesser_reference(:,:,ik)+dG_lesser(:,:,ik,MEM_index(NE_i_time,G_MEM_steps))
     !
   enddo
   !
   if(.not.l_RT_CCA_Kernel) call RT_ELPH_zeroing()
   !
   if(it_now<NE_steps+Thermal_steps) then
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
   if (it_start>Thermal_steps.and..not.l_sc_ip) then 
     call el_density_matrix(G_lesser_reference(:,:,RT_ibz_coo(:,1)),E,k,rho_reference,SC_bands(1))
   endif
   !
   ! Last thing to do is to update the planned time sampling time (if needed)
   !
   nt=RT_Lifetimes_evaluation_plan( )
   !
 else
   !
   ! Possibility to pump by hand one or more electrons up
   !======================================================
   call RT_manual_excitation(E,k)
   !
 endif
 !
 ! Update occupations after manual pump or G_lesser from DB (?)
 !==============================================================
 do ib=SC_bands(1),SC_bands(2)
   RT_E_occupations(ib,:)= aimag(dG_lesser(ib,ib,:,1))+          aimag(G_lesser_reference(ib,ib,:))
   RT_H_occupations(ib,:)=-aimag(dG_lesser(ib,ib,:,1))+(spin_occ-aimag(G_lesser_reference(ib,ib,:)))
   RT_delta0_occupations(ib,:)=aimag(dG_lesser(ib,ib,:,1))
 enddo
 !
 if (l_NE_dynamics) NE_i_time=it_start-Thermal_steps
 !
 if (l_NE_with_fields) then
   call RT_Ext_fields(A_vecpot,(it_start-1)*RT_step)
   A_vecpot_previous =A_vecpot
 endif
 !
 call msg('r','')
 !
 ! Apply QP-corrections 
 !======================
 !
 do ib=SC_bands(1),SC_bands(2)
   Ho(ib,ib,1:QP_nk,1:n_sp_pol)=Ho(ib,ib,1:QP_nk,1:n_sp_pol) &
&                              -E_bare(ib,1:QP_nk,1:n_sp_pol)+E%E(ib,1:QP_nk,1:n_sp_pol)
 enddo
 !
 do ib=1,SC_bands(1)-1
   Ho(ib,ib,1:QP_nk,1:n_sp_pol)=Ho(ib,ib,1:QP_nk,1:n_sp_pol) &
&                              -E_bare(SC_bands(1),1:QP_nk,1:n_sp_pol)+E%E(SC_bands(1),1:QP_nk,1:n_sp_pol)
 enddo
 !
 ! Store the starting Eigenvalues in Eo
 !======================================
 !
 if (.not.associated(E%Eo)) allocate(E%Eo(SC_bands(2),QP_nk,n_sp_pol))
 E%Eo(1:SC_bands(2),1:QP_nk,1:n_sp_pol)=E%E(1:SC_bands(2),1:QP_nk,1:n_sp_pol)
 !
 ! Define RT_eh_en
 !================
 if( all(RT_eh_en(:)==0._SP) ) then
   ! Case A: was not defined in input
   if (l_RT_uses_E_rim) then
     RT_eh_en(1)=minval(E%E_RIM(SC_bands(1):SC_bands(2),:,:)) 
     RT_eh_en(2)=maxval(E%E_RIM(SC_bands(1):SC_bands(2),:,:)) 
   else
     RT_eh_en(1)=minval(E%E(SC_bands(1):SC_bands(2),:,:)) 
     RT_eh_en(2)=maxval(E%E(SC_bands(1):SC_bands(2),:,:)) 
   endif
 else
   ! Case B: was defined in input
   RT_eh_en(1)=RT_eh_en(1)+E%E_VBM
   RT_eh_en(2)=RT_eh_en(2)+E%E_CBm
 endif
 !
 ! Retarded Green Functions matrix
 ! ===============================
 !
 if (l_elph_scatt.or.l_elel_scatt) then
   !
   call k_build_up_BZ_tables(q)
   call k_build_up_BZ_tables(k)
   !
   N_Gr_descs=5
   !
   Gr_description(1)=' [Gr] Retarded Green`s function  : '//trim(Gr_kind)
   Gr_description(2)=' [Gr] Damping (c) Energy ref [eV]: '//trim(real2ch(QP_ctl_Wc_E_ref(3,1)*HA2EV))
   Gr_description(3)=' [Gr] Damping (c) [eV,adim,eV^-1]: '//&
&                    trim(real2ch(QP_ctl_Wc(3,1,1)))//' '//&
&                    trim(real2ch(QP_ctl_Wc(3,2,1)))//' '//trim(real2ch(QP_ctl_Wc(3,3,1)))
   Gr_description(4)=' [Gr] Damping (v) Energy ref [eV]: '//trim(real2ch(QP_ctl_Wv_E_ref(3,1)*HA2EV))
   Gr_description(5)=' [Gr] Damping (v) [eV,adim,eV^-1]: '//&
&                    trim(real2ch(QP_ctl_Wv(3,1,1)))//' '//&
&                    trim(real2ch(QP_ctl_Wv(3,2,1)))//' '//trim(real2ch(QP_ctl_Wv(3,3,1)))
   !
 else
   !
   N_Gr_descs=1
   Gr_description(1)=' [Gr] Retarded Green`s function  : none'
   !
 endif
 !
end subroutine RT_start_and_restart

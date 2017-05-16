!
!        Copyright (C) 2000-2017 the YAMBO team
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
 !   > Use SC as a starting point
 !   > RESTART from a previous run
 ! [4] Initialize the bare Hartree and XC
 ! [5] Initialize the fields
 ! [6] (Eventually) load the retarded Green Functions 
 !
 use pars,           ONLY:SP,cI,cZERO,rZERO
 use units,          ONLY:HA2EV,AUT2FS
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_use_Hxc_collisions
 use com,            ONLY:msg
 use QP_m,           ONLY:QP_ctl_Wc_E_ref,QP_ctl_Wv_E_ref,QP_ctl_Wc,QP_ctl_Wv
 use electrons,      ONLY:levels,n_sp_pol,spin_occ,n_spin
 use R_lattice,      ONLY:bz_samp,nXkibz
 use RT_control,     ONLY:ID,NE_i_start_time,JPC_RT_IO_t,GF_RT_IO_t,OUTPUT_RT_IO_t,TIME_adjust
 use real_time,      ONLY:NE_steps,RT_ind_J,l_NE_dynamics,RT_E_occupations,RT_H_occupations,&
&                         G_lesser_reference,rho_reference,magn_reference,RT_init_occupation_reference,  &
&                         l_NE_with_fields,XC0,HARTREE0,RT_ibz_coo,NE_time,l_RT_is_WFfree, &
&                         RT_P,RT_step,l_RT_CCA_Kernel,dG_lesser,NE_i_time,RT_dyn_step,&
&                         Gr_kind,Gr_description,l_RT_uses_E_DbGd,N_Gr_descs,RT_E_and_k,&
&                         RT_E_occupations_reference,RT_ibz_coo,RT_nk,G_lesser,RT_potential, &
&                         RT_H_occupations_reference,RT_delta_occupations,RT_delta0_occupations,&
&                         DbGd_ibz_coo,RT_eh_en,G_MEM_steps,RT_bands,i_MEM_collision,i_MEM_now,&
&                         i_MEM_previous,MEM_pointer,NE_time_step_update_jump,RT_life_extrap_times
 use fields,         ONLY:A_tot
 use hamiltonian,    ONLY:H_nl_sc,V_hartree_sc,V_xc_sc,rho_n,magn_n,Hzero
 use IO_m,           ONLY:OP_RD,OP_RD_CL,OP_IF_START_RD_CL_IF_END,RD_CL_IF_END,io_control,REP,VERIFY,NONE
 use X_m,            ONLY:X_t
 use wave_func,      ONLY:WF
 use xc_functionals, ONLY:V_xc
 use parallel_m,     ONLY:PP_redux_wait
 use stderr,         ONLY:real2ch
 !
#include<memory.h>
 !
 type(levels)      :: E
 type(bz_samp)     :: k,q
 type(X_t)         :: X
 !
 ! Work Space
 !
 integer :: i_sp_pol,ik,COM_,io_E,io_V,io_WF,io_G,io_JPSM,io_THETA,io_REF,&
&           io_OCCUPATIONS,ID_OCCUPATIONS,nt,i_memory
 integer, external :: io_RT_components,RT_Lifetimes_evaluation_plan
 logical :: RESTART
 !
 ! Initialize the Gf and potentials
 !====================================
 call RT_initialize_local()
 ! 
 ! G_lesser occupations and reference
 !====================================
 call RT_set_G_lesser() 
 call RT_set_occupations()
 !
 if(.not.l_RT_is_WFfree) then
   !
   call el_density_matrix(G_lesser_reference,E,k,rho_reference,1)
   if(n_spin>1) call el_magnetization_matrix(G_lesser_reference,E,k,magn_reference,1)
   !
   call Bare_Hamiltonian(E,k,k)
   !
   ! Reference Hartree and XC
   !==========================
   do i_sp_pol=1,n_sp_pol
     do ik=1,nXkibz
       call V_real_space_to_H(ik,i_sp_pol,V_hartree_sc,HARTREE0(:,:,ik,i_sp_pol),WF,'def') 
       call V_real_space_to_H(ik,i_sp_pol,V_xc,        XC0(:,:,ik,i_sp_pol),WF,'xc ') 
     enddo
   enddo
   !
   ! In the driver loop both V_hartree_sc and V_xc corresponds to the [delta_rho] value. As here
   ! delta_rho=0 both V_hartree_sc and V_xc must be set to zero. Eventually a non zero value is read
   ! from the potential databaes in case of a RESTARTed run
   !
   V_xc_sc           =rZERO
   V_hartree_sc      =rZERO
   !
   call PP_redux_wait(HARTREE0)
   call PP_redux_wait(XC0)
   !
 endif
 !
 ! Perform a first I/O to check which DBs are present. 
 !=====================================================
 !
 COM_=REP
 call io_control(ACTION=OP_RD_CL,COM=COM_,SEC=(/1/),MODE=VERIFY,ID=ID(4)) ! Current response
 io_JPSM=io_RT_components('JPSM',ID(4))
 if (io_JPSM==0) COM_=NONE
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
 ! Starting point 
 !================
 !
 if (RESTART) then
   !
   ! I use G_lesser(:,:,:) to reconstruct everything
   ! and I read NE_i_time from the databases. 
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=VERIFY,ID=ID(8))
   io_REF=io_RT_components('REF',ID(8))
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(5))
   io_G=io_RT_components('G',ID(5))
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID_OCCUPATIONS)
   io_OCCUPATIONS=io_RT_components('carriers',ID_OCCUPATIONS)
   !
   if (l_elph_scatt.and..not.l_RT_CCA_Kernel) then
     call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(7))
     io_THETA=io_RT_components('THETA',ID(7))
   endif
   !
   ! Note that the databases must be loaded at the RESTART time (NE_time)
   !
   NE_i_time=nint(NE_time/RT_step)+1
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/NE_i_time+1/),MODE=VERIFY,ID=ID_OCCUPATIONS)
   io_OCCUPATIONS=io_RT_components('carriers',ID_OCCUPATIONS)
   !
   do ik=1,RT_nk
     !
     call io_control(ACTION=OP_IF_START_RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),MODE=VERIFY,ID=ID(5))
     io_G=io_RT_components('G',ID(5))
     !
     if (l_elph_scatt.and..not.l_RT_CCA_Kernel) then
       call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),MODE=VERIFY,ID=ID(7))
       io_THETA=io_RT_components('THETA',ID(7))
     endif
     !
     ! The G_lesser must be now rebuild from the reference (T=0) and latest dG components
     !
     do i_memory=1,G_MEM_steps
       G_lesser(:,:,ik,i_memory)=G_lesser_reference(:,:,ik)+dG_lesser(:,:,ik,i_memory)
       if (MEM_pointer(i_memory)==NE_i_time  ) i_MEM_now=i_memory
       if (MEM_pointer(i_memory)==NE_i_time-1) i_MEM_previous=i_memory
     enddo
     !
   enddo
   !
   ! Define RT_E/H_occupations_reference
   !
   call RT_init_occupation_reference( )
   !
   ! To be used in the first part of the RT_driver main loop
   !
   i_MEM_collision = i_MEM_now
   !
   if(.not.l_RT_CCA_Kernel) call RT_ELPH_zeroing()
   !
   NE_i_start_time=NE_i_time
   !
   ! Last thing to do is to update the planned time sampling time (if needed)
   !
   nt=RT_Lifetimes_evaluation_plan( )
   !
   ! Update occupations after loading from DB 
   !==========================================
   call RT_set_occupations()
   !
   call msg( 'nr','---RESTART report---')
   !
   call msg('s','[RESTART] Number of steps done    [o/o]:',real(NE_i_time)/real(NE_steps)*100._SP)
   call msg('r',  'Number of steps done    [o/o]:',real(NE_i_time)/real(NE_steps)*100._SP)
   call msg('r',  'Time step                     [fs]:',RT_dyn_step*AUT2FS)
   call msg('r',  'dT Update Interval            [fs]:',NE_time_step_update_jump*AUT2FS)
   call msg('r',  'Lifetimes inter/intr times    [fs]:',RT_life_extrap_times*AUT2FS)
   call msg('r',  'T between I/O of J,P,f        [fs]:',JPC_RT_IO_t%INTERVAL_time*AUT2FS)
   call msg('r',  '          I/O of   GFs        [fs]:',GF_RT_IO_t%INTERVAL_time*AUT2FS)
   !
   OUTPUT_RT_IO_t%INTERVAL_time  = max(TIME_adjust(OUTPUT_RT_IO_t%INTERVAL_time,RT_dyn_step),RT_dyn_step)
   OUTPUT_RT_IO_t%INTERVAL_steps = nint(OUTPUT_RT_IO_t%INTERVAL_time/RT_step)
   OUTPUT_RT_IO_t%last_point     = NE_i_time - OUTPUT_RT_IO_t%INTERVAL_steps
   !
   call msg('r',  '               outputs        [fs]:',OUTPUT_RT_IO_t%INTERVAL_time*AUT2FS)
   !
 else
   !
   NE_time=0._SP
   !
 endif
 !
 if (l_NE_dynamics) NE_i_time=NE_i_start_time
 !
 ! Initialize the Vector Potential
 !=================================
 !
 if (l_NE_with_fields) call RT_propagate_fields(E,k,A_tot,A_tot,A_tot,dG_lesser(:,:,:,1),NE_time-RT_step,RT_step)
 !
 call msg('r','')
 !
 ! Define RT_eh_en
 !================
 if( all(RT_eh_en(:)==0._SP) ) then
   ! Case A: was not defined in input
   if (l_RT_uses_E_DbGd) then
     RT_eh_en(1)=minval(E%E_DbGd(RT_bands(1):RT_bands(2),:,:)) 
     RT_eh_en(2)=maxval(E%E_DbGd(RT_bands(1):RT_bands(2),:,:)) 
   else
     RT_eh_en(1)=minval(E%E(RT_bands(1):RT_bands(2),:,:)) 
     RT_eh_en(2)=maxval(E%E(RT_bands(1):RT_bands(2),:,:)) 
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
 contains
   !
   subroutine RT_initialize_local()
     !
     G_lesser          =cZERO
     G_lesser_reference=cZERO
     dG_lesser         =cZERO
     !
     H_nl_sc           =cZERO
     !
     if(.not.l_RT_is_WFfree) then
       !
       V_xc_sc           =rZERO
       V_hartree_sc      =rZERO
       HARTREE0          =cZERO
       XC0               =cZERO
       !
       rho_reference     =rZERO
       if(n_spin>1) magn_reference    =rZERO
       !
     endif
     !
     ! Set to zero occupations
     !
     RT_E_occupations          =rzero
     RT_H_occupations          =rzero
     RT_E_occupations_reference=rzero
     RT_H_occupations_reference=rzero
     RT_delta0_occupations     =rzero
     RT_delta_occupations      =rzero
     !
     ! Set to zero current and fields
     !
     RT_P              =cZERO
     RT_ind_J          =cZERO
     !
   end subroutine
   !
   subroutine RT_set_G_lesser()
     !
     integer :: ik,ik_RT_DbGd,ik_E_DbGd,ib
     !
     ! Defined from occupations %f
     ! In case RT_apply has been used the reference is
     ! defined from the %fo occupations
     !
     do ib=RT_bands(1),RT_bands(2)
       do ik=1,k%nibz
         do ik_RT_DbGd=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
           ik_E_DbGd=ik_RT_DbGd-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
           if (l_RT_uses_E_DbGd) then
             !if (allocated(E%W))  RT_E_and_k%W(ib,ik_RT_DbGd,1) =abs(E%W_DbGd(ib,ik_E_DbGd,1))
             if (allocated(E%W))  E%W_DbGd(ib,ik_E_DbGd,1) =abs(E%W(ib,ik,1))
             RT_E_and_k%En(ib,ik_RT_DbGd,1)=E%E_DbGd(ib,ik_E_DbGd,1)
             !RT_E_and_k%kpt(:,ik_RT_DbGd)=k%pt(ik,:)
             G_lesser(ib,ib,ik_RT_DbGd,1) =cI*E%f_DbGd(ib,ik_E_DbGd,1)
             if (allocated(E%fo)) G_lesser_reference(ib,ib,ik_RT_DbGd) =cI*(E%f_DbGd(ib,ik_E_DbGd,1)-E%df_DbGd(ib,ik_E_DbGd,1))
           else
             G_lesser(ib,ib,ik,1) =cI*E%f(ib,ik,1)
             if (allocated(E%W))  RT_E_and_k%W(ib,ik,1) =abs(E%W(ib,ik,1))
             RT_E_and_k%En(ib,ik,1)=E%E(ib,ik,1)
             !RT_E_and_k%kpt(:,ik)=k%pt(ik,:)
             if (allocated(E%fo)) G_lesser_reference(ib,ib,ik) =cI*E%fo(ib,ik,1)
           endif
         enddo
       enddo
     enddo
     if (.not.(allocated(E%fo))) G_lesser_reference=G_lesser(:,:,:,1)
     dG_lesser(:,:,:,1)= G_lesser(:,:,:,1)-G_lesser_reference
     !
   end subroutine RT_set_G_lesser
   ! 
   subroutine RT_set_occupations()
     !
     use functions,  ONLY:Fermi_fnc
     use real_time,  ONLY:l_RT_relax_time_approx,RT_RTA_chem,RT_RTA_temp, &
&                         RT_RTA_E_occ_infty,RT_RTA_H_occ_infty
     !
     real(SP):: En
     integer :: ik,ik_RT_DbGd,ik_E_DbGd,ib
     !
     ! Occupations are defined from G_lesser and G_lesser_reference
     !
     do ib=RT_bands(1),RT_bands(2)
       RT_E_occupations_reference(ib,:)=         aimag(G_lesser_reference(ib,ib,:))
       RT_H_occupations_reference(ib,:)=spin_occ-aimag(G_lesser_reference(ib,ib,:))
       RT_delta0_occupations(ib,:)     =         aimag(dG_lesser(ib,ib,:,1))
       RT_delta_occupations(ib,:)      =         aimag(dG_lesser(ib,ib,:,1))
       RT_E_occupations(ib,:) = RT_E_occupations_reference(ib,:)+RT_delta0_occupations(ib,:)
       RT_H_occupations(ib,:) = RT_H_occupations_reference(ib,:)-RT_delta0_occupations(ib,:)
     enddo
     !
     ! RTA infinity ditributions
     !
     if (l_RT_relax_time_approx) then
       do ib=RT_bands(1),RT_bands(2)
         do ik=1,k%nibz
           do ik_RT_DbGd=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
             ik_E_DbGd=ik_RT_DbGd-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
             if (     l_RT_uses_E_DbGd) En=E%E_DbGd(ib,ik_E_DbGd,1)
             if (.not.l_RT_uses_E_DbGd) En=E%E(ib,ik,1)
             RT_RTA_E_occ_infty(ib,ik_RT_DbGd)= Fermi_fnc(  En-E%E_CBm-RT_RTA_chem(1) ,RT_RTA_temp(1))*spin_occ
             RT_RTA_H_occ_infty(ib,ik_RT_DbGd)= Fermi_fnc(-(En-E%E_VBM-RT_RTA_chem(2)),RT_RTA_temp(2))*spin_occ
           enddo
         enddo
       enddo
     endif
     !
   end subroutine RT_set_occupations
   !
end subroutine RT_start_and_restart

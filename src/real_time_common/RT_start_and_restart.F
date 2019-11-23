!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine RT_start_and_restart(E,k,q)
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
 use pars,           ONLY:SP,cZERO,rZERO
 use units,          ONLY:HA2EV,AUT2FS
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt
 use com,            ONLY:msg
 use QP_CTL_m,       ONLY:QP_ctl_user
 use electrons,      ONLY:levels,n_sp_pol,spin_occ,n_spin
 use R_lattice,      ONLY:bz_samp,nXkibz
 use RT_control,     ONLY:ID,NE_i_start_time,OBS_RT_IO_t,GF_RT_IO_t,OUTPUT_RT_IO_t,TIME_adjust
 use real_time,      ONLY:NE_steps,RT_ind_J,l_NE_dynamics,RT_E_occupations,RT_H_occupations,&
&                         G_lesser_reference,rho_reference,magn_reference,RT_init_occupation_reference,  &
&                         l_NE_with_fields,XC0,HARTREE0,RT_ibz_coo,NE_time,l_RT_is_WFfree, &
&                         RT_P,RT_step,l_RT_CCA_Kernel,dG_lesser,NE_i_time,RT_dyn_step,&
&                         Gr_kind,Gr_description,l_RT_uses_E_DbGd,N_Gr_descs,RT_E_and_k,&
&                         RT_E_occupations_reference,RT_ibz_coo,RT_nk,G_lesser, &
&                         RT_H_occupations_reference,RT_life_delta_occupations,RT_delta_occupations,&
&                         DbGd_ibz_coo,RT_eh_en,G_MEM_steps,RT_bands,i_MEM_collision,i_MEM_now,&
&                         i_MEM_previous,MEM_pointer,NE_time_step_update_jump,RT_life_extrap_times,&
&                         l_velocity_gauge_corr
 use fields,         ONLY:A_tot
 use hamiltonian,    ONLY:H_nl_sc,V_hartree_sc,V_xc_sc
 use IO_m,           ONLY:OP_RD,OP_RD_CL,OP_IF_START_RD_CL_IF_END,RD_CL_IF_END,io_control,REP,VERIFY,NONE
 use wave_func,      ONLY:WF
 use xc_functionals, ONLY:V_xc
 use H_interfaces,   ONLY:V_real_space_to_H
 use parallel_m,     ONLY:PP_redux_wait
 use stderr,         ONLY:real2ch
 !
#include<memory.h>
 !
 type(levels)      :: E
 type(bz_samp)     :: k,q
 !
 ! Work Space
 !
 integer :: i_sp_pol,ik,COM_,io_G,io_OBS,io_THETA,io_REF,&
&           io_OCCUPATIONS,ID_OCCUPATIONS,nt,i_memory
 integer, external :: io_RT_components
 !
 !
 logical :: RESTART
 !
 ! Initialize the Gf and potentials
 !====================================
 call RT_initialize_local()
 ! 
 ! G_lesser occupations and reference
 !====================================
 call RT_set_G_lesser()
 !
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
       call V_real_space_to_H(ik,i_sp_pol,HARTREE0(:,:,ik,i_sp_pol),WF,'def',V=V_hartree_sc) 
       call V_real_space_to_H(ik,i_sp_pol,     XC0(:,:,ik,i_sp_pol),WF,'xc ',Vxc=V_xc) 
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
 RESTART=.FALSE.
 !
 !
 if (.not.RESTART) NE_time=0._SP
 !
 if (l_NE_dynamics) NE_i_time=NE_i_start_time
 !
 ! Initialize the Vector Potential
 !=================================
 !
 if (l_NE_with_fields) call RT_propagate_fields(E,k,A_tot,A_tot,dG_lesser(:,:,:,1),NE_time-RT_step,RT_step)
 !
 ! Pseudo potential terms needed for velocity gauge
 !==================================================
 !
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
   RT_eh_en(1)=RT_eh_en(1)+E%E_VBM(1)
   RT_eh_en(2)=RT_eh_en(2)+E%E_CBm(1)
 endif
 !
 ! Retarded Green Functions matrix
 ! ===============================
 !
 N_Gr_descs=1
 Gr_description(1)=' [Gr] Retarded Green`s function  : none'
 !
 !
 call PP_redux_wait()
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
     real(SP):: Energy,f_occ,f_occ_ref,Width
     !
     ! Defined from occupations %f
     ! In case RT_apply has been used the reference is
     ! defined from the %fo occupations
     !
     do ib=RT_bands(1),RT_bands(2)
       do ik=1,k%nibz
         !
         do ik_RT_DbGd=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
           !
           ik_E_DbGd=ik_RT_DbGd-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
           !
           if (l_RT_uses_E_DbGd) then
             !
             if (allocated(E%W))  E%W_DbGd(ib,ik_E_DbGd,1)     = abs(E%W(ib,ik,1))
             !
             Energy    = E%E_DbGd(ib,ik_E_DbGd,1)
             f_occ     = E%f_DbGd(ib,ik_E_DbGd,1)
             f_occ_ref = f_occ
             Width     = 0._SP
             if (allocated(E%fo)) f_occ_ref = E%f_DbGd(ib,ik_E_DbGd,1)-E%df_DbGd(ib,ik_E_DbGd,1)
             if (allocated(E%W))  Width     = abs(E%W_DbGd(ib,ik_E_DbGd,1))
             !
           else
             !
             Energy    = E%E(ib,ik,1)
             f_occ     = E%f(ib,ik,1)
             f_occ_ref = f_occ
             Width     = 0._SP
             if (allocated(E%fo)) f_occ_ref = E%fo(ib,ik,1)
             if (allocated(E%W))  Width     = abs(E%W(ib,ik,1))
             !
           endif
           !
           RT_E_and_k%E(ib,ik_RT_DbGd,1) = Energy
           RT_E_and_k%W(ib,ik_RT_DbGd,1) = Width 
           !
           G_lesser(ib,ib,ik_RT_DbGd,1)         = cmplx(0.,f_occ    ,SP)
           G_lesser_reference(ib,ib,ik_RT_DbGd) = cmplx(0.,f_occ_ref,SP)
           dG_lesser(ib,ib,ik_RT_DbGd,1)        = G_lesser(ib,ib,ik_RT_DbGd,1)-G_lesser_reference(ib,ib,ik_RT_DbGd)
           !
         enddo
       enddo
     enddo
     !
   end subroutine RT_set_G_lesser
   !
   !
end subroutine RT_start_and_restart

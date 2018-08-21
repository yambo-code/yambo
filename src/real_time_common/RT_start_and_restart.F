!
!        Copyright (C) 2000-2018 the YAMBO team
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
 use units,          ONLY:HA2EV,AUT2FS
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_use_Hxc_collisions
 use com,            ONLY:msg
 use QP_CTL_m,       ONLY:QP_ctl_user
 use electrons,      ONLY:levels,n_sp_pol,spin_occ,n_spin
 use R_lattice,      ONLY:bz_samp,nXkibz
 use RT_control,     ONLY:ID,NE_i_start_time,JPC_RT_IO_t,GF_RT_IO_t,OUTPUT_RT_IO_t,TIME_adjust
 use real_time,      ONLY:NE_steps,RT_ind_J,l_NE_dynamics,RT_E_occupations,RT_H_occupations,&
&                         G_lesser_reference,rho_reference,magn_reference,RT_init_occupation_reference,  &
&                         l_NE_with_fields,XC0,HARTREE0,RT_ibz_coo,NE_time,&
&                         RT_P,RT_step,l_RT_CCA_Kernel,dG_lesser,NE_i_time,RT_dyn_step,&
&                         Gr_kind,Gr_description,l_RT_uses_E_DbGd,N_Gr_descs,&
&                         RT_E_occupations_reference,RT_ibz_coo,RT_nk,G_lesser, &
&                         RT_H_occupations_reference,RT_delta_occupations,RT_delta0_occupations,&
&                         DbGd_ibz_coo,RT_eh_en,G_MEM_steps,RT_bands,i_MEM_collision,i_MEM_now,&
&                         i_MEM_previous,MEM_pointer,NE_time_step_update_jump,RT_life_extrap_times
 use fields,         ONLY:A_vecpot,A_vecpot_previous,A_ext
 use hamiltonian,    ONLY:H_nl_sc,V_hartree_sc,E_bare,V_xc_sc,rho_n,magn_n,Ho
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
 integer :: i_sp_pol,ik,ik_RT_rim,ik_E_rim,COM_,ib,io_G,io_JPSM,io_THETA,io_REF,&
&           io_OCCUPATIONS,ID_OCCUPATIONS,nt,i_memory
 complex(SP):: M_tmp(RT_bands(2),RT_bands(2))
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
 ! Build Ho and save initial energies 
 !====================================
 !
 if(allocated(E%Eo)) then
   E_bare=E%Eo(:RT_bands(2),:,:)
 else
   E_bare=E%E(:RT_bands(2),:,:)
 endif
 !
 call Bare_Hamiltonian(E,k,k)
 !
 ! Reference Hartree and XC
 !==========================
 do i_sp_pol=1,n_sp_pol
   do ik=1,nXkibz
     !
     M_tmp=cZERO
     call V_real_space_to_H(ik,i_sp_pol,V_hartree_sc,M_tmp,WF,'def') 
     HARTREE0(:,:,ik,i_sp_pol)=M_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
     !
     M_tmp=cZERO
     call V_real_space_to_H(ik,i_sp_pol,V_xc,        M_tmp,WF,'xc ') 
     XC0(:,:,ik,i_sp_pol)=M_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
     !
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
 ! Reference density
 !====================
 rho_reference=rho_n
 if(n_spin>1) magn_reference=magn_n
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
 if (l_NE_with_fields) then
   !
   call RT_Ext_fields(A_ext,NE_time)
   !
   ! If RESTARTing A_vecpot is read from GF database
   !
   if (NE_i_start_time==1) A_vecpot=A_ext
   ! 
   ! To properly initiate the loop the A@To must be stored in A_vecpot_previous
   ! 
   A_vecpot_previous = A_vecpot
   !
 endif
 !
 call msg('r','')
 !
 ! Apply QP-corrections 
 !======================
 !
 do ib=RT_bands(1),RT_bands(2)
   Ho(ib,ib,1:nXkibz,1:n_sp_pol)=Ho(ib,ib,1:nXkibz,1:n_sp_pol) &
&                              -E_bare(ib,1:nXkibz,1:n_sp_pol)+E%E(ib,1:nXkibz,1:n_sp_pol)
 enddo
 !
 do ib=1,RT_bands(1)-1
   Ho(ib,ib,1:nXkibz,1:n_sp_pol)=Ho(ib,ib,1:nXkibz,1:n_sp_pol) &
&                              -E_bare(RT_bands(1),1:nXkibz,1:n_sp_pol)+E%E(RT_bands(1),1:nXkibz,1:n_sp_pol)
 enddo
 !
 ! Store the starting Eigenvalues in Eo
 !======================================
 !
 if (.not.allocated(E%Eo)) then
   YAMBO_ALLOC(E%Eo,(RT_bands(2),nXkibz,n_sp_pol))
 endif
 E%Eo(1:RT_bands(2),1:nXkibz,1:n_sp_pol)=E%E(1:RT_bands(2),1:nXkibz,1:n_sp_pol)
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
 contains
   !
   subroutine RT_initialize_local()
     !
     G_lesser          =cZERO
     G_lesser_reference=cZERO
     dG_lesser         =cZERO
     !
     V_xc_sc           =rZERO
     V_hartree_sc      =rZERO
     H_nl_sc           =cZERO
     HARTREE0          =cZERO
     XC0               =cZERO
     !
     rho_reference     =rZERO
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
     ! Defined from occupations %f
     ! In case RT_apply has been used the reference is
     ! defined from the %fo occupations
     !
     do ib=RT_bands(1),RT_bands(2)
       do ik=1,k%nibz
         do ik_RT_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
           ik_E_rim=ik_RT_rim-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
           if (l_RT_uses_E_DbGd) then
             G_lesser(ib,ib,ik_RT_rim,1) =cI*E%f_DbGd(ib,ik_E_rim,1)
             if (allocated(E%W))  E%W_DbGd(ib,ik_E_rim,1)=E%W(ib,ik,1)
             if (allocated(E%fo)) G_lesser_reference(ib,ib,ik_RT_rim) =cI*(E%f_DbGd(ib,ik_E_rim,1)-E%df_DbGd(ib,ik_E_rim,1))
           else
             G_lesser(ib,ib,ik,1) =cI*E%f(ib,ik,1)
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
   !
end subroutine RT_start_and_restart

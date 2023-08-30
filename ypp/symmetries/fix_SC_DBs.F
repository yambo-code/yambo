!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): CA DS
!
subroutine fix_SC_DBs(k,k_save,kpoints_map,action_kind)
 !
 use pars,                ONLY:SP,lchlen
 use drivers,             ONLY:l_sc_run
 use com,                 ONLY:core_io_path,more_io_path
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:manage_action,OP_WR_CL,NONE,OP_WR,OP_RD,VERIFY,&
&                              OP_RD_CL,DUMP,RD_CL_IF_END,WR_CL_IF_END
 use SC,                  ONLY:SC_bands
 use hamiltonian,         ONLY:H_rotation,H_nl_sc,l_sc_V_is_local,H_potential
 use electrons,           ONLY:levels,E_reset,n_sp_pol
 use R_lattice,           ONLY:bz_samp,nkibz
 !
#include<memory.h>
 !
 type(bz_samp),intent(in) :: k
 type(bz_samp),intent(in) :: k_save
 integer,intent(in)       :: kpoints_map(2,k%nibz)
 !
 integer,intent(in)       :: action_kind
 !
 ! Work space
 !
 type(levels)          :: en_sc,en_sc_expanded
 !
 character(lchlen)     :: core_io_path_save
 !
 integer               :: ID
 integer               :: io_SC_E_err,io_SC_V_err,io_SC_WF_err,IO_ACT
 integer               :: ik,i_sp_pol,i_frag
 !
 complex(SP), allocatable :: full_H(:,:,:,:)
 !
 ! External functions
 !
 integer, external :: io_SC_components
 !
 !
 if(action_kind==2) return
 !
 core_io_path_save=core_io_path
 !
 ! Check presence of SC DBs
 !
 call E_reset(en_sc)
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
 io_SC_WF_err=io_SC_components('WF',en_sc,ID)
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
 io_SC_E_err=io_SC_components('E',en_sc,ID)
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
 io_SC_V_err=io_SC_components('V',en_sc,ID)
 !
 if (io_SC_E_err/=0.or.io_SC_WF_err/=0.or.io_SC_V_err/=0) io_SC_E_err=-1 
 !  
 if(io_SC_E_err==-1) return
 !
 call section('=',"SC wavefunctions and energies")
 !===============================
 !
 ! Energies
 !
 en_sc%nb=SC_bands(2)
 en_sc%nk=k_save%nibz
 YAMBO_ALLOC(en_sc%E,(en_sc%nb,en_sc%nk,n_sp_pol))
 !
 nkibz=k_save%nibz
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1,2/),ID=ID)
 io_SC_E_err=io_SC_components('E',en_sc,ID)
 !
 call map_energies(en_sc,en_sc_expanded,k,kpoints_map)
 !
 nkibz=k%nibz
 !
 core_io_path=more_io_path
 call io_control(ACTION=OP_WR_CL,COM=NONE,MODE=DUMP,SEC=(/1,2/),ID=ID)
 io_SC_E_err=io_SC_components('E',en_sc_expanded,ID)
 !
 !  Wave-function
 !
 YAMBO_ALLOC(H_rotation,(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),k%nibz,n_sp_pol))
 !
 nkibz=k_save%nibz
 !
 core_io_path=core_io_path_save
 call io_control(ACTION=OP_RD,COM=NONE,MODE=VERIFY,SEC=(/1/),ID=ID)
 io_SC_WF_err=io_SC_components('WF',en_sc,ID)
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,k_save%nibz
     i_frag=ik+(i_sp_pol-1)*k_save%nibz
     IO_ACT=manage_action(RD_CL_IF_END,i_frag,1,k_save%nibz*n_sp_pol)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/1+i_frag/),ID=ID)
     io_SC_WF_err=io_SC_components('WF',en_sc,ID)
   enddo
 enddo
 !
 nkibz=k%nibz
 !
 core_io_path=more_io_path
 call io_control(ACTION=OP_WR,COM=NONE,SEC=(/1/),ID=ID)
 io_SC_WF_err=io_SC_components('WF',en_sc,ID)
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,k_save%nibz
     i_frag=ik+(i_sp_pol-1)*k_save%nibz
     IO_ACT=manage_action(WR_CL_IF_END,i_frag,1,k_save%nibz*n_sp_pol)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/1+i_frag/),ID=ID)
     io_SC_WF_err=io_SC_components('WF',en_sc,ID)
   enddo
 enddo
 !
 do i_sp_pol=1,n_sp_pol
   do ik=k_save%nibz+1,k%nibz
     H_rotation(:,:,ik,i_sp_pol)=H_rotation(:,:,kpoints_map(1,ik),i_sp_pol)
     i_frag=ik+(i_sp_pol-1)*k_save%nibz
     IO_ACT=manage_action(WR_CL_IF_END,i_frag,1,k_save%nibz*n_sp_pol)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/1+i_frag/),ID=ID)
     io_SC_WF_err=io_SC_components('WF',en_sc,ID)
   enddo
 enddo
 !
 YAMBO_FREE(H_rotation)
 !
 ! Copy the potential
 !
 call XC_potentials(H_potential)
 call XC_additional_SC_potentials()
 !
 if(.not.l_sc_V_is_local) then
   !
   YAMBO_ALLOC(H_nl_sc,(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2)))
   !
   YAMBO_ALLOC(full_H,(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),k_save%nibz,n_sp_pol))
   !
   nkibz=k_save%nibz
   !
   core_io_path=core_io_path_save
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID)
   io_SC_V_err=io_SC_components('V',en_sc,ID)
   !
   do i_sp_pol=1,n_sp_pol
     do ik=1,k_save%nibz
       i_frag=ik+(i_sp_pol-1)*k_save%nibz
       IO_ACT=manage_action(RD_CL_IF_END,i_frag,1,k_save%nibz*n_sp_pol)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/1+i_frag/),ID=ID)
       io_SC_V_err=io_SC_components('V',en_sc,ID)
       full_H(:,:,ik,i_sp_pol)=H_nl_sc(:,:)
     enddo
   enddo
   !
   nkibz=k%nibz
   !
   core_io_path=more_io_path
   call io_control(ACTION=OP_WR,COM=NONE,SEC=(/1/),ID=ID)
   io_SC_V_err=io_SC_components('V',en_sc,ID)
   !
   do i_sp_pol=1,n_sp_pol
     do ik=1,k_save%nibz
       H_nl_sc(:,:)=full_H(:,:,ik,i_sp_pol)
       i_frag=ik+(i_sp_pol-1)*k_save%nibz
       IO_ACT=manage_action(WR_CL_IF_END,i_frag,1,k_save%nibz*n_sp_pol)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/1+i_frag/),ID=ID)
       io_SC_V_err=io_SC_components('V',en_sc,ID)
     enddo
   enddo
   !
   do i_sp_pol=1,n_sp_pol
     do ik=k_save%nibz+1,k%nibz
       H_nl_sc(:,:)=full_H(:,:,kpoints_map(1,ik),i_sp_pol)
       i_frag=ik+(i_sp_pol-1)*k_save%nibz
       IO_ACT=manage_action(WR_CL_IF_END,i_frag,1,k_save%nibz*n_sp_pol)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/1+i_frag/),ID=ID)
       io_SC_V_err=io_SC_components('V',en_sc,ID)
     enddo    
   enddo    
   !
   YAMBO_FREE(H_nl_sc)
   !
   YAMBO_FREE(full_H)
   !
 endif
 !
 core_io_path=core_io_path_save
 !
 l_sc_run=.true. ! to not read again SC wavefunctions
 !
end subroutine

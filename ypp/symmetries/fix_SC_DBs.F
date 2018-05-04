!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA DS
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
subroutine fix_SC_DBs(k,k_save,kpoints_map,action_kind)
 !
 use pars,                ONLY:SP,lchlen
 use drivers,             ONLY:l_sc_run
 use com,                 ONLY:core_io_path,more_io_path
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_WR,OP_RD,VERIFY,&
&                              OP_RD_CL,DUMP,RD_CL_IF_END,WR_CL_IF_END
 use SC,                  ONLY:l_sc_V_is_local,H_nl_sc,SC_bands
 use hamiltonian,         ONLY:H_rotation
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
 integer               :: io_SC_E_err,io_SC_V_err,io_SC_WF_err
 integer               :: ik
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
 do ik=1,k_save%nibz
   call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ik+1+(i_sp_pol-1)*k_save%nibz/),ID=ID)
   io_SC_WF_err=io_SC_components('WF',en_sc,ID)
 enddo
 !
 nkibz=k%nibz
 !
 core_io_path=more_io_path
 call io_control(ACTION=OP_WR,COM=NONE,SEC=(/1/),ID=ID)
 io_SC_WF_err=io_SC_components('WF',en_sc,ID)
 !
 do ik=1,k_save%nibz
   call io_control(ACTION=WR_CL_IF_END,SEC=(/ik+1+(i_sp_pol-1)*k%nibz/),ID=ID)
   io_SC_WF_err=io_SC_components('WF',en_sc,ID)
 enddo
 !
 do ik=k_save%nibz+1,k%nibz
   H_rotation(:,:,ik,:)=H_rotation(:,:,kpoints_map(1,ik),:)
   call io_control(ACTION=WR_CL_IF_END,SEC=(/ik+1+(i_sp_pol-1)*k%nibz/),ID=ID)
   io_SC_WF_err=io_SC_components('WF',en_sc,ID)
 enddo
 !
 YAMBO_FREE(H_rotation)
 !
 ! Copy the potential
 !
 call XC_switch()
 if(.not.l_sc_V_is_local) then
   !
   YAMBO_ALLOC(H_nl_sc,(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2)))
   !
   YAMBO_ALLOC(full_H(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),k_save%nibz,n_sp_pol))
   !
   nkibz=k_save%nibz
   !
   core_io_path=core_io_path_save
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID)
   io_SC_V_err=io_SC_components('V',en_sc,ID)
   !
   do i_sp_pol=1,n_sp_pol
   do ik=1,k_save%nibz
     call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1+(i_sp_pol-1)*k_save%nibz/),ID=ID)
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
     call io_control(ACTION=WR_CL_IF_END,SEC=(/ik+1+(i_sp_pol-1)*k%nibz/),ID=ID)
     io_SC_V_err=io_SC_components('V',en_sc,ID)
   enddo
   enddo
   !
   do i_sp_pol=1,n_sp_pol
   do ik=k_save%nibz+1,k%nibz
     H_nl_sc(:,:)=full_H(:,:,kpoints_map(1,ik),i_sp_pol)
     call io_control(ACTION=WR_CL_IF_END,SEC=(/ik+1+(i_sp_pol-1)*k%nibz/),ID=ID)
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

!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine fix_ATOMPROJs(E,k,k_save,kpoints_map,old_nsym,old_dl_sop,old_sop_inv,S_contains_TR,action_kind)
 !
 use pars,                ONLY:SP,lchlen,cZERO
 use com,                 ONLY:msg,core_io_path,more_io_path,warning
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_CL,OP_WR,OP_RD,VERIFY,&
&                              OP_APP,cp_file,OP_RD_CL,RD_CL,RD,DUMP
 use electrons,           ONLY:n_sp_pol
 use LIVE_t,              ONLY:live_timing
 use stderr,              ONLY:intc
 use R_lattice,           ONLY:bz_samp
 use electrons,           ONLY:levels
 use atom_proj
 !
#include<memory.h>
 !
 type(levels),intent(in)  :: E
 !
 type(bz_samp),intent(in) :: k
 type(bz_samp),intent(in) :: k_save
 integer,intent(in)       :: kpoints_map(2,k%nibz)
 !
 integer,intent(in)       :: old_nsym
 real(SP),intent(in)      :: old_dl_sop(3,3,old_nsym)
 integer,intent(in)       :: old_sop_inv(old_nsym)
 logical,intent(in)       :: S_contains_TR(old_nsym)
 !
 integer,intent(in)       :: action_kind
 !
 ! Work space
 !
 character(lchlen)     :: core_io_path_save,fragment_name,fragment_name_new
 integer               :: n_steps,ID
 integer               :: ierr,io_err
 !
 complex(SP),allocatable :: atom_proj_store(:,:,:,:)
 !
 ! Dummies
 !
 integer               :: is,i_sp_pol,ik,ik_save,ifrag,ifrag_new
 !
 ! External functions
 !
 integer, external :: io_ATMPROJ_pwscf
 !
 ! Check the presence of ATOMPROJs DB
 !
 n_steps=k%nibz-k_save%nibz
 !
 io_err = -1
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_err=io_ATMPROJ_pwscf(ID)
 !
 if (io_err/=0) return
 !
 call section('=',"Atomic projections (PWSCF)")
 !
 call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=io_ATMPROJ_pwscf(ID)
 !
 YAMBO_ALLOC(atom_proj_store,(n_atom_wfc,n_proj_bands,k_save%nibz,n_sp_pol))
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,k_save%nibz
     !
     ifrag=ik+(i_sp_pol-1)*k_save%nibz
     !
     if (ifrag< n_sp_pol*k_save%nibz) call io_control(ACTION=RD,SEC=(/ifrag+1/),ID=ID)
     if (ifrag==n_sp_pol*k_save%nibz) call io_control(ACTION=RD_CL,SEC=(/ifrag+1/),ID=ID)
     io_err=io_ATMPROJ_pwscf(ID)
     !
     atom_proj_store(:,:,ik,i_sp_pol)=atomic_projections(:,:,ik,i_sp_pol)
     !
   enddo
 enddo
 !
 core_io_path_save=core_io_path
 core_io_path=more_io_path
 !
 YAMBO_FREE(atomic_projections)
 YAMBO_ALLOC(atomic_projections,(n_atom_wfc,n_proj_bands,k%nibz,n_sp_pol))
 atomic_projections=cZERO
 !
 select case(action_kind)
 case(1)
   !
   call msg('s',':: Copying existing database ...')
   !
   call cp_file(trim(core_io_path_save)//"/SAVE/ns.atom_proj_pwscf",trim(more_io_path)//"/SAVE",ierr)
   do i_sp_pol=1,n_sp_pol
     do ik=1,k_save%nibz
       ifrag=ik+(i_sp_pol-1)*k_save%nibz
       ifrag_new=ik+(i_sp_pol-1)*k%nibz
       fragment_name    ='ns.atom_proj_pwscf_fragment_'//trim(intc(ifrag))
       fragment_name_new='ns.atom_proj_pwscf_fragment_'//trim(intc(ifrag_new))
       call cp_file(trim(core_io_path_save)//"/SAVE/"//trim(fragment_name),&
&                   trim(more_io_path)//"/SAVE/"//trim(fragment_name),ierr)
       fragment_name='s.atom_proj_pwscf_fragment_'//trim(intc(ifrag))
       fragment_name_new='s.atom_proj_pwscf_fragment_'//trim(intc(ifrag_new))
       call cp_file(trim(core_io_path_save)//"/SAVE/"//trim(fragment_name),&
&                   trim(more_io_path)//"/SAVE/"//trim(fragment_name),ierr)
     enddo
   enddo
   !
   call msg('s','done')
   !
   !
   if(n_steps>0) call live_timing('Atomic projections rotation',n_steps*n_sp_pol)
   !
   call warning(' After rotation m projections will be meaningless')
   !
   ! 2019/01/10: to fix this the rotation matrix in the angular momentum
   !             channel should be used
   !
   do i_sp_pol=1,n_sp_pol
     do ik=k_save%nibz+1,k%nibz
       !
       ifrag=ik+(i_sp_pol-1)*k%nibz
       !
       ik_save=kpoints_map(1,ik)
       is=kpoints_map(2,ik)
       !
       atomic_projections(:,:,ik,i_sp_pol)=atom_proj_store(:,:,ik_save,i_sp_pol)
       !
       call io_control(ACTION=OP_APP_CL,COM=NONE,SEC=(/ifrag+1/),ID=ID)      
       io_err=io_ATMPROJ_pwscf(ID)
       !
       call live_timing(steps=1)
       !
     enddo
   enddo
   !
   if(n_steps>0) call live_timing()
   !
   !
 case(2)
   !
   if(io_err==0) call msg('s',':: Atomic projections reduction...')
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
   io_err=io_ATMPROJ_pwscf(ID) 
   !
   do i_sp_pol=1,n_sp_pol
     do ik=1,k%nibz
       !
       ifrag=ik+(i_sp_pol-1)*k%nibz
       !
       atomic_projections(:,:,ik,i_sp_pol) =atom_proj_store(:,:,kpoints_map(1,ik),i_sp_pol)
       !
       call io_control(ACTION=OP_APP_CL,COM=NONE,SEC=(/ifrag+1/),ID=ID)      
       io_err=io_ATMPROJ_pwscf(ID)
       !
     enddo
   enddo
   !
   YAMBO_FREE(atom_proj_store)
   !
   call msg('s','done')
   !
 end select
 core_io_path=core_io_path_save
 !
end subroutine fix_ATOMPROJs

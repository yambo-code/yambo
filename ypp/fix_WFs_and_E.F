!
! Copyright (C) 2000-2012 D. Sangalli, C. Attaccalite and the YAMBO team 
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
subroutine fix_WFs_and_E(E,k,k_save,kpoints_map,wf_ncx_save,old_nsym,S_contains_TR,action_kind)
 !
 use pars,                ONLY:SP,lchlen
 use com,                 ONLY:msg,warning,core_io_path,more_io_path
 use drivers,             ONLY:l_sc_run
 use memory_m,            ONLY:mem_est
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,OP_WR,OP_RD,VERIFY,&
&                              OP_APP,cp_file,rm_file,cp_directory,Fragmented_IO,mk_dir, &
&                              io_fragmented,OP_RD_CL,RD_CL,RD,DUMP,RD_CL_IF_END,WR_CL_IF_END
 use wave_func,           ONLY:ioWF,wf_nc_k, wf_igk,wf_ncx,wf_nb_io, &
&                              wf_ng,wf_state,WF_load,wf_state,wf,WF_free,wf_nb_io_groups
 use electrons,           ONLY:levels,E_reset,n_spin,n_sp_pol
 use timing,              ONLY:live_timing
 use stderr,              ONLY:intc
 use R_lattice,           ONLY:bz_samp,g_rot
 use D_lattice,           ONLY:spin_sop
 !
 implicit none
 !
 type(levels)             :: E
 !
 type(bz_samp),intent(in) :: k
 type(bz_samp),intent(in) :: k_save
 integer,intent(in)       :: kpoints_map(2,k%nibz)
 integer,intent(in)       :: wf_ncx_save 
 !
 integer,intent(in)       :: old_nsym
 logical,intent(in)       :: S_contains_TR(old_nsym)
 !
 integer,intent(in)       :: action_kind
 !
 ! Work space
 !
 type(levels)          :: E_expanded,E_reduced
 !
 character(lchlen)     :: core_io_path_save,fragment_name
 integer               :: n_steps
 integer               :: ACTION_,ID
 integer               :: ierr,io_err
 !
 integer,allocatable      :: ic_rot_table(:) 
 real(SP),    allocatable :: wf_disk(:,:,:,:)
 complex(SP), allocatable :: wf_tmp(:,:)
 !
 ! Dummies
 !
 integer               :: i1,is,ik,ik_save
 integer               :: ib,ibm,i_wf,i_spin
 integer               :: ic,ic_rot,ig,ig_rot
 !
 ! External functions
 !
 integer, external :: ioDB1
 !
 !
 if (trim(core_io_path)/=trim(more_io_path)) then
   call mk_dir(more_io_path)
   call mk_dir(trim(more_io_path)//'/SAVE')
 endif
 !
#if defined _YPP_SC || defined _YPP_RT
   call fix_SC_DBs(k,k_save,kpoints_map,action_kind)
#endif
 !
 core_io_path_save=core_io_path
 core_io_path=more_io_path
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID)  
 Fragmented_IO=io_fragmented(ID)
 io_err=ioWF(ID)
 !
 select case(action_kind)
 case(1)
   !
   call section('=',"Energies")
   !===========================
   !
   call map_energies(E,E_expanded,k,kpoints_map)
   !
   call section('=',"Wavefunctions")
   !===============================
   !
   if (trim(core_io_path_save)/=trim(more_io_path).and.wf_ncx_save==wf_ncx) then
     !
     call msg('s',':: Copying existing database ...')
     !
     call cp_file(trim(core_io_path_save)//"/SAVE/s.wf",trim(more_io_path)//"/SAVE",ierr)
     call cp_file(trim(core_io_path_save)//"/SAVE/ns.wf",trim(more_io_path)//"/SAVE",ierr)
     do ik=1,k_save%nibz
       do i1=1,wf_nb_io_groups
         fragment_name='ns.wf_fragments_'//trim(intc(ik))//'_'//trim(intc(i1))
         call cp_directory(trim(core_io_path_save)//"/SAVE/"//trim(fragment_name),trim(more_io_path)//"/SAVE",ierr)
         fragment_name='s.wf_fragments_'//trim(intc(ik))//'_'//trim(intc(i1))
         call cp_directory(trim(core_io_path_save)//"/SAVE/"//trim(fragment_name),trim(more_io_path)//"/SAVE",ierr)
       enddo
     enddo
     !
     call msg('l','done')
     !
   else
     call warning(':: Existing database will be overwritten')
   endif
   !
   !call WF_load(wf_ng,1,(/1,E%nb/),(/1,k_save%nibz/),space='C',title='')
   !
   ! Rotating wavefunctions by components
   !
   allocate(wf_disk(2,wf_nb_io,wf_ncx,n_spin))
   call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))
   !
   if(wf_ncx_save/=wf_ncx) then
     !
     call live_timing('WFs comp. reduction',k_save%nibz)
     ! ============================================
     !
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
     io_err=ioWF(ID)
     !
     ACTION_=OP_APP_WR_CL
     if(Fragmented_IO) ACTION_=OP_WR_CL
     !
     do ik=1,k_save%nibz
       !
       ik_save=kpoints_map(1,ik)
       call WF_load(wf_ng,1,(/1,E%nb/),(/ik_save,ik_save/),space='C',title='')
       !
       call io_control(ACTION=ACTION_,COM=NONE,SEC=(/ik+1,1/),ID=ID)
       ! 
       wf_disk=0._SP
       !
       do ib=1,E%nb
         do i_spin=1,n_spin
           !
           i_wf=wf_state(ib,ik_save,i_spin)
           !
           wf_disk(1,ib,:wf_nc_k(ik),i_spin)=  real(wf(:wf_nc_k(ik_save),i_wf))
           wf_disk(2,ib,:wf_nc_k(ik),i_spin)= aimag(wf(:wf_nc_k(ik_save),i_wf))
           !
         enddo
       enddo
       !   
       io_err=ioWF(ID,wf=wf_disk)
       call WF_free()
       !
       call live_timing(steps=1)
     enddo
     !
     call live_timing()
     !
   endif
   !
   call section('=',"Final I/O")
   !============================
   !
   n_steps=k%nibz-k_save%nibz
   if(n_steps>0) call live_timing('WFs rotation',n_steps)
   !
   allocate(ic_rot_table(wf_ncx))
   allocate(wf_tmp(wf_ncx,n_spin))
   !
   ACTION_=OP_APP_WR_CL
   if(Fragmented_IO) ACTION_=OP_WR_CL
   !
   do ik=k_save%nibz+1,k%nibz 
     !      
     ik_save=kpoints_map(1,ik)
     is=kpoints_map(2,ik)
     !
     call WF_load(wf_ng,1,(/1,E%nb/),(/ik_save,ik_save/),space='C',title='')
     !
     call io_control(ACTION=ACTION_,COM=NONE,SEC=(/ik+1,1/),ID=ID)
     !
     ic_rot_table=-1
     do ic=1,wf_nc_k(ik_save)
       ig_rot=g_rot(is,wf_igk(ic,ik_save))
       do ic_rot=1,wf_nc_k(ik)
         ig=wf_igk(ic_rot,ik)
         if(ig==ig_rot) exit
       enddo
       ic_rot_table(ic)=ic_rot
     enddo
     !
     wf_disk=(0.,0.)
     !
     do ib=1,E%nb
       !
       wf_tmp=(0._SP,0._SP)
       !
       do i_spin=1,n_spin
         !
         i_wf=wf_state(ib,ik_save,i_spin)
         !
         forall(ic=1:wf_nc_k(ik_save)) wf_tmp(ic_rot_table(ic),i_spin)=wf(ic,i_wf)
         !
       enddo
       !
       forall(ic=1:wf_nc_k(ik)) wf_tmp(ic,:)=matmul(spin_sop(:,:,is),wf_tmp(ic,:))
       if ( S_contains_TR(is) ) wf_tmp=conjg(wf_tmp)
       !
       wf_disk(1,ib,1:wf_ncx,:)= real(wf_tmp(:wf_ncx,:))
       wf_disk(2,ib,1:wf_ncx,:)= aimag(wf_tmp(:wf_ncx,:))
       !
     enddo
     !
     io_err=ioWF(ID,wf=wf_disk)
     call WF_free()
     !
     call live_timing(steps=1)
     !
   enddo
   !
   if(n_steps>0) call live_timing()
   ! 
   if(Fragmented_IO) then
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
     io_err=ioWF(ID)
   endif
   !
   deallocate(ic_rot_table,wf_disk)
   call mem_est("wf_disk")
   deallocate(wf_tmp)
   !
 case(2)
   !
   call section('=',"Energies")
   !===========================
   !
   call E_reset(E_reduced)
   E_reduced%nb=E%nb
   E_reduced%nk=k%nibz
   allocate(E_reduced%E(E%nb,k%nibz,n_sp_pol))
   !
   do ik=1,k%nibz
     E_reduced%E(:,ik,:)=E%E(:,kpoints_map(1,ik),:)
   enddo
   !
   call section('=',"Final I/O")
   !
   if (trim(core_io_path)==trim(more_io_path)) call warning(':: Existing database will be overwritten')
   !
   core_io_path_save=core_io_path
   core_io_path=more_io_path
   !
   call msg('s',':: WFs ...')
   !
   !call WF_load(wf_ng,1,(/1,E%nb/),(/1,k_save%nibz/),space='C',title='')
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
   io_err=ioWF(ID)
   !
   allocate(wf_disk(2,wf_nb_io,wf_ncx,n_spin))
   call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))
   !
   ACTION_=OP_APP_WR_CL
   if(Fragmented_IO) ACTION_=OP_WR_CL
   !
   do ik=1,k%nibz
     !
     wf_disk=0._SP
     !
     ik_save=kpoints_map(1,ik)
     call WF_load(wf_ng,1,(/1,E%nb/),(/ik_save,ik_save/),space='C',title='')
     !
     do ib=1,E%nb
       do i_spin=1,n_spin
         i_wf=wf_state(ib,ik_save,i_spin)
         wf_disk(1,ib,1:wf_ncx,i_spin)= real(wf(1:wf_ncx,i_wf))
         wf_disk(2,ib,1:wf_ncx,i_spin)= aimag(wf(1:wf_ncx,i_wf))
       enddo
     enddo
     !
     call WF_free()
     !
     call io_control(ACTION=ACTION_,COM=NONE,SEC=(/ik+1,1/),ID=ID)
     io_err=ioWF(ID,wf=wf_disk)
     !
   enddo
   !
   if(Fragmented_IO) then
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
     io_err=ioWF(ID)
   endif
   !
   call msg('l','done')
   !
 end select
 !
 call msg('s',':: DB1 ...')
 !
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 if(action_kind==1) io_err=ioDB1(E_expanded,k,ID)
 if(action_kind==2) io_err=ioDB1(E_reduced,k,ID)
 !
 core_io_path=core_io_path_save
 !
 call msg('l','done')
 !
 ! (n)db clean
 !=============
 !
 if (trim(core_io_path)==trim(more_io_path)) then
   call msg('s',':: Cleaning ...')
   call rm_file((trim(core_io_path))//"/SAVE/ndb.kindx")
   call rm_file((trim(core_io_path))//"/SAVE/ndb.gops")
   call rm_file((trim(core_io_path))//"/SAVE/ndb.dipoles")
   call rm_file((trim(core_io_path))//"/SAVE/db.dipoles")
   call rm_file((trim(core_io_path))//"/SAVE/ndb.P_and_P2")
   call rm_file((trim(core_io_path))//"/SAVE/db.P_and_P2")
   call rm_file((trim(core_io_path))//"/SAVE/db.kindx")
   call rm_file((trim(core_io_path))//"/SAVE/db.gops")
   call msg('l','done')
 endif
 !
 ! CLEAN
 !=======
 if(action_kind==1) call E_reset(E_expanded)
 if(action_kind==2) call E_reset(E_reduced)
 !
end subroutine
!
subroutine map_energies(E,E_expanded,k,kpoints_map)
 !
 use electrons,           ONLY:levels,E_reset,n_sp_pol
 use R_lattice,           ONLY:bz_samp
 !
 implicit none
 !
 type(levels), intent(in)     :: E
 type(levels), intent(out)    :: E_expanded
 type(bz_samp), intent(in)    :: k
 integer,       intent(in)    :: kpoints_map(2,k%nibz)
 !
 ! Work Space
 !
 integer :: ik
 !
 call E_reset(E_expanded)
 E_expanded%nb=E%nb
 E_expanded%nk=k%nibz
 allocate(E_expanded%E(E%nb,k%nibz,n_sp_pol))
 !
 do ik=1,k%nibz
   E_expanded%E(:,ik,:)=E%E(:,kpoints_map(1,ik),:)
 enddo
 !
end subroutine map_energies

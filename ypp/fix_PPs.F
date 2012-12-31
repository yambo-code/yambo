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
subroutine fix_PPs(E,k,k_save,kpoints_map,wf_ng_save,old_nsym,old_dl_sop,old_sop_inv,S_contains_TR,action_kind)
 !
 use pars,                ONLY:SP,lchlen
 use com,                 ONLY:msg,warning,core_io_path,more_io_path
 use drivers,             ONLY:l_sc_run
 use memory_m,            ONLY:mem_est
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,OP_WR,OP_RD,VERIFY,&
&                              OP_APP,cp_file,rm_file,cp_directory,Fragmented_IO,mk_dir, &
&                              io_fragmented,OP_RD_CL,RD_CL,RD,DUMP,RD_CL_IF_END,WR_CL_IF_END
 use electrons,           ONLY:n_spin,n_sp_pol
 use pseudo,              ONLY:pp_kb,pp_kbd,n_atomic_species,pp_n_l_times_proj_max,PP_free,Vnl
 use timing,              ONLY:live_timing
 use X_m,                 ONLY:X_t
 use stderr,              ONLY:intc
 use wave_func,           ONLY:wf_nb_io_groups,wf_ng
 use R_lattice,           ONLY:bz_samp,ng_closed,g_rot,nXkibz,ng_vec
 use D_lattice,           ONLY:n_atomic_species
 use electrons,           ONLY:levels
 !
 implicit none
 !
 type(levels),intent(in)  :: E
 !
 type(bz_samp),intent(in) :: k
 type(bz_samp),intent(in) :: k_save
 integer,intent(in)       :: kpoints_map(2,k%nibz)
 integer,intent(in)       :: wf_ng_save
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
 type(X_t)             :: X_Vnl
 !
 character(lchlen)     :: core_io_path_save,fragment_name
 integer               :: n_steps,ID
 integer               :: ierr,ioKB_err,io_Vnl_err
 !
 real(SP),allocatable     :: pp_kb_store(:,:,:,:,:),pp_kbd_store(:,:,:,:,:)
 complex(SP), allocatable :: Vnl_store(:,:,:,:,:)
 !
 ! Dummies
 !
 integer               :: i1,is,ik,ik_save
 integer               :: ib,ibm,i_spin
 !
 ! External functions
 !
 integer, external :: ioKB_PP,io_Vnl
 !
 ! Check the presence of PPs DBs
 !
 n_steps=k%nibz-k_save%nibz
 !
 ! Abinit
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 ioKB_err=ioKB_PP(ID) 
 !
 ! PWscf
 X_Vnl%ib=(/1,E%nb/)       ! full bands range
 X_Vnl%ngostnts=wf_ng_save ! all G-vectors
 nXkibz=k_save%nibz
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_Vnl_err=io_Vnl(X_Vnl,E,ID)
 !
 if(ioKB_err/=0.and.io_Vnl_err/=0) return
 !
 if(ioKB_err==0)   call section('=',"PseudoPotentials (PWscf)")
 if(io_Vnl_err==0) call section('=',"PseudoPotentials (PWscf)")
 !
 ! Read PP DBs
 !
 if(ioKB_err==0) then
   !
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
   ioKB_err=ioKB_PP(ID) 
   !  
   allocate(pp_kb_store(ng_vec,n_atomic_species,pp_n_l_times_proj_max,n_spin,k%nibz))
   allocate(pp_kbd_store(ng_vec,n_atomic_species,pp_n_l_times_proj_max,n_spin,k%nibz))
   call mem_est("pp_kb_store pp_kbd_store",(/size(pp_kb),size(pp_kbd)/),elements_kind=(/SP,SP/))
   !
   do ik=1,k_save%nibz
     !
     if (ik< k_save%nibz) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==k_save%nibz) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)      
     ioKB_err=ioKB_PP(ID)
     !
     pp_kb_store(:,:,:,:,ik) =pp_kb(:,:,:,:)
     pp_kbd_store(:,:,:,:,ik)=pp_kbd(:,:,:,:)
     !
   enddo
   !
 endif
 !
 if(io_Vnl_err==0) then
   allocate( Vnl_store(3,E%nb,E%nbm,k_save%nibz,n_sp_pol) )
   Vnl_store=(0._SP,0._SP)
   call mem_est("Vnl_store",(/size(Vnl_store)/),elements_kind=(/SP/))
   !
   call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),ID=ID)
   io_Vnl_err=io_Vnl(X_Vnl,E,ID)
   !
   Vnl_store=Vnl
   !
   deallocate(Vnl)
   !   
 endif
 !
 core_io_path_save=core_io_path
 core_io_path=more_io_path
 !
 select case(action_kind)
 case(1)
   !
   if (trim(core_io_path_save)/=trim(more_io_path)) then
     if(ioKB_err==0) then
       !
       call msg('s',':: Copying existing database ...')
       !
       call cp_file(trim(core_io_path_save)//"/SAVE/s.kb_pp",trim(more_io_path)//"/SAVE",ierr)
       call cp_file(trim(core_io_path_save)//"/SAVE/ns.kb_pp",trim(more_io_path)//"/SAVE",ierr)
       do ik=1,k_save%nibz
         do i1=1,wf_nb_io_groups
           fragment_name='ns.kb_pp_fragments_'//trim(intc(ik))//'_'//trim(intc(i1))
           call cp_directory(trim(core_io_path_save)//"/SAVE/"//trim(fragment_name),trim(more_io_path)//"/SAVE",ierr)
           fragment_name='s.kb_pp_fragments_'//trim(intc(ik))//'_'//trim(intc(i1))
           call cp_directory(trim(core_io_path_save)//"/SAVE/"//trim(fragment_name),trim(more_io_path)//"/SAVE",ierr)
         enddo
       enddo
       !
       call msg('l','done')
       !
     endif
   else
     call warning(':: Existing database will be overwritten')
   endif
   !
   ! Pseudo-potentials
   !
   if(ioKB_err==0) then 
     !
     pp_kb=0._SP
     pp_kbd=0._SP
     !
     if(n_steps>0) call live_timing('PPs rotation',n_steps)
     !
     do ik=k_save%nibz+1,k%nibz
       !
       ik_save=kpoints_map(1,ik)
       is=kpoints_map(2,ik)
       !
       pp_kb(1:ng_closed,:,:,:) =pp_kb_store(g_rot(old_sop_inv(is),1:ng_closed),:,:,:,ik_save)
       pp_kbd(1:ng_closed,:,:,:)=pp_kbd_store(g_rot(old_sop_inv(is),1:ng_closed),:,:,:,ik_save)
       !
       call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1/),ID=ID)      
       ioKB_err=ioKB_PP(ID)
       !
       call live_timing(steps=1)
       !
     enddo
     !
     deallocate(pp_kb_store,pp_kbd_store)
     call mem_est("pp_kb_store pp_kbd_store")
     !
     call PP_free()
     !
     if(n_steps>0) call live_timing()
     !
   endif
   !
   if(io_Vnl_err==0) then 
     !
     allocate(Vnl(3,E%nb,E%nbm,k%nibz,n_sp_pol))
     !
     Vnl(1:3, 1:E%nb, 1:E%nbm, 1:k_save%nibz, 1:n_sp_pol)=&
  &       Vnl_store(1:3, 1:E%nb, 1:E%nbm, 1:k_save%nibz, 1:n_sp_pol)
     !
     if(n_steps>0) call live_timing('PPs rotation',n_steps)
     !
     do ik=k_save%nibz+1,k%nibz
       !
       ik_save=kpoints_map(1,ik)
       is=kpoints_map(2,ik)
       !
       forall(ib=1:E%nb,ibm=1:E%nbm,i_spin=1:n_sp_pol) &
  &          Vnl(:,ib,ibm,ik,i_spin) = matmul( old_dl_sop(:,:,is), Vnl_store(:,ib,ibm,ik_save,i_spin) ) 
       ! Vnl is invariant under T-rev as iR and p ??
       if ( S_contains_TR(is) ) Vnl(:,:,:,ik,:)=conjg( Vnl(:,:,:,ik,:) )
       !
       call live_timing(steps=1)
       !
     enddo
     !
     X_Vnl%ngostnts=wf_ng
     nXkibz=k%nibz
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)      
     io_Vnl_err=io_Vnl(X_Vnl,E,ID)
     !
     deallocate(Vnl,Vnl_store)
     call mem_est("Vnl Vnl_store")
     !
     if(n_steps>0) call live_timing()
     !
   endif
   !
 case(2)
   !
   if(ioKB_err==0.or.io_Vnl_err==0) call msg('s',':: Reducing PPs DB...')
   !
   if(ioKB_err==0) then
     !
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
     ioKB_err=ioKB_PP(ID) 
     !
     do ik=1,k%nibz
       !
       pp_kb(:,:,:,:) =pp_kb_store(:,:,:,:,kpoints_map(1,ik))
       pp_kbd(:,:,:,:)=pp_kbd_store(:,:,:,:,kpoints_map(1,ik))     
       !
       call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1/),ID=ID)      
       ioKB_err=ioKB_PP(ID)
       !
     enddo
     !
     deallocate(pp_kb_store,pp_kbd_store)
     call mem_est("pp_kb_store pp_kbd_store")
     call PP_free()
     !
   endif
   !
   if(io_Vnl_err==0) then
     !
     allocate(Vnl(3,E%nb,E%nbm,k%nibz,n_sp_pol))
     !
     forall(ik=1:k%nibz) Vnl(:,:,:,ik,:) = Vnl_store(:,:,:,kpoints_map(1,ik),:)
     !
     X_Vnl%ngostnts=wf_ng
     nXkibz=k%nibz
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
     io_Vnl_err=io_Vnl(X_Vnl,E,ID)
     !
     deallocate(Vnl_store,Vnl)
     call mem_est("Vnl_store Vnl") 
     !
   endif
   !
   call msg('l','done')
   !
 end select
 !
 core_io_path=core_io_path_save
 !
end subroutine

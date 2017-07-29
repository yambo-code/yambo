!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine fix_WFs_and_E(E,k,k_save,kpoints_map,old_nsym,S_contains_TR,action_kind)
 !
 use pars,                ONLY:SP,lchlen,cZERO
 use com,                 ONLY:msg,core_io_path,more_io_path,error
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_WR,OP_RD,&
&                              cp_file,cp_directory,OP_RD_CL,RD_CL,frag_WF
 use wave_func,           ONLY:io_WF,wf_nc_k, wf_igk,wf_ncx,wf_nb_io,wf_ng, &
 &                             WF,wf_nb_io_groups
 use electrons,           ONLY:levels,E_reset,n_spin,n_spinor
 use LIVE_t,              ONLY:live_timing
 use stderr,              ONLY:intc
 use R_lattice,           ONLY:bz_samp,g_rot,ng_closed,nkibz
 use D_lattice,           ONLY:spin_sop
 use interfaces,          ONLY:WF_load
 use YPP_symm,            ONLY:wf_nc_k_save,wf_igk_save,wf_nc_k_reduced,wf_igk_reduced,&
&                              wf_nc_k_new,l_wf_nc_k_red,wf_igk_new,&
&                              wf_nc_k_new,wf_ncx_new,wf_ncx_save,wf_ng_new,wf_ng_save
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
 logical,intent(in)       :: S_contains_TR(old_nsym)
 !
 integer,intent(in)       :: action_kind
 !
 ! Work space
 !
 type(levels)          :: E_new
 !
 character(lchlen)     :: core_io_path_save,fragment_name
 integer               :: n_steps,nb1,nb2,ng_closed_save
 integer               :: ID
 integer               :: ierr,io_err
 !
 integer               :: wf_nb_io_save
 !
 integer,     allocatable :: ic_rot_table(:) 
 real(SP),    allocatable :: wf_disk(:,:,:,:)
 complex(SP), allocatable :: wf_tmp(:,:)
 !
 ! Dummies
 !
 integer               :: is,ik,ik_save
 integer               :: ib,ib_grp,ib_disk,i_wf,i_spin
 integer               :: ic,ic_rot,ig,ig_rot
 logical               :: l_found
 !
 ! External functions
 !
 integer, external :: io_DB1
 !
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID)
 io_err=io_WF(ID)
 !
 core_io_path_save=core_io_path
 core_io_path=more_io_path
 !
 call section('=',"Energies and WFs shells")
 !===========================================
 !
 ! Rotate spheres of G-vectors centered on kpts
 call fix_WFs_Gshells(k,k_save,kpoints_map,old_nsym,action_kind)
 !
 call map_energies(E,E_new,k,kpoints_map)
 !
 ! For some reason ioDB1 changes the value of ng_closed and of wf_nb_io
 ! This is a work-around to this problem
 ng_closed_save=ng_closed
 wf_nb_io_save=wf_nb_io
 !
 call msg('s',':: I/O...')
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=io_DB1(E_new,k,ID)
 call msg('l','done')
 !
 ng_closed=ng_closed_save
 wf_nb_io=wf_nb_io_save
 !
 call section('=',"Wavefunctions")
 !===============================
 !
 ! After I/O of DB1 I need to restore the old WFs shells
 ! to make sure that WF_load works properly
 !
 YAMBO_FREE(wf_nc_k)
 YAMBO_FREE(wf_igk)
 YAMBO_ALLOC(wf_nc_k,(k_save%nibz))
 YAMBO_ALLOC(wf_igk,(wf_ncx_save,k_save%nibz))
 !
 wf_nc_k=wf_nc_k_save
 wf_igk =wf_igk_save
 wf_ncx =wf_ncx_save
 wf_ng  =wf_ng_save 
 !
 select case(action_kind)
 !
 case(1)
   !
   if (.not.l_wf_nc_k_red) then
     !
     call msg('s',':: Copying existing database ...')
     !
     call cp_file(trim(core_io_path_save)//"/SAVE/s.wf",trim(more_io_path)//"/SAVE",ierr)
     call cp_file(trim(core_io_path_save)//"/SAVE/ns.wf",trim(more_io_path)//"/SAVE",ierr)
     do ik=1,k_save%nibz
       do ib_grp=1,wf_nb_io_groups
         fragment_name='s.wf_fragments_'//trim(intc(ik))//'_'//trim(intc(ib_grp))
         call cp_directory(trim(core_io_path_save)//"/SAVE/"//trim(fragment_name),trim(more_io_path)//"/SAVE",ierr)
         fragment_name='ns.wf_fragments_'//trim(intc(ik))//'_'//trim(intc(ib_grp))
         call cp_directory(trim(core_io_path_save)//"/SAVE/"//trim(fragment_name),trim(more_io_path)//"/SAVE",ierr)
       enddo
     enddo
     !
     call msg('l','done')
     !
   endif
   !
   ! Rotating wavefunctions by components
   !
   YAMBO_ALLOC(wf_disk,(2,wf_nb_io,wf_ncx_new,n_spin))
   !
   if(l_wf_nc_k_red) then
     !
     call live_timing('WFs comp. reduction',k_save%nibz)
     !
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
     io_err=io_WF(ID)
     !
     do ik=1,k_save%nibz
       !
       ik_save=kpoints_map(1,ik)
       !
       do ib_grp=1,wf_nb_io_groups
         !
         wf_disk=0._SP
         !
         nb1=wf_nb_io*(ib_grp-1)+1
         nb2=wf_nb_io*ib_grp
         !
         nkibz=k_save%nibz
         wf_ncx=wf_ncx_save
         core_io_path=core_io_path_save
         call WF_load(WF,wf_ng,1,(/nb1,nb2/),(/ik_save,ik_save/),space='C',title='',impose_free_and_alloc=.true.)
         !
         do ib=nb1,nb2
           ib_disk=ib-nb1+1
           do i_spin=1,n_spin
             i_wf=WF%index(ib,ik_save,i_spin)
             wf_disk(1,ib_disk,:wf_nc_k_new(ik),i_spin)=  real(WF%c(:wf_nc_k_reduced(ik_save),i_wf))
             wf_disk(2,ib_disk,:wf_nc_k_new(ik),i_spin)= aimag(WF%c(:wf_nc_k_reduced(ik_save),i_wf))
           enddo
         enddo
         !
         call WF_free(WF)
         !
         nkibz=k%nibz
         wf_ncx=wf_ncx_new
         core_io_path=more_io_path
         call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/ik+1,ib_grp/),ID=ID)
         io_err=io_WF(ID,wf=wf_disk)
         !
       enddo
       !
       call live_timing(steps=1)
       !
     enddo
     !
     call live_timing()
     !
   endif
   !
   n_steps=(k%nibz-k_save%nibz)
   if(n_steps>0) call live_timing('WFs rotation',n_steps)
   !
   YAMBO_ALLOC(ic_rot_table,(wf_ncx_new))
   YAMBO_ALLOC(wf_tmp,(wf_ncx_new,n_spin))
   !
   do ik=k_save%nibz+1,k%nibz 
     !      
     ik_save=kpoints_map(1,ik)
     is=kpoints_map(2,ik)
     !
     ic_rot_table=-1
     do ic=1,wf_nc_k_reduced(ik_save)
       ig_rot=g_rot(is,wf_igk_reduced(ic,ik_save))
       l_found=.false.
       do ic_rot=1,wf_nc_k_new(ik)
         ig=wf_igk_new(ic_rot,ik)
         if(ig==ig_rot) then
           l_found=.true.
           exit
         endif
       enddo
       if(.not.l_found) call error("Erro rotating an wf_igk array")
       ic_rot_table(ic)=ic_rot
     enddo
     !
     do ib_grp=1,wf_nb_io_groups
       !
       wf_disk=0._SP
       !
       nb1=wf_nb_io*(ib_grp-1)+1
       nb2=wf_nb_io*ib_grp
       !
       nkibz=k_save%nibz
       wf_ncx=wf_ncx_save
       core_io_path=core_io_path_save
       call WF_load(WF,wf_ng,1,(/nb1,nb2/),(/ik_save,ik_save/),space='C',title='',impose_free_and_alloc=.true.)
       !
       do ib=nb1,nb2
         !
         ib_disk=ib-nb1+1
         !
         wf_tmp=cZERO
         !
         do i_spin=1,n_spin
           !
           i_wf=WF%index(ib,ik_save,i_spin)
           !
           forall(ic=1:wf_nc_k_reduced(ik_save)) wf_tmp(ic_rot_table(ic),i_spin)=WF%c(ic,i_wf)
           !
         enddo
         !
         if(n_spinor==2) then
           forall(ic=1:wf_nc_k_new(ik)) wf_tmp(ic,:)=matmul(spin_sop(:,:,is),wf_tmp(ic,:))
         endif
         if ( S_contains_TR(is) ) wf_tmp=conjg(wf_tmp)
         !
         wf_disk(1,ib_disk,1:wf_ncx_new,:)= real(wf_tmp(:wf_ncx_new,:))
         wf_disk(2,ib_disk,1:wf_ncx_new,:)= aimag(wf_tmp(:wf_ncx_new,:))
         !
       enddo
       !
       call WF_free(WF)
       !
       nkibz=k%nibz
       wf_ncx=wf_ncx_new
       core_io_path=more_io_path
       call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/ik+1,ib_grp/),ID=ID)
       io_err=io_WF(ID,wf=wf_disk)
       !
     enddo
     !
     if(n_steps>0) call live_timing(steps=1)
     !
   enddo
   !
   if(n_steps>0) call live_timing()
   !
   YAMBO_FREE(ic_rot_table)
   YAMBO_FREE(wf_tmp)
   !
 case(2)
   !
   call msg('s',':: WFs reduction...')
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
   io_err=io_WF(ID)
   !
   YAMBO_ALLOC(wf_disk,(2,wf_nb_io,wf_ncx_new,n_spin))
   !
   do ik=1,k%nibz
     !
     ik_save=kpoints_map(1,ik)
     !
     do ib_grp=1,wf_nb_io_groups
       !
       wf_disk=0._SP
       !
       nb1=wf_nb_io*(ib_grp-1)+1
       nb2=wf_nb_io*ib_grp
       !
       nkibz=k_save%nibz
       wf_ncx=wf_ncx_save
       core_io_path=core_io_path_save
       call WF_load(WF,wf_ng,1,(/nb1,nb2/),(/ik_save,ik_save/),space='C',title='',impose_free_and_alloc=.true.)
       !
       do ib=nb1,nb2
         ib_disk=ib-nb1+1
         do i_spin=1,n_spin
           i_wf=WF%index(ib,ik_save,i_spin)
           wf_disk(1,ib_disk,1:wf_ncx_new,i_spin)= real(WF%c(1:wf_ncx_new,i_wf))
           wf_disk(2,ib_disk,1:wf_ncx_new,i_spin)= aimag(WF%c(1:wf_ncx_new,i_wf))
         enddo
       enddo
       !
       call WF_free(WF)
       !
       nkibz=k%nibz
       wf_ncx=wf_ncx_new
       core_io_path=more_io_path
       call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/ik+1,ib_grp/),ID=ID)
       io_err=io_WF(ID,wf=wf_disk)
       !
     enddo
     !
   enddo
   !
   call msg('l','done')
   !
 end select
 ! 
 if(frag_WF) then
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
   io_err=io_WF(ID)
 endif
 !
 core_io_path=core_io_path_save
 !
 call fix_QP_DBs(k,k_save,kpoints_map,action_kind)
 !
#if defined _YPP_SC
 call fix_SC_DBs(k,k_save,kpoints_map,action_kind)
#endif
 !
 ! Finally I save again the new WFs_shells to be used by fix_PPs
 YAMBO_FREE(wf_nc_k)
 YAMBO_FREE(wf_igk)
 YAMBO_ALLOC(wf_nc_k,(k%nibz))
 YAMBO_ALLOC(wf_igk,(wf_ncx,k%nibz))
 !
 wf_nc_k=wf_nc_k_new
 wf_igk =wf_igk_new
 wf_ncx =wf_ncx_new
 wf_ng  =wf_ng_new 
 !
 ! CLEAN
 !=======
 call E_reset(E_new)
 YAMBO_FREE(wf_disk)
 !
end subroutine
!
subroutine map_energies(E,E_new,k,kpoints_map)
 !
 use electrons,           ONLY:levels,E_reset,n_sp_pol
 use R_lattice,           ONLY:bz_samp
 !
#include<memory.h>
 !
 type(levels), intent(in)     :: E
 type(levels), intent(out)    :: E_new
 type(bz_samp), intent(in)    :: k
 integer,       intent(in)    :: kpoints_map(2,k%nibz)
 !
 ! Work Space
 !
 integer :: ik
 !
 call E_reset(E_new)
 E_new%nb=E%nb
 E_new%nk=k%nibz
 YAMBO_ALLOC(E_new%E,(E%nb,k%nibz,n_sp_pol))
 !
 do ik=1,k%nibz
   E_new%E(:,ik,:)=E%E(:,kpoints_map(1,ik),:)
 enddo
 !
end subroutine map_energies

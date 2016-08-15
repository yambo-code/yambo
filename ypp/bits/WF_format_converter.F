!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine WF_format_converter(E,Xk)
 !
 use pars,                ONLY:SP,lchlen
 use R_lattice,           ONLY:bz_samp
 use memory_m,            ONLY:mem_est
 use LIVE_t,              ONLY:live_timing
 use IO_m,                ONLY:mk_dir,rm_file,cp_file
 use com,                 ONLY:msg,core_io_path,more_io_path
 use electrons,           ONLY:levels,n_spin,n_sp_pol,n_spinor
 use wave_func,           ONLY:wf_ncx,io_WF,io_WF_old,wf_nb_io_groups,wf_nb_io
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,OP_RD_CL
 !
 implicit none
 !
 type(levels),intent(in)     :: E
 type(bz_samp),intent(inout) :: Xk
 !
 ! Work Space
 !
 character(lchlen)  :: core_io_path_save
 integer  :: ik,i_sp_pol,i_spinor,i_spin,ib_io,ib_grp,ifrag
 real(SP),    allocatable :: wf_disk_old(:,:,:,:)
 complex(SP), allocatable :: wf_disk_new(:,:,:)
 !
 integer           :: i_err,io_err,ID_old,ID_new
 !
 if (core_io_path==more_io_path) then
   more_io_path=trim(core_io_path)//"/FixSAVE"
   call msg('s','Setting I/O path to "FixSAVE"')
 endif
 !
 core_io_path_save=core_io_path
 !
 call mk_dir(more_io_path)
 call mk_dir(trim(more_io_path)//"/SAVE")
 call cp_file(trim(core_io_path_save)//"/SAVE/ns.db1",trim(more_io_path)//"/SAVE",i_err)
 call cp_file(trim(core_io_path_save)//"/SAVE/ns.kb*",trim(more_io_path)//"/SAVE",i_err)
 call rm_file((trim(more_io_path))//"/SAVE/ndb.gops")
 call rm_file((trim(more_io_path))//"/SAVE/ndb.kindx")
 !
 call section('=',"Converting wave--functions to new format.")
 !==============================================================
 !
 allocate(wf_disk_new(wf_ncx,n_spinor,wf_nb_io))
 call mem_est("wf_disk_new",(/size(wf_disk_new)/),(/2*SP/))
 !
 allocate(wf_disk_old(2,wf_nb_io,wf_ncx,n_spin))
 call mem_est("wf_disk_old",(/size(wf_disk_old)/),(/SP/))
 !
 call live_timing('WFs conversion',E%nk*wf_nb_io_groups)
 !
 do ik=1,E%nk
   !
   do ib_grp=1,wf_nb_io_groups
     !
     core_io_path=core_io_path_save
     !
     if (ik==1.and.ib_grp==1) call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,1/),ID=ID_old)
     if (ik> 1.or. ib_grp> 1) call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/ik+1,ib_grp/),ID=ID_old)
     io_err=io_WF_old(ID_old,wf_old=wf_disk_old)
     !
     do i_sp_pol=1,n_sp_pol
       !
       do i_spinor=1,n_spinor
         i_spin=i_sp_pol+i_spinor-1
         do ib_io=1,wf_nb_io
           wf_disk_new(:,i_spinor,ib_io)=cmplx(wf_disk_old(1,ib_io,:,i_spin),wf_disk_old(2,ib_io,:,i_spin),SP)
         enddo
       enddo
       !
       ifrag=ik+(i_sp_pol-1)*E%nk
       !
       core_io_path=more_io_path
       !
       if (ifrag==1.and.ib_grp==1) call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2,1/),ID=ID_new)
       if (ifrag> 1.or. ib_grp> 1) call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ifrag+1,ib_grp/),ID=ID_new)
       io_err=io_WF(ID_new,wf_disk_new)
       !
     enddo
     !
     call live_timing(steps=1)
     !
   enddo
 enddo
 !
 call live_timing()
 !
 core_io_path=core_io_path_save
 !
 deallocate(wf_disk_new,wf_disk_old)
 call mem_est("wf_disk_new wf_disk_old")
 !
end subroutine

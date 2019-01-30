!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine WF_format_converter(E,Xk,q)
 !
 use pars,                ONLY:SP,lchlen
 use R_lattice,           ONLY:bz_samp
 use pseudo,              ONLY:PP_free,PP_free_old,PP_alloc_abinit, &
&                              pp_kbs,pp_kbs_old,pp_kb,pp_kb_old,pp_kbd,pp_kbd_old
 use LIVE_t,              ONLY:live_timing
 use D_lattice,           ONLY:n_atoms_species_max
 use IO_m,                ONLY:mk_dir,rm_file,cp_file
 use com,                 ONLY:msg,core_io_path,more_io_path
 use electrons,           ONLY:levels,n_spin,n_sp_pol,n_spinor
 use wave_func,           ONLY:wf_ncx,io_WF,io_WF_old,wf_nb_io_groups,wf_nb_io,wf_igk
 use IO_m,                ONLY:io_control,NONE,DUMP,OP_WR_CL,OP_APP_CL,OP_RD_CL
 !
#include<memory.h>
 !
 type(levels),intent(in)     :: E
 type(bz_samp),intent(inout) :: Xk
 type(bz_samp),intent(inout) :: q
 !
 ! Work Space
 !
 character(lchlen)  :: core_io_path_save,section_name
 integer  :: ik,i_sp_pol,i_spinor,i_spin,ib_io,ib_grp,ifrag
 real(SP),    allocatable :: wf_disk_old(:,:,:,:)
 complex(SP), allocatable :: wf_disk_new(:,:,:)
 !
 integer           :: i_err,io_err,io_KB_abinit_err,ID_old,ID_new
 integer, external :: io_KB_abinit,io_KB_abinit_old
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
 call rm_file((trim(more_io_path))//"/SAVE/ndb.gops")
 call rm_file((trim(more_io_path))//"/SAVE/ndb.kindx")
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID_old)
 io_KB_abinit_err=io_KB_abinit_old(ID_old)
 !
 if (io_KB_abinit_err==0) then
   !
   section_name="Converting WFs and abinit KB to new format."
   !
 else
   !
   call cp_file(trim(core_io_path_save)//"/SAVE/ns.kb*",trim(more_io_path)//"/SAVE",i_err)
   section_name="Converting WFs to new format."
   !
 endif
 !
 call section('=',trim(section_name))
 !====================================
 !
 YAMBO_ALLOC(wf_disk_new,(wf_ncx,n_spinor,wf_nb_io))
 !
 YAMBO_ALLOC(wf_disk_old,(2,wf_nb_io,wf_ncx,n_spin))
 !
 core_io_path=core_io_path_save
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID_old)
 io_err=io_WF_old(ID_old)
 !
 if (n_atoms_species_max>0.and.io_KB_abinit_err==0) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID_old)
   io_KB_abinit_err=io_KB_abinit_old(ID_old)
 endif
 !
 core_io_path=more_io_path
 !
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID_new)
 io_err=io_WF(ID_new)
 !
 if (n_atoms_species_max>0.and.io_KB_abinit_err==0) then
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID_new)
   io_KB_abinit_err=io_KB_abinit(ID_new)
 endif
 !
 call live_timing('Conversion',E%nk*wf_nb_io_groups)
 !
 do ik=1,E%nk
   !
   do ib_grp=1,wf_nb_io_groups
     !
     core_io_path=core_io_path_save
     !
     if (n_atoms_species_max>0.and.ib_grp==1.and.io_KB_abinit_err==0) then
       call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/ik+1/),ID=ID_old)
       io_KB_abinit_err=io_KB_abinit_old(ID_old)
     endif
     !
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/ik+1,ib_grp/),ID=ID_old)
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
       if (n_atoms_species_max>0.and.ib_grp==1.and.io_KB_abinit_err==0) then
         !
         if(ifrag==1) then
           call PP_alloc_abinit()
           pp_kbs=pp_kbs_old
         endif
         !
         pp_kb(:,:,:,i_sp_pol) =pp_kb_old(wf_igk(:,ik),:,:,i_sp_pol) 
         pp_kbd(:,:,:,i_sp_pol)=pp_kbd_old(wf_igk(:,ik),:,:,i_sp_pol)
         !
       endif
       !
       core_io_path=more_io_path
       !
       if (n_atoms_species_max>0.and.ib_grp==1.and.io_KB_abinit_err==0) then
         call io_control(ACTION=OP_APP_CL,COM=NONE,SEC=(/ifrag+1/),ID=ID_new)
         io_KB_abinit_err=io_KB_abinit(ID_new)
       endif
       !
       call io_control(ACTION=OP_APP_CL,COM=NONE,SEC=(/ifrag+1,ib_grp/),ID=ID_new)
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
 YAMBO_FREE(wf_disk_new)
 YAMBO_FREE(wf_disk_old)
 !
 if (io_KB_abinit_err==0) then
   call PP_free()
   call PP_free_old()
 endif
 !
#if defined _YPP_ELPH
 call ELPH_format_converter(E,Xk,q)
#endif
 !
end subroutine

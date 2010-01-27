!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team
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
subroutine Fragments_Restart(ID,current_fragment,fragments_todo)
 !
 use pars,      ONLY:schlen,lchlen,IP
 use drivers,   ONLY:list_dbs
 use IO_m,      ONLY:write_is_on,io_restart_point,io_file,code_revision,code_version,&
&                    DUMP,io_com,NONE,io_mode,VERIFY,rm_file,mk_dir,io_extension
 use stderr,    ONLY:intc
 use com,       ONLY:get_name,file_exists,more_io_path
 use timing,    ONLY:date_and_time_string
 use com,       ONLY:msg
 use memory_m,  ONLY:mem_est
 use fragments, ONLY:Parallel_Fragments,fragment_exists,fragment_status
 implicit none
 integer          ::ID
 integer, optional::current_fragment,fragments_todo
 ! 
 ! Work Space
 !
 integer          ::i1
 logical          ::allocated_fragment_status
 character(lchlen)::restart_file,restart_folder,local_string
 !
 io_restart_point(ID)=0
 !
 if (.not.present(current_fragment).and..not.present(fragments_todo).and.io_mode(ID)/=VERIFY.and.io_mode(ID)/=DUMP) return
 !
 if (present(current_fragment)) then
   !
   if (.not.write_is_on(ID)) return
   !
   ! If the database fragmented are parallelized I do not need a RESTART folder to restart the
   ! calculation. This is done, instead, by checking the existence of each fragment file.
   !
   if (Parallel_Fragments) return
   !
   restart_file  =get_name(desc=io_extension(ID),type=4,CORE_IO=.false.,MORE_IO=.true.,COM_IO=.FALSE.,NETCDF=.false.)
   !
   restart_folder=trim(more_io_path)//"/RESTART"
   !
   if (current_fragment==fragments_todo) then
     call rm_file(restart_file)
     call rm_file(restart_folder)
     return
   endif
   !
   if (.not.file_exists(trim(restart_folder))) call mk_dir(restart_folder)
   !
   open(unit=81,file=trim(restart_file))
   write (81,'(/2x,a,3(i2.2,a),a,i3.3)') &
         'Restart file for YAMBO ',code_version(1),&
         '.',code_version(2),'.',code_version(3),' -- ',&
         'revision ',code_revision
   local_string=date_and_time_string()
   write (81,'(/2x,a)') trim(local_string)
   write (81,'(/2x,2(a,i6,1x))') 'Section Completed ',current_fragment,'. To reach ',fragments_todo
   close(81)
   !
 else
   !
   local_string=" "
   !
   ! [1] Use fragments
   !
   ! If the database is fragmented I find the restart point by looking at the
   ! fragments databases
   !
   if (present ( fragments_todo ) ) then
     if ( fragments_todo > 0 ) then
       !
       allocated_fragment_status=allocated(fragment_status)
       if (.not.allocated_fragment_status) allocate(fragment_status(fragments_todo))
       fragment_status=-1
       !
       do i1=1,fragments_todo
         if (fragment_exists(ID,i_fragment=i1)) Fragment_status(i1)=1
       enddo
       !
       if (.not.all(Fragment_status==1).and..not.all(Fragment_status==-1)) then
         do i1=1,fragments_todo
           if (io_restart_point(ID)==0.and.Fragment_status(i1)==-1) io_restart_point(ID)=i1
         enddo
       endif
       if (io_restart_point(ID)==fragments_todo) io_restart_point(ID)=0
       !
       if (.not.allocated_fragment_status) deallocate(fragment_status)
       !
       if (io_restart_point(ID)/=0) local_string='[RESTARTer] Section(s) missing for '//trim(io_file(ID))
     endif
     !
   endif
   !
   ! [2] RESTART folder
   !
   !
   restart_file=get_name(desc=io_extension(ID),type=4,CORE_IO=.false.,MORE_IO=.true.,COM_IO=.FALSE.,NETCDF=.false.)
   if (file_exists(trim(restart_file))) then
     !
     open(unit=81,file=trim(restart_file))
     read (81,'(/////2x,a,i6)') local_string(:18),io_restart_point(ID)
     close(81)
     io_restart_point(ID)=io_restart_point(ID)+1
     !
     local_string='[RESTARTer] Section(s) completed for '//trim(io_file(ID))//' :'//trim(intc(io_restart_point(ID)-1))
     !
   endif
   !
   ! [3] Message
   !
   if (len_trim(local_string)>0) then
     if (list_dbs)                           call msg('s',  trim(local_string))
     if (.not.list_dbs.and.io_com(ID)/=NONE) call msg('rns',trim(local_string))
   endif
   !
 endif
 !
end subroutine

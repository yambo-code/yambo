!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
function get_name(desc,subfolder,type,CORE_IO,MORE_IO,COM_IO,ALT_JOB_STRING,ALT_JOB_DIR)
 !
 use pars,     ONLY:lchlen
 use com,      ONLY:com_path,more_io_path,core_io_path,jobdir,jobstr
 !
 ! -4 ----------------  -> JOBSTR/JOBFOLDER/desc (no NETCDF support)
 ! -3 ----------------  -> desc 
 ! -2 ----------------  -> desc (no NETCDF support)
 ! -1 ----------------  -> subfolder/ndesc
 !  0 Stable s.         -> CORE_IO_PATH/SAVE/subfolder/ns.desc
 !  1 Stable db.        -> CORE_IO_PATH/SAVE/subfolder/ndb.desc
 !  2 Job dependent db. -> PATH/JOBSTR/subfolder/ndb.desc 
 !  3 Job dependent o.  -> COM_PATH/o.desc-JOBSTR
 !  4 ----------------  -> MORE_IO_PATH/RESTART/ndb.desc_FOLDER
 !                                                  |
 !                                                  JOBSTR, or  SAVE if no JOBSTR 
 ! if (CORE_IO) PATH=core_io_path
 ! if (MORE_IO) PATH=more_io_path
 ! if ( COM_IO) PATH=com_path
 !
 ! Update of 27/12/2012. Added second jobstr to be used when the database of type 2 is read.
 !           11/04/2018. Added support for jobstr and jobdir
 !
 implicit none
 !
 character(lchlen)      ::get_name
 !
 character(*)           ::desc
 integer                ::type
 logical                ::CORE_IO,MORE_IO,COM_IO
 character(*), optional ::subfolder
 character(*), optional ::ALT_JOB_STRING
 character(*), optional ::ALT_JOB_DIR
 ! 
 ! Work Space
 !
 character(lchlen)::ch(2)
 character(lchlen)::folder,jobstr_,jobdir_
 !
 get_name=" "
 jobstr_ =jobstr
 jobdir_ =jobdir
 if (present(ALT_JOB_STRING)) jobstr_=ALT_JOB_STRING
 if (present(ALT_JOB_DIR)   ) jobdir_=ALT_JOB_DIR
 folder  ="SAVE"
 if (.not.CORE_IO.and..not.MORE_IO.and..not.COM_IO) return
 !
 ch(1)=core_io_path
 !
 select case(type)
   case(-2,-3)
     get_name="./"//trim(desc)
     return
   case(-1)
     ch(2)=desc
     folder="."
     if (present(subfolder)) folder=subfolder
     !
     ! As I want the path to be relative to the working directory I need to set ch(1)=".". In case 
     ! the path shpuld be absolute ch(1) must be " " 
     !
     ch(1)='.'
     !
   case(0)
     ch(2)="s."//trim(desc)
   case(1)
     ch(2)="db."//trim(desc)
   case(-4,2,4)
     ch(2)="db."//trim(desc)
     if (len_trim(jobstr_)>0) then
       if (len_trim(jobdir_)> 0) folder=trim(jobdir_)//"/"//trim(jobstr_)
       if (len_trim(jobdir_)==0) folder=trim(jobstr_)
     endif
     if (type==4) then
       ch(2)="db."//trim(desc)//"_"//trim(folder)
       folder="RESTART"
       !
       ! The RESTART folder is always in the outpath
       !
       ch(1)=more_io_path
       !
     else if (type==2) then
       !
       if (CORE_IO) ch(1)=core_io_path
       if (MORE_IO) ch(1)=more_io_path
       if ( COM_IO) ch(1)=com_path
       !
       !
     else if (type<0) then
       !
       folder="."
       if (len_trim(jobstr_)>0) then
         if (len_trim(jobdir_)> 0) folder=trim(jobdir_)//"/"//trim(jobstr_)
         if (len_trim(jobdir_)==0) folder=trim(jobstr_)
       endif
       get_name=trim(folder)//"/"//trim(desc)
       return
       !
     endif
   case(3)
     ch(2)="o."//trim(desc)
     folder=""
     if (len_trim(jobstr_)>0) ch(2)="o-"//trim(jobstr_)//"."//trim(desc)
     ch(1)=com_path
 end select 
 !
 if (len_trim(folder)>0) ch(1)=trim(ch(1))//"/"//trim(folder)
 if (present(subfolder).and.type>=0) ch(1)=trim(ch(1))//"/"//trim(subfolder)
 !
 get_name=trim(ch(1))//"/"//trim(ch(2))
 if (type/=3) get_name=trim(ch(1))//"/n"//trim(ch(2))
 !
end function get_name
!
!
function all_locations(desc,subfolder)
 !
 use pars,            ONLY:lchlen,num_of_alternative_locations
 use IO_m,            ONLY:get_name
 use com,             ONLY:n_alt_jobstr,alt_jobdir,alt_jobstr,n_max_jobstr
 !
 implicit none
 !
 character(lchlen), dimension(num_of_alternative_locations) :: all_locations
 !
 character(*)           ::desc
 character(*), optional ::subfolder
 !
 ! WS
 integer :: i_jobstr
 !
 ! Possible Paths
 !
 ! 1  MORE_IO=.TRUE.  CORE_IO=.FALSE. COM_IO=.FALSE. type=2 (with JOBSTR)
 ! 2  MORE_IO=.TRUE.  CORE_IO=.FALSE. COM_IO=.FALSE. type=1 (no   JOBSTR)
 ! 3  MORE_IO=.FALSE. CORE_IO=.TRUE.  COM_IO=.FALSE. type=2 (with JOBSTR)
 ! 4  MORE_IO=.FALSE. CORE_IO=.TRUE.  COM_IO=.FALSE. type=1 (no   JOBSTR)
 ! .
 ! .
 ! .
 ! 3+2*I MORE_IO=.TRUE.  CORE_IO=.FALSE. COM_IO=.FALSE. type=2 (with ALT_JOBSTR(I))
 ! 4+2*I MORE_IO=.FALSE. CORE_IO=.TRUE.  COM_IO=.FALSE. type=2 (with ALT_JOBSTR(I))
 !
 all_locations(1)=get_name(desc,subfolder,2,CORE_IO=.FALSE.,MORE_IO=.TRUE.,COM_IO=.FALSE.)
 all_locations(2)=get_name(desc,subfolder,1,CORE_IO=.FALSE.,MORE_IO=.TRUE.,COM_IO=.FALSE.)
 all_locations(3)=get_name(desc,subfolder,2,CORE_IO=.TRUE.,MORE_IO=.FALSE.,COM_IO=.FALSE.)
 all_locations(4)=get_name(desc,subfolder,1,CORE_IO=.TRUE.,MORE_IO=.FALSE.,COM_IO=.FALSE.)
 !
 ! Added on 27/12/2012 using second_jobstr (Andrea)
 ! Added on 27/6/2013 using third_jobstr (Andrea)
 ! 30/8/2013 Merged and added general number of jobstrs (Andrea)
 !
 do i_jobstr=1,n_max_jobstr
   all_locations(3+2*i_jobstr:4+2*i_jobstr) = ' '
 enddo
 !
 do i_jobstr=1,n_alt_jobstr
   all_locations(3+2*i_jobstr) =get_name(desc,subfolder,2,CORE_IO=.FALSE.,&
&                                            MORE_IO=.TRUE.,COM_IO=.FALSE.,&
&                                            ALT_JOB_STRING=trim(alt_jobstr(i_jobstr)),&
&                                            ALT_JOB_DIR   =trim(alt_jobdir(i_jobstr)))
   all_locations(4+2*i_jobstr)=get_name(desc,subfolder,2,CORE_IO=.TRUE.,&
&                                           MORE_IO=.FALSE.,COM_IO=.FALSE.,&
&                                           ALT_JOB_STRING=trim(alt_jobstr(i_jobstr)),&
&                                           ALT_JOB_DIR   =trim(alt_jobdir(i_jobstr)))
 enddo
 !
end function all_locations

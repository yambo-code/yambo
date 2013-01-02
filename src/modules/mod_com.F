!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
module com
 !
 use pars,       ONLY:lchlen,SP,schlen
 use stderr,     ONLY:gen_fmt
 implicit none
 !
 ! PATHS & JOB_str
 !
 character(lchlen)  :: core_io_path
 character(lchlen)  :: more_io_path
 character(lchlen)  :: com_path
 character(lchlen)  :: jobstr
 character(lchlen)  :: repfile
 !
 character(3)      :: terminator(4)
 !
 ! Sectioning
 !
 integer           :: depth,isec(5)
 character(1)      :: previous_secmode
 character(schlen) :: secnm
 ! 
 ! Output Files/Report File
 !
 integer,parameter  :: nofs=200
 integer,parameter  :: max_open_ofs=200
 character(lchlen)  :: of(nofs)
 character(lchlen)  :: opened_of(max_open_ofs)
 integer            :: of_unit(max_open_ofs)
 logical            :: write_to_of
 logical            :: write_to_report
 !
 interface msg
   module procedure na0msg,na1msg,namsg,ni1msg,nimsg,nr1msg,nrmsg,nl0msg
 end interface
 !
 contains
   !
   character(lchlen) function get_name(desc,subfolder,type,CORE_IO,MORE_IO,COM_IO,NETCDF)
     !
     ! -2 ----------------  -> desc (NETCDF deduced from presence of "ndb" in desc)
     ! -1 ----------------  -> subfolder/(n)desc
     !  0 Stable s.         -> CORE_IO_PATH/SAVE/subfolder/(n)s.desc
     !  1 Stable db.        -> CORE_IO_PATH/SAVE/subfolder/(n)db.desc
     !  2 Job dependent db. -> PATH/JOBSTR/subfolder/(n)db.desc
     !  3 Job dependent o.  -> COM_PATH/o.desc-JOBSTR
     !  4 ----------------  -> MORE_IO_PATH/RESTART/(n)db.desc_FOLDER
     !                                                    |
     ! if (CORE_IO) PATH=core_io_path
     ! if (MORE_IO) PATH=more_io_path
     ! if ( COM_IO) PATH=com_path
     !                                                    JOBSTR, or  SAVE if no JOBSTR 
     character(*)           ::desc
     integer                ::type
     logical                ::CORE_IO,MORE_IO,NETCDF,COM_IO
     character(*), optional ::subfolder
     ! 
     ! Work Space
     !
     character(schlen)::ch(2)
     character(lchlen)::folder
     !
     get_name=""
     folder="SAVE"
     if (.not.CORE_IO.and..not.MORE_IO.and..not.COM_IO) return
     !
     ch(1)=core_io_path
     !
     select case(type)
       case(-2)
         get_name="./"//trim(desc)
         NETCDF=index(desc,'ndb')>0
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
       case(2,4)
         ch(2)="db."//trim(desc)
         if (len_trim(jobstr)>0) folder=trim(jobstr)
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
         endif
       case(3)
         ch(2)="o."//trim(desc)
         folder=""
         if (len_trim(jobstr)>0) ch(2)="o-"//trim(jobstr)//"."//trim(desc)
         ch(1)=com_path
     end select 
     !
     if (len_trim(folder)>0) ch(1)=trim(ch(1))//"/"//trim(folder)
     if (present(subfolder).and.type>=0) ch(1)=trim(ch(1))//"/"//trim(subfolder)
     !
     if (NETCDF) then
       get_name=trim(ch(1))//"/n"//trim(ch(2))
     else
       get_name=trim(ch(1))//"/"//trim(ch(2))
     endif
     !
   end function
   !
   function all_locations(desc,subfolder,NETCDF_DB)
     !
     character(*)           ::desc
     character(*), optional ::subfolder
     logical                ::NETCDF_DB
     character(lchlen)      ::all_locations(8)
     !
     ! Possible Paths
     !
     ! 1/2. MORE_IO=.TRUE.  CORE_IO=.FALSE. COM_IO=.FALSE. type=2 (with JOBSTR)
     ! 3/4. MORE_IO=.TRUE.  CORE_IO=.FALSE. COM_IO=.FALSE. type=1 (no   JOBSTR)
     ! 5/6. MORE_IO=.FALSE. CORE_IO=.TRUE. COM_IO=.FALSE.  type=2 (with JOBSTR)
     ! 7/8. MORE_IO=.FALSE. CORE_IO=.TRUE. COM_IO=.FALSE.  type=1 (no   JOBSTR)
     !
     all_locations(1)=get_name(desc,subfolder,2,CORE_IO=.FALSE.,&
&                              MORE_IO=.TRUE.,COM_IO=.FALSE.,NETCDF=.not.NETCDF_DB)
     all_locations(2)=get_name(desc,subfolder,2,CORE_IO=.FALSE.,&
&                              MORE_IO=.TRUE.,COM_IO=.FALSE.,NETCDF=NETCDF_DB)
     all_locations(3)=get_name(desc,subfolder,1,CORE_IO=.FALSE.,&
&                              MORE_IO=.TRUE.,COM_IO=.FALSE.,NETCDF=.not.NETCDF_DB)
     all_locations(4)=get_name(desc,subfolder,1,CORE_IO=.FALSE.,&
&                              MORE_IO=.TRUE.,COM_IO=.FALSE.,NETCDF=NETCDF_DB)
     all_locations(5)=get_name(desc,subfolder,2,CORE_IO=.TRUE.,&
&                              MORE_IO=.FALSE.,COM_IO=.FALSE.,NETCDF=.not.NETCDF_DB)
     all_locations(6)=get_name(desc,subfolder,2,CORE_IO=.TRUE.,&
&                              MORE_IO=.FALSE.,COM_IO=.FALSE.,NETCDF=NETCDF_DB)
     all_locations(7)=get_name(desc,subfolder,1,CORE_IO=.TRUE.,&
&                              MORE_IO=.FALSE.,COM_IO=.FALSE.,NETCDF=.not.NETCDF_DB)
     all_locations(8)=get_name(desc,subfolder,1,CORE_IO=.TRUE.,&
&                              MORE_IO=.FALSE.,COM_IO=.FALSE.,NETCDF=NETCDF_DB)
   end function
   !
   subroutine of_open_close(of_name,mode)
     !
     character(*)           :: of_name
     character(*),optional  :: mode
     ! 
     ! Work Space
     !
     integer          :: i2,i3,file_index
     character(lchlen):: local_file_name
     !
     of_name=trim(of_name)
     !
     if (len_trim(of_name)==0) return
     !
     if (present(mode)) then
       !  
       local_file_name=of_name
       !
       ! Flush output
       !
       if (index(mode,'f')/=0) then
         file_index=-1
         do i2=1,max_open_ofs
            if (trim(opened_of(i2))==local_file_name) file_index=i2
         enddo
         if(file_index==-1) return
         call flush(abs(of_unit(file_index)))
       endif
       !
       if ( (index(mode,'o')==0.and.index(mode,'O')==0) .or. .not.write_to_of) return
       !
       ! Here I open the unit using file_name to define the name
       !
       if (index(mode,'a')==0.and.index(mode,'O')==0) then
         local_file_name=get_name(desc=of_name,type=3,CORE_IO=.false.,&
&                                 MORE_IO=.false.,COM_IO=.true.,NETCDF=.false.)
         call rename_file(local_file_name)
       endif
       !
       ! Check first if already open
       !
       do i2=1,max_open_ofs
         if (trim(opened_of(i2))==local_file_name) return
       enddo
       !
       ! Find the first free unit
       !
       do i2=1,max_open_ofs
         if (of_unit(i2)==0) then
           file_index=i2
           of_unit(file_index)=20+i2
           exit
         endif
       enddo
       of_name=local_file_name
       opened_of(file_index)=local_file_name
       !
       if (index(mode,'a')==0) open(of_unit(file_index),file=trim(opened_of(file_index)))
       if (index(mode,'a')/=0) open(of_unit(file_index),file=trim(opened_of(file_index)),&
&                                   position='append')
       if (index(mode,"t")>0) call write_the_logo(of_unit(file_index),'#')
    
       of_unit(file_index)=-of_unit(file_index)
       !
     else
       !
       do i2=1,max_open_ofs
         if (index(opened_of(i2),of_name)/=0) then
           close(iabs(of_unit(i2)))
           of_unit(i2)=0
           if (.not.any(of==of_name)) then
             do i3=1,nofs
               if (len_trim(of(i3))==0) then
                 of(i3)=opened_of(i2)
                 exit
               endif
             enddo
           endif
           opened_of(i2)=' '
           exit
         endif
       enddo
     endif
     !
   end subroutine
   !
   subroutine write_the_logo(unit_,comment_)
     use timing,       ONLY:live_time_msg
     use par_proc_m,   ONLY:master_cpu
     use LOGO,         ONLY:pickup_a_logo,logo_line,n_logo_lines
     integer     ::unit_
     character(1)::comment_
     character(schlen)::space_,ch ! Work Space
     integer          ::i1        !
     !
     call pickup_a_logo(unit_)
     !
     if (.not.master_cpu.or.unit_<0) return
     !
     if (trim(comment_)=='')  space_='(t5,3a)'
     if (trim(comment_)=='#') then
       space_='(t1,3a)'
       do i1=1,n_logo_lines
          ch='# '//logo_line(i1)
          logo_line(i1)=trim(ch)
       enddo
     endif
     !
     do i1=1,n_logo_lines
       if (unit_/=6) write (unit_,trim(space_)) logo_line(i1)
       if (unit_==6.and.i1>=n_logo_lines-1) cycle
       if (unit_==6) call live_time_msg("n",logo_line(i1),"","%s",NO_CPUT=.true.)
     enddo
     if (unit_/=6.and.trim(comment_)=='')  write (unit_,'(a)') ' '
     if (unit_/=6.and.trim(comment_)=='#')  write (unit_,'(a)') '#'
     !
   end subroutine
   !
   logical function OF_exists(name)
     character(*) ::name
     integer      ::i_f
     OF_exists=.false. 
     do i_f=1,max_open_ofs
       if (index(opened_of(i_f),name)/=0) OF_exists=.TRUE.
     enddo
   end function
   !
   logical function file_exists(name)
     character(*) name
     file_exists=.false.
     if (len_trim(name)==0) return
     inquire(file=name,exist=file_exists)
   end function
   !
   subroutine rename_file(name)
     character(lchlen):: name
     character(lchlen):: ch 
     integer          :: i1
     if (.not.file_exists(name)) return
     i1=0
     ch=name
     do while (file_exists(ch))
       i1=i1+1
       write (ch,'(2a,i2.2)') trim(name),'_',i1
     enddo
     name=ch
   end subroutine
   !
   subroutine warning(mesg)
     use timing,     ONLY:live_time_msg
     use stderr,     ONLY:string_pack
     character(*) :: mesg
     call msg('nr',string_pack('[WARNING] ',trim(mesg)))
     call live_time_msg("n",string_pack('[WARNING] ',trim(mesg)),"","%s")
   end subroutine
   !
   subroutine error(mesg)
     use timing,     ONLY:live_time_msg,live_timing_is_on
     use stderr,     ONLY:string_pack
     use par_proc_m, ONLY:p_finalize
     character(*)    :: mesg
     !
     if (file_exists(repfile) ) call report_file_fix
     !
     live_timing_is_on=.FALSE.
     !
     call msg('nr',string_pack('[ERROR] STOP signal received while in :',trim(secnm)))
     call live_time_msg("nn",&
&         string_pack('[ERROR] STOP signal received while in :',trim(secnm)),"","%s")
     call msg('nr',string_pack('[ERROR] ',trim(mesg)))
     call live_time_msg("n",string_pack('[ERROR] ',trim(mesg)),"","%s")
     call live_time_msg("n","","","%s")
     !
     call p_finalize
     !
     stop
   end subroutine
   !
   subroutine report_file_fix
     if (.not.write_to_report) return
     close(11)
     open(unit=11,file=trim(repfile),position='append')
   end subroutine
   !
   subroutine msg_deliver(formatted_msg)
     character(*) :: formatted_msg
     integer      :: i_unit
     do i_unit=1,max_open_ofs
       if (of_unit(i_unit)<=0) cycle
       write (of_unit(i_unit),'(a)') formatted_msg
       of_unit(i_unit)=-of_unit(i_unit)
     enddo
    end subroutine
   !
   !#######################
   ! INTERFACE STARTS HERE
   !#######################
   !
   subroutine na0msg(how,mesg)
     character(*)     :: mesg
     character(*)     :: how
     character        :: crap(1)
     integer          :: INDENT
     crap(1)=" "
     INDENT=-1
     if(len_trim(mesg)==0) then
       call namsg(how," ",crap,INDENT=INDENT)
       return
     endif
     if (mesg(1:1)=="#") INDENT=0
     call namsg(how,trim(mesg),crap,INDENT=INDENT)
   end subroutine
   !
   subroutine nl0msg(how,mesg,val,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     logical          :: val
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     if (val) call na1msg(how,mesg,"yes",USE_TABS=USE_TABS,INDENT=INDENT)
     if (.not.val) call na1msg(how,mesg,"no",USE_TABS=USE_TABS,INDENT=INDENT)
   end subroutine
   !
   subroutine na1msg(how,mesg,val,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     character(*)     :: val
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     call namsg(how,mesg,(/val/),USE_TABS=USE_TABS,INDENT=INDENT)
   end subroutine
   !
   subroutine ni1msg(how,mesg,val,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     integer          :: val
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     call nimsg(how,mesg,(/val/),USE_TABS=USE_TABS,INDENT=INDENT)
   end subroutine
   !
   subroutine nr1msg(how,mesg,val,USE_TABS,INDENT)
     character(*)     :: mesg
     character(*)     :: how
     real(SP)         :: val
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     call nrmsg(how,mesg,(/val/),USE_TABS,INDENT)
   end subroutine
   !
   subroutine namsg(how,mesg,val,USE_TABS,INDENT)
     use stderr,   ONLY: c_print,write_to_log,tty_size
     use timing,   ONLY: live_time_msg
     character(*)     :: mesg
     character(*)     :: how
     character(*)     :: val(:)
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     ! 
     ! Work Space
     !
     integer          :: i1,i2,l_indent 
     character(lchlen):: msgfmt,lch
     logical          :: l_fmt
     !
     if (len_trim(how)==0) return
     call msg_manager(how)
     l_fmt=.false.
     l_indent=-1
     if (present(USE_TABS)) l_fmt=.true.
     if (present(INDENT)) l_indent=INDENT
     msgfmt=composed_fmt('a',size(val),l_fmt,l_indent)
     do i1=1,max_open_ofs
       if (of_unit(i1)<=0) cycle
       write (of_unit(i1),trim(msgfmt)) mesg,(trim(val(i2)),i2=1,size(val))
       of_unit(i1)=-of_unit(i1)
     enddo
     if (index(how,'o')>0.or.index(how,'O')>0) return
     if (.not.write_to_log.or.(index(how,'s')==0.and.index(how,'l')==0)) return
     msgfmt=composed_fmt('a',-size(val),.false.,-1)
     write (lch,trim(msgfmt)) mesg,(trim(val(i2)),i2=1,size(val))
     if (index(how,'l')/=0.and.tty_size>0) &
&       call c_print(terminator(3),lch,terminator(4),"%s")
     if (index(how,'s')/=0) call live_time_msg("n",trim(lch),"","%s")
   end subroutine
   !
   subroutine nimsg(how,mesg,val,USE_TABS,INDENT)
     use stderr,   ONLY: c_print,write_to_log,tty_size
     use timing,   ONLY: live_time_msg
     character(*)     :: mesg
     character(*)     :: how
     integer          :: val(:)
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     ! 
     ! Work Space
     !
     integer :: i1,l_indent
     logical :: l_fmt
     character(lchlen)::lch,msgfmt
     !
     if (len_trim(how)==0) return
     call msg_manager(how)
     l_fmt=.false.
     l_indent=-1
     if (present(USE_TABS)) l_fmt=.true.
     if (present(INDENT)) l_indent=INDENT
     msgfmt=composed_fmt(trim(gen_fmt(val)),size(val),l_fmt,l_indent)
     do i1=1,max_open_ofs
       if (of_unit(i1)<=0) cycle
       write (of_unit(i1),trim(msgfmt)) mesg,val
       of_unit(i1)=-of_unit(i1)
     enddo
     if (index(how,'o')>0.or.index(how,'O')>0) return
     if (.not.write_to_log.or.(index(how,'s')==0.and.index(how,'l')==0)) return
     msgfmt=composed_fmt(trim(gen_fmt(val)),-size(val),.false.,-1)
     write (lch,trim(msgfmt)) mesg,val 
     if (index(how,'l')/=0.and.tty_size>0) &
&       call c_print(terminator(3),lch,terminator(4),"%s")
     if (index(how,'s')/=0) call live_time_msg("n",trim(lch),"","%s")
   end subroutine
   !
   subroutine nrmsg(how,mesg,val,USE_TABS,INDENT,formatted_msg)
     use stderr,   ONLY: c_print,write_to_log,tty_size
     use timing,   ONLY: live_time_msg
     character(*)     :: mesg
     character(*)     :: how
     real(SP)         :: val(:)
     integer,optional :: INDENT
     logical,optional :: USE_TABS
     character(lchlen), optional :: formatted_msg
     ! 
     ! Work Space
     !
     integer  :: i1,l_indent
     character(lchlen):: lch,msgfmt
     logical          :: l_fmt
     if (len_trim(how)==0) return
     call msg_manager(how)
     l_fmt=.false.
     l_indent=-1
     if (present(USE_TABS)) l_fmt=.true.
     if (present(INDENT)) l_indent=INDENT
     msgfmt=composed_fmt(trim(gen_fmt(r_v=val)),size(val),l_fmt,l_indent)
     do i1=1,max_open_ofs
       if (of_unit(i1)<=0) cycle
       if (present(formatted_msg)) then
         write (formatted_msg,trim(msgfmt)) mesg,val
       else
         write (of_unit(i1),trim(msgfmt)) mesg,val
         of_unit(i1)=-of_unit(i1)
       endif
     enddo
     if (index(how,'o')>0.or.index(how,'O')>0) return
     if (.not.write_to_log.or.(index(how,'s')==0.and.index(how,'l')==0)) return
     msgfmt=composed_fmt(trim(gen_fmt(r_v=val)),-size(val),.false.,-1)
     write (lch,trim(msgfmt)) mesg,val 
     if (index(how,'l')/=0.and.tty_size>0) call &
&       c_print(terminator(3),lch,terminator(4),"%s")
     if (index(how,'s')/=0) call live_time_msg("n",trim(lch),"","%s")
   end subroutine
   !
   character(lchlen) function composed_fmt(basic_fmt,n,USE_TABS,INDENT)
     use stderr, ONLY:string_pack,of_tabs
     character(*)     :: basic_fmt
     integer          :: n,INDENT
     logical          :: USE_TABS
     ! 
     ! Work Space
     !
     integer           :: i1
     character(lchlen) :: lch
     !
     if (USE_TABS) then
       composed_fmt=string_pack(terminator(1),'a,')
       lch=composed_fmt
       do i1=1,n-1
         write (lch,'(2a,i3.3,3a)') trim(composed_fmt),'t',&
&                                   of_tabs(i1)+2+INDENT,',',basic_fmt,','
         composed_fmt=lch
       enddo
       write (composed_fmt,'(2a,i3.3,3a)') trim(lch),'t',of_tabs(n)+2+INDENT,&
&                                        ',',basic_fmt,trim(terminator(2))
     else
       write (composed_fmt,'(a,i2.2,a,i3.2,4a)') trim(terminator(1)),depth+1,'x,a,',n,&
&                                             '(',basic_fmt,',1x)',trim(terminator(2))
       if (INDENT==0) write (composed_fmt,'(2a,i2.2,4a)') &
&         trim(terminator(1)),'a,',n,'(',basic_fmt,',1x)',trim(terminator(2))
     endif
     if (n<0) write (composed_fmt,'(a,i3.2,3a)') '(a,',iabs(n),'(',basic_fmt,',1x))'
   end function
   !
   subroutine msg_manager(how)
     !
     ! how = '(n)s(n)' '(n)r(n)' '(n)l(n)' 'o PATTERN'
     !
     ! r(eport)
     ! s(tderr)
     ! oN=msgunits(N) 
     !
     use stderr,  ONLY:string_split
     character(*) how
     ! 
     ! Work Space
     !
     integer          ::i1,i2
     character(schlen)::ch_piece(10)
     !
     terminator(1)='('
     terminator(2)=')'
     !
     ! o. files
     !
     if (how(:1)=="o") then
       call string_split(how,ch_piece)
       do i2=2,10
         if (len_trim(ch_piece(i2))==0) cycle
         do i1=1,max_open_ofs-1
           if (index( opened_of(i1),trim(ch_piece(i2)) )/=0) then
             of_unit(i1) =-of_unit(i1)
           endif
         enddo
       enddo
       return
     endif
     !
     ! Report
     !
     if (index(how,'r' )/=0.and.write_to_report) then
       of_unit(max_open_ofs) =-of_unit(max_open_ofs)
       if (index(how,'nr')/=0) terminator(1)='(/'
       if (index(how,'rn')/=0) terminator(2)='/)'
     endif
     !
     ! Log 
     !
     if (index(how,'l' )/=0) then
       terminator(3:4)=' '
       if (index(how,'nl')/=0) terminator(3)='n'
       if (index(how,'ln')/=0) terminator(4)='n'
     endif
     !
   end subroutine
   !
end module com

!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
  implicit none
  !
  ! PATHS & JOB_str
  !
  character(lchlen)  :: in_path
  character(lchlen)  :: out_path
  character(lchlen)  :: db_wr_path
  character(lchlen)  :: jobstr
  character(lchlen)  :: repfile
  !
  character(10)::build_date
  integer      ::code_version(3)
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
  integer,parameter  :: nofs=100
  integer,parameter  :: max_open_ofs=100
  character(lchlen)  :: of(nofs)
  character(lchlen)  :: opened_of(max_open_ofs)
  integer            :: of_unit(max_open_ofs)
  logical            :: write_to_of
  logical            :: write_to_report
  integer, parameter :: of_tabs(10)=(/2,13,24,35,46,57,68,79,90,101/)
  !
  interface msg
    module procedure na0msg,na1msg,namsg,ni1msg,nimsg,nr1msg,nrmsg,nl0msg
  end interface
  !
contains
  !
  character(lchlen) function get_name(desc,subfolder,type,TO_RD,TO_WR,NETCDF)
    !
    ! -2 ----------------  -> desc (NETCDF as OUTPUT)
    ! -1 ----------------  -> subfolder/(n)desc
    !  0 Stable s.         -> INPATH/SAVE/subfolder/(n)s.desc
    !  1 Stable db.        -> INPATH/SAVE/subfolder/(n)db.desc
    !  2 Job dependent db. -> OUTPATH/JOBSTR/subfolder/(n)db.desc
    !  3 Job dependent o.  -> OUTPATH/o.desc-JOBSTR
    !  4 ----------------  -> OUTPATH/RESTART/(n)db.desc_FOLDER
    !                                                    |
    !                                                    JOBSTR, or  SAVE if no JOBSTR 
    use stderr,         ONLY:string_pack
    character(*)           ::desc
    integer                ::type,ID
    logical                ::TO_RD,TO_WR,NETCDF
    character(*), optional ::subfolder
    ! 
    ! Work Space
    !
    character(schlen)::ch(2)
    character(lchlen)::folder
    !
    get_name=""
    folder="SAVE"
    if (.not.TO_RD.and..not.TO_WR) return
    !
    if (TO_RD) ch(1)=in_path
    if (TO_WR) ch(1)=out_path
    !
    select case(type)
      case(-2)
        get_name=string_pack('./',desc)
        NETCDF=index(desc,'ndb')>0
        return
      case(-1)
        ch(2)=desc
        folder="."
        if (present(subfolder)) folder=subfolder
      case(0)
        ch(2)=string_pack('s.',desc)
      case(1)
        ch(2)=string_pack('db.',desc)
      case(2,4)
        ch(2)=string_pack('db.',desc)
        if (len_trim(jobstr)>0) folder=trim(jobstr)
        if (type==4) then
          ch(2)=string_pack('db.',desc,'_',trim(folder))
          folder="RESTART"
        endif
      case(3)
        ch(2)=string_pack('o.',desc)
        folder=""
        if (len_trim(jobstr)>0) ch(2)=string_pack('o-',trim(jobstr),'.',desc)
    end select 
    !
    ! The RESTART folder is always in the outpath
    !
    if (type==4 ) ch(1)=out_path
    !
    ! No inpath/outpath must pre-appended when an explicit path is given
    !
    if (type==-1) ch(1)=' '
    !
    if (len_trim(folder)>0) ch(1)=string_pack(trim(ch(1)),'/',folder)
    if (present(subfolder).and.type>=0) ch(1)=string_pack(trim(ch(1)),'/',subfolder)
    if (NETCDF) then
      get_name=string_pack(trim(ch(1)),'/n',trim(ch(2)))
    else
      get_name=string_pack(trim(ch(1)),'/',trim(ch(2)))
    endif
    !
  end function
  !
  subroutine of_open_close(of_name,mode)
    !
    character(*)           :: of_name
    character(*),optional  :: mode
    ! 
    ! Work Space
    !
    integer          :: n_files,i1,i2,i3,file_index
    character(lchlen):: local_file_name
    !
    if (len_trim(of_name)==0) return
    !
    if (present(mode)) then
      if ( (index(mode,'o')==0.and.index(mode,'O')==0) .or. .not.write_to_of) return
      !
      ! Here I open the unit using file_name to define the name
      !
      local_file_name=of_name
      if (index(mode,'a')==0.and.index(mode,'O')==0) then
        local_file_name=get_name(desc=of_name,type=3,TO_RD=.false.,&
&                                TO_WR=.true.,NETCDF=.false.)
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
&                                  position='append')
      if (index(mode,"t")>0) call title(of_unit(file_index),'#')
   
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
  subroutine title(ou,cmnt)
    use timing,       ONLY:live_time_msg
    use par_proc_m,   ONLY:master_node
    integer     ::ou
    character(1)::cmnt
    ! 
    ! Work Space
    !
    character(schlen)::ch,lfmt
    integer          ::isp
    include 'version.inc'
    if (trim(cmnt)=='') isp=10
    if (trim(cmnt)=='#') isp=1
    if (.not.master_node) return
    if (ou<0) return
    select case(ou)
      case (:5,7:)
        write (lfmt,'(a,i2.2,a)') '(t',isp,',3a)'
        write (ou,trim(lfmt)) trim(cmnt)
        write (ou,trim(lfmt)) trim(cmnt),'   ______   ______   _       _______'
        write (ou,trim(lfmt)) trim(cmnt),'  /  ____) |  ____) | |     |  _____)'
        write (ou,trim(lfmt)) trim(cmnt),' (  (___   | (___   | |     | (_____'
#if defined irix
        write (ou,trim(lfmt)) trim(cmnt),'  \___  \  |  ___)  | |     |  _____)'
#else
        write (ou,trim(lfmt)) trim(cmnt),'  \\___  \\  |  ___)  | |     |  _____)'
#endif
        write (ou,trim(lfmt))  trim(cmnt),'  ____)  ) | (____  | |___  | |'
        write (ch,'(a,i1.1,a,i1.1,a,i2.2)') ' (______/  |______) |_____) |_|',&
&             code_version(1),'.',code_version(2),'.',code_version(3)
        write (ou,trim(lfmt)) trim(cmnt),trim(ch)
        write (ou,trim(lfmt)) trim(cmnt)
        write (ou,trim(lfmt)) trim(cmnt),'           Build ',trim(build_date)
        write (ou,trim(lfmt)) trim(cmnt),' http://www.fisica.uniroma2.it/~self'
        write (ou,trim(lfmt)) trim(cmnt)
      case (6)
        call live_time_msg("n",&
&         "   ______   ______   _       _______ ","","%s",NO_CPUT=.true.)
        call live_time_msg("n",&
&         "  /  ____) |  ____) | |     |  _____)","","%s",NO_CPUT=.true.)
        call live_time_msg("n",&
&         " (  (___   | (___   | |     | (_____ ","","%s",NO_CPUT=.true.)
#if defined irix
        call live_time_msg("n",&
&         "  \___  \  |  ___)  | |     |  _____)","","%s",NO_CPUT=.true.)
#else
        call live_time_msg("n",&
&         "  \\___  \\  |  ___)  | |     |  _____)","","%s",NO_CPUT=.true.)
#endif
        call live_time_msg("n",&
&         "  ____)  ) | (____  | |___  | |","","%s",NO_CPUT=.true.)
        write (ch,'(a,i1.1,a,i1.1,a,i2.2)') &
&         ' (______/  |______) |_____) |_|',&
&         code_version(1),'.',code_version(2),'.',code_version(3)
        call live_time_msg("n",trim(ch),"","%s",NO_CPUT=.true.)
        write (ch,'(2a)') ' Build ',trim(build_date)
        call live_time_msg("n",trim(ch),"n","%s",NO_CPUT=.true.)
    end select
    !
  end subroutine
  !
  logical function file_exists(name)
    character(*) name
    file_exists=.false.
    if (trim(name)=='') return
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
    use timing,     ONLY :live_time_msg,live_timing_is_on
    use stderr,     ONLY :string_pack
    use par_proc_m, ONLY :p_finalize
    character(*) mesg
    if (file_exists(repfile) ) call report_file_fix
    live_timing_is_on=.FALSE.
    call msg('nr',string_pack('[ERROR] STOP signal received while in :',trim(secnm)))
    call live_time_msg("nn",&
&        string_pack('[ERROR] STOP signal received while in :',trim(secnm)),"","%s")
    call msg('nr',string_pack('[ERROR] ',trim(mesg)))
    call live_time_msg("n",string_pack('[ERROR] ',trim(mesg)),"","%s")
    call live_time_msg("n","","","%s")
    call p_finalize
    stop
  end subroutine
  !
  subroutine report_file_fix
    if (.not.write_to_report) return
    close(11)
    open(unit=11,file=trim(repfile),position='append')
  end subroutine
  !
  !#######################
  ! INTERFACE STARTS HERE
  !#######################
  !
  subroutine na0msg(how,mesg)
    character(*)     :: mesg,how
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
    character(*)     :: mesg,how
    logical          :: val
    integer,optional :: INDENT
    logical,optional :: USE_TABS
    if (val) call na1msg(how,mesg,"yes",USE_TABS=USE_TABS,INDENT=INDENT)
    if (.not.val) call na1msg(how,mesg,"no",USE_TABS=USE_TABS,INDENT=INDENT)
  end subroutine
  !
  subroutine na1msg(how,mesg,val,USE_TABS,INDENT)
    character(*)     :: mesg,how,val
    integer,optional :: INDENT
    logical,optional :: USE_TABS
    call namsg(how,mesg,(/val/),USE_TABS=USE_TABS,INDENT=INDENT)
  end subroutine
  !
  subroutine ni1msg(how,mesg,val,USE_TABS,INDENT)
    character(*)     :: mesg,how
    integer          :: val
    integer,optional :: INDENT
    logical,optional :: USE_TABS
    call nimsg(how,mesg,(/val/),USE_TABS=USE_TABS,INDENT=INDENT)
  end subroutine
  !
  subroutine nr1msg(how,mesg,val,USE_TABS,INDENT)
    character(*)     :: mesg,how
    real(SP)         :: val
    integer,optional :: INDENT
    logical,optional :: USE_TABS
    call nrmsg(how,mesg,(/val/),USE_TABS,INDENT)
  end subroutine
  !
  subroutine namsg(how,mesg,val,USE_TABS,INDENT)
    use stderr,   ONLY: c_print,write_to_log,tty_size
    use timing,   ONLY: live_time_msg
    character(*)     :: mesg,how,val(:)
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
&      call c_print(terminator(3),lch,terminator(4),"%s")
    if (index(how,'s')/=0) call live_time_msg("n",trim(lch),"","%s")
  end subroutine
  !
  subroutine nimsg(how,mesg,val,USE_TABS,INDENT)
    use stderr,   ONLY: c_print,write_to_log,tty_size
    use timing,   ONLY: live_time_msg
    character(*)     :: mesg,how
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
&      call c_print(terminator(3),lch,terminator(4),"%s")
    if (index(how,'s')/=0) call live_time_msg("n",trim(lch),"","%s")
  end subroutine
  !
  subroutine nrmsg(how,mesg,val,USE_TABS,INDENT)
    use stderr,   ONLY: c_print,write_to_log,tty_size
    use timing,   ONLY: live_time_msg
    character(*)     :: mesg,how
    real(SP)         :: val(:)
    integer,optional :: INDENT
    logical,optional :: USE_TABS
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
      write (of_unit(i1),trim(msgfmt)) mesg,val
      of_unit(i1)=-of_unit(i1)
    enddo
    if (index(how,'o')>0.or.index(how,'O')>0) return
    if (.not.write_to_log.or.(index(how,'s')==0.and.index(how,'l')==0)) return
    msgfmt=composed_fmt(trim(gen_fmt(r_v=val)),-size(val),.false.,-1)
    write (lch,trim(msgfmt)) mesg,val 
    if (index(how,'l')/=0.and.tty_size>0) call &
&      c_print(terminator(3),lch,terminator(4),"%s")
    if (index(how,'s')/=0) call live_time_msg("n",trim(lch),"","%s")
  end subroutine
  !
  character(lchlen) function composed_fmt(basic_fmt,n,USE_TABS,INDENT)
    use stderr, ONLY:string_pack
    character(*)     :: basic_fmt
    integer          :: n,INDENT
    logical          :: USE_TABS
    ! 
    ! Work Space
    !
    integer           :: i1,len_mesg
    character(lchlen) :: lch
    !
    if (USE_TABS) then
      composed_fmt=string_pack(terminator(1),'a,')
      lch=composed_fmt
      do i1=1,n-1
        write (lch,'(2a,i3.3,3a)') trim(composed_fmt),'t',&
&                                  of_tabs(i1)+2+INDENT,',',basic_fmt,','
        composed_fmt=lch
      enddo
      write (composed_fmt,'(2a,i3.3,3a)') trim(lch),'t',of_tabs(n)+2+INDENT,&
&                                       ',',basic_fmt,trim(terminator(2))
    else
      write (composed_fmt,'(a,i2.2,a,i3.2,4a)') trim(terminator(1)),depth+1,'x,a,',n,&
&                                            '(',basic_fmt,',1x)',trim(terminator(2))
      if (INDENT==0) write (composed_fmt,'(2a,i2.2,4a)') &
&        trim(terminator(1)),'a,',n,'(',basic_fmt,',1x)',trim(terminator(2))
    endif
    if (n<0) write (composed_fmt,'(a,i3.2,3a)') '(a,',iabs(n),'(',basic_fmt,',1x))'
  end function
  !
  character(schlen) function gen_fmt(i_v,r_v)
    integer ,optional :: i_v(:)
    real(SP),optional :: r_v(:)
    ! 
    ! Work Space
    !
    integer  :: MXexp,MNexp,MDexp,iexp,MXval,i1
    real(SP) :: MX,MN,abs_r_v
    integer, parameter :: f_length=9,g_length=9
    !
    if (present(i_v)) then
      MXval=max(maxval(i_v),-minval(i_v))
      iexp=1
      if (MXval/=0) iexp=nint(log10(real(MXval)))+2
      write (gen_fmt,'(a,i2.2)')  'i',iexp
    endif
    !
    if (present(r_v)) then
      MN=huge(SP)
      MX=-huge(SP)
      do i1=1,size(r_v)
        abs_r_v=abs(r_v(i1))
        if (abs_r_v<MN.and.abs_r_v>0.) MN=abs_r_v
        if (abs_r_v>MX.and.abs_r_v>0.) MX=abs_r_v
      enddo
      MXexp=int(log10(MX))
      MNexp=int(log10(MN))
      iexp=max(iabs(MXexp),iabs(MNexp))
      MDexp=int(log10(MX/MN))
      if (size(r_v)==1.and.r_v(1)==0.) then
        iexp=0
        MDexp=0
      endif
      if (iexp<=2.and.MDexp<=2) then
        !
        !  f_length-3-iexp:  3 for '-','.' + 1 as iexp(10)=1/iexp(100)=2
        !
        write (gen_fmt,'(2(a,i2.2))')  'F',f_length,'.',f_length-3-iexp
      else
        !
        !  g_length-6: 6 because of '-1.','E00'
        !
        if (iexp<10) write (gen_fmt,'(2(a,i2.2),a)')  'G',g_length,'.',g_length-5,'E1'
        if (iexp> 9) write (gen_fmt,'(2(a,i2.2),a)')  'G',g_length,'.',g_length-6,'E2'
      endif
    endif
    !
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

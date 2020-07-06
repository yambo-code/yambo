!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
module LIVE_t
 !
 use pars,     ONLY:SP,DP,schlen,lchlen
 use stderr,   ONLY:log_as_a_file,intc
 use openmp,   ONLY:master_thread
 !
 implicit none
 !
 ! Checked
 !
 integer           :: date_time_at_start(6)
 logical           :: live_timing_is_on
 !
 ! Live Timing
 !
 integer             :: nhash
 integer             :: time_steps
 integer             :: steps_done
 integer             :: steps_done_in_the_memory
 integer             :: hashes_now
 integer             :: hashes_done
 integer             :: memory_steps
 real(DP),allocatable:: cput_seg(:,:)
 real(DP),allocatable:: cput_sec(:,:)
 real(DP),allocatable:: cput_tot(:,:)
 real(DP)            :: cput_last_report
 real(DP)            :: cput_last_estimate
 character(schlen)   :: timing_name
 character(schlen)   :: USER_wall_time_string=" "
 integer             :: USER_wall_time(3)=0 ! Days, Hours, Minutes
 !
 ! l_* files
 !
 logical           :: log_line_to_dump
 character(lchlen) :: log_line
 !
 contains
   !
   subroutine GET_user_WALL_time( )
     integer           :: i_pos(3)
     i_pos=(/index(trim(USER_wall_time_string),"d"),index(trim(USER_wall_time_string),"h"),index(trim(USER_wall_time_string),"m")/) 
     if (i_pos(3)/=0) then
       if (i_pos(2)/=0) then
         read (USER_wall_time_string(i_pos(2)+1:i_pos(3)-1),*) USER_wall_time(3)
       else if (i_pos(1)/=0) then
         read (USER_wall_time_string(i_pos(1)+1:i_pos(3)-1),*) USER_wall_time(3)
       else 
         read (USER_wall_time_string(1:i_pos(3)-1),*) USER_wall_time(3)
       endif
     endif
     if (i_pos(2)/=0) then
       if (i_pos(1)/=0) then
         read (USER_wall_time_string(i_pos(1)+1:i_pos(2)-1),*) USER_wall_time(2)
       else 
         read (USER_wall_time_string(1:i_pos(2)-1),*) USER_wall_time(2)
       endif
     endif
     if (i_pos(1)/=0) then
       read (USER_wall_time_string(1:i_pos(1)-1),*) USER_wall_time(1)
     endif
     USER_wall_time_string=trim(intc(USER_wall_time(1)))//":"//trim(intc(USER_wall_time(2)))//":"//trim(intc(USER_wall_time(3)))
   end subroutine
   !
   character(lchlen) function date_and_time_string(dt_in,dt_out,skip_host)
     use C_driver,    ONLY:code_bin
     use parallel_m,  ONLY:host_name,host_name_length
     integer,optional    ::dt_in(6),dt_out(6)
     logical,optional    ::skip_host
     integer      :: dz(8)
     character(5) :: cz
     character(8) :: cd
     character(10):: ctz
     !
     if (present(dt_in)) then
       dz(:6)=dt_in
     else
       call date_and_time(cd,ctz,cz,dz)
     endif
     !
     if (present(skip_host)) then
       write (date_and_time_string,'(2(i2.2,a),i4.4,a,2(i2.2,a),3a)') &
&             dz(2),'/',dz(3),'/',dz(1),' at ',dz(5),':',dz(6)
     else
       write (date_and_time_string,'(2(i2.2,a),i4.4,a,2(i2.2,a),3a)') &
&             dz(2),'/',dz(3),'/',dz(1),' at ',dz(5),':',dz(6),' ',&
&             trim(code_bin),' @ ',host_name(:host_name_length)
     endif
     if (present(dt_out)) dt_out=dz(:6)
   end function
   !
   subroutine live_timing(message,steps,DEPTH)
     character(*),optional :: message
     integer     ,optional :: steps
     real(SP)    ,optional :: DEPTH
     !
     if (.not.master_thread) return
     if (.not.live_timing_is_on) return
     !
     if (.not.present(steps)) then
       call live_timing_close( )
       return
     endif
     !
     if (.not.present(message)) then
       call live_timing_add(steps)
       return
     endif
     !
     call live_timing_activate(message,steps)
     memory_steps=0
     !
     if (present(DEPTH)) then
       memory_steps=int(steps*DEPTH)
     endif
     !
   end subroutine
   !
   subroutine ct(INIT,INIT_SEG,SEG,INIT_SEC,SEC,FIN)
     !
     ! cput_seg = Segment CPUT
     ! cput_sec = Section CPUT
     !
     use parallel_m, ONLY:myid,ncpu
     implicit none
     logical, optional :: INIT,INIT_SEG,SEG,INIT_SEC,SEC,FIN
     !
     real(DP) :: cput_now
     !
     call cti(cput_now)
     if (present(INIT)) then
       allocate(cput_seg(ncpu,2),cput_sec(ncpu,2),cput_tot(ncpu,2))
       cput_seg=0._DP
       cput_seg(myid+1,2)=cput_now
       cput_sec=0._DP
       cput_sec(myid+1,2)=cput_now
       cput_tot=0._DP
       cput_tot(myid+1,2)=cput_now
     endif
     cput_tot(myid+1,1)=(cput_now-cput_tot(myid+1,2))
     !
     if (present(INIT_SEC)) cput_sec(myid+1,:)=(/0.d0,cput_now/)
     if (present(INIT_SEG)) cput_seg(myid+1,:)=(/0.d0,cput_now/)
     !
     if (present(SEC)) cput_sec(myid+1,1)=(cput_now-cput_sec(myid+1,2))
     if (present(SEG)) cput_seg(myid+1,1)=(cput_now-cput_seg(myid+1,2))
     !
     if (present(FIN)) deallocate(cput_seg,cput_sec,cput_tot)
     !
   end subroutine
   !
   character(schlen) function time_string(tcpu)
     implicit none
     real(DP) tcpu
     ! 
     ! Work Space
     !
     real(DP) ltcpu
     integer :: d,h,m,s
     character(4) :: cs,cm,ch,cd
     ltcpu=abs(tcpu)
     d=int(ltcpu/86400.d0)
     ltcpu=ltcpu-d*86400.d0
     h=int(ltcpu/3600.d0)
     ltcpu=ltcpu-h*3600.d0
     m=int(ltcpu/60.d0)
     s=int(ltcpu-m*60.d0)
     cs=" "
     cm=" "
     ch=" "
     cd=" "
     if (d>0) then
       write (cd,'(i2.2,a)') d,'d'
       write (ch,'(a,i2.2,a)') '-',h,'h'
       write (ch,'(a,i2.2,a)') '-',m,'m'
     else if (h>0) then
       write (cd,'(i2.2,a)') h,'h'
       write (ch,'(a,i2.2,a)') '-',m,'m'
       if (s>0)  write (ch,'(a,i2.2,a)') '-',s,'s'
     else if (m>0) then
       write (cd,'(i2.2,a)') m,'m'
       write (ch,'(a,i2.2,a)') '-',s,'s'
     else if (s>1) then
       write (cd,'(i2.2,a)') s,'s'
     endif
     time_string=trim(cd)//trim(ch)//trim(cm)//trim(cs)
   end function
   !
   subroutine LIVE_message(message,lfmt,rfmt,sfmt,CPU_TIME,CPU_ID)
     !
     !C simple message:
     !
     ! lfmt    =left formatting (n nn r)
     ! message =Message
     ! rfmt    =right formatting (n nn r)
     ! sfmt    =Message formatting
     !
     use stderr,     ONLY : c_print,logfile,logfile_unit,write_to_log,intc,logfile_unit
     use parallel_m, ONLY : myid,ncpu,n_nodes,host_name
     implicit none
     character(*)          :: message
     character(*),optional :: lfmt,rfmt,sfmt
     logical,     optional :: CPU_TIME,CPU_ID
     ! 
     ! Work Space
     !
     character(lchlen):: lch,fmt_here,message_composed,time_is_now_string 
     character(schlen):: lfmt_,rfmt_,sfmt_
     logical          :: add_cput,add_CPU_id
     !
     if (.not.write_to_log.and.index(message,"[ERROR]")==0) return
     !
     rfmt_=" "
     sfmt_="%s"
     if (.not.log_as_a_file) lfmt_="r"
     if (     log_as_a_file) lfmt_="n"
     !
     if (present(lfmt)) lfmt_=trim(lfmt)
     if (present(rfmt)) rfmt_=trim(rfmt)
     if (present(sfmt)) sfmt_=trim(sfmt)
     !
     ! Update the reference CPUT
     !
     add_CPU_id=.true.
     if (present(CPU_ID)) then
       add_CPU_id=CPU_ID
     endif
     message_composed=trim(message)
     if (ncpu/=1.and.add_CPU_id.and.(index(lfmt_,"n")>0.or.index(lfmt_,"r")>0.)) then
       write (message_composed,'(a,i4.4,2a)') 'P',myid+1,': ',trim(message)
       if (n_nodes(1)>1) then
         message_composed='P'//trim(intc(myid+1))//"-"//trim(host_name)//": "//trim(message)
       else
         message_composed='P'//trim(intc(myid+1))//": "//trim(message)
       endif
     endif
     add_cput=live_timing_is_on.and.master_thread
     if (present(CPU_TIME)) then
       add_cput=CPU_TIME
     endif
     if (add_cput) then
       !
       ! Update cput_tot
       !
       call ct()
       !
       time_is_now_string=time_string(cput_tot(myid+1,1))
       if (len_trim(time_is_now_string)==0) time_is_now_string='---'
       write (lch,'(4a)') ' <',trim(time_is_now_string),'> ',trim(message_composed)
       message_composed=lch
     endif
     !
     ! TTY is active ? 
     !
     if (.not.log_as_a_file) then
       call c_print(lfmt_,trim(message_composed),rfmt_,sfmt_)
       return
     endif
     !
     ! ELSE write to log_file (if log_line_to_dump=.TRUE.)
     !
     ! The idea is :  lfmt message_composed rfmt ->   log_line(in)  screen     log_line(out)
     !                "n"  "A"              ""        "??"          "??"       "A"
     !                "n"  "B"              ""        "A"           "A"        "B"
     ! or
     !                "n"  "A"              ""        "??"          "??"       "A"
     !                ""   "B"              "n"       "A"           "AB"       ""
     ! or
     !                "n"  "A"              ""        "??"          "??"       "A"
     !                ""   "B"              ""        "A"           ""         "AB"
     !
     if (index(trim(lfmt_),"n")>0) then
       if (trim(lfmt_)=="nn") write (logfile_unit,'(a/)') ""
       write (logfile_unit,'(a)') trim(log_line)
       log_line=trim(message_composed) 
       if (trim(rfmt_)=="n".or.trim(rfmt_)=="nn") log_line=""
       if (trim(rfmt_)=="nn") write (logfile_unit,'(a/)') ""
       call yambo_flush(logfile_unit)
       return
     endif
     fmt_here='(2a)'
     if (trim(message)=='|'.and.nhash/=hashes_done) then
       write (fmt_here,'(a,i2.2,a)') '(a,',nhash-hashes_done,'x,a)'
     endif
     write (lch,trim(fmt_here)) trim(log_line),trim(message_composed)
     log_line=lch
     if (index(trim(rfmt_),"n")>0) then
       write (logfile_unit,'(a)') trim(log_line)
       if (trim(rfmt_)=="nn") write (logfile_unit,'(a/)') ""
       log_line=""
     endif
     call yambo_flush(logfile_unit)
     !
   end subroutine
   !
   subroutine live_timing_activate(name,steps)
     implicit none
     character(*)      :: name
     ! 
     ! Work Space
     !
     integer      :: steps
     !
     timing_name=name
     hashes_done=0
     hashes_now=0
     time_steps=steps
     steps_done=0
     steps_done_in_the_memory=0
     cput_last_report=0.
     cput_last_estimate=0.
     call ct(INIT_SEG=.true.)
     if (.not.log_as_a_file) call LIVE_message(trim(name),"n","","%s")
     call live_timing_update("--","--")
   end subroutine
   !
   subroutine live_timing_close( )
     implicit none
     ! 
     if (steps_done==time_steps) return
     !
     call live_timing_add(steps=time_steps-steps_done)
     !
   end subroutine
   !
   !
   subroutine live_timing_add(steps)
     use parallel_m, ONLY : myid
     implicit none
     integer :: steps
     ! 
     ! Work Space
     !
     character(schlen)  :: xtch,etch
     real(DP),parameter :: rts=5._DP
     real(DP) :: lts
     logical  :: time_report
     !
     time_report=.false.
     !
     call ct(SEG=.true.)
     if (steps_done+steps<=time_steps) then
       steps_done=steps_done+steps
       steps_done_in_the_memory=steps_done_in_the_memory+steps
     else
       steps_done=time_steps
     endif
     !
     if (time_steps > 0 ) then
       hashes_now=int(real(steps_done)/real(time_steps)*real(nhash))
     else
       hashes_now=0
     endif
     !
     if (hashes_now==hashes_done) return
     !
     etch=time_string(cput_seg(myid+1,1)) 
     if (len_trim(etch)==0) etch='--'
     !
     time_report=cput_seg(myid+1,1)-cput_last_report>=rts.or.steps_done==time_steps
     !
     if (.not.time_report) return
     !
     ! report...
     !
     cput_last_report=cput_seg(myid+1,1)
     !
     if (memory_steps>0) then
       if (steps_done_in_the_memory>=memory_steps) then
         lts=(cput_seg(myid+1,1)-cput_last_estimate)*real(time_steps-steps_done,DP)/real(steps_done_in_the_memory,DP)
         lts=lts+cput_seg(myid+1,1)
         steps_done_in_the_memory=0
         cput_last_estimate=cput_seg(myid+1,1)
       else
         lts=cput_seg(myid+1,1)*real(time_steps,DP)/real(steps_done,DP)
       endif
     else
       lts=cput_seg(myid+1,1)*real(time_steps,DP)/real(steps_done,DP)
     endif
     !
     xtch=time_string(lts)
     if (len_trim(xtch)==0) xtch='--'
     !
     call live_timing_update(trim(xtch),trim(etch))
     !
   end subroutine
   !
   subroutine live_timing_update(xch,ech)
     implicit none
     character(*)::xch,ech
     ! 
     ! Work Space
     !
     integer          :: perc,i1
     character(schlen):: lch,lfmt,lmsg
     !
     if (hashes_now==hashes_done.and.steps_done/=0) return
     !
     ! Write the descriptor 
     !
     write (lch,'(2a)') trim(timing_name),' |'
     if (.not.log_as_a_file) call LIVE_message(lch,"r","","%s")
     if (log_as_a_file) call LIVE_message(lch,"n","","%s")
     !
     ! Write the progression bar
     !
     if (hashes_now/=hashes_done) hashes_done=hashes_now
     !
     do i1=1,hashes_done
       call LIVE_message("#","","","%s",CPU_TIME=.false.)
     enddo
     !
     i1=nhash-hashes_done+1 
     if (hashes_done==nhash) i1=0
     if (i1>=10) write (lfmt,'(a,i2.2,a)') '%',nhash-hashes_done+1,'s'
     if (i1< 10) write (lfmt,'(a,i1.1,a)') '%',nhash-hashes_done+1,'s'
     call LIVE_message("|","","",lfmt,CPU_TIME=.false.)
     perc=int(real(steps_done)/real(time_steps)*100.)
     write (lmsg,'(1x,a,i3.3,5a)') '[',perc,'%] ',ech,'(E) ',xch,'(X)'
     call LIVE_message(lmsg,"","","%s",CPU_TIME=.false.)
     !
   end subroutine
   !
   ! COPYRIGHT
   ! Copyright (C) 1998-2002 ABINIT group (DCA, XG, GMR, LSI, MM, MT)
   ! This file is distributed under the terms of the
   ! GNU General Public License, see ~ABINIT/Infos/copyright
   ! or http://www.gnu.org/copyleft/gpl.txt .
   ! 
   ! NOTES
   ! For CPU time, contains machine-dependent code (choice will be selected
   ! by c preprocessor).
   ! Note that all supported machines are listed explicitly below; there
   ! is no "else" which covers "other".  The C preprocessor will place
   ! a spurious line of code (see below) into the fortran source unless
   ! preprocessed with -Dflag where flag refers to one of the supported machines.
   !
   ! Presently supported flags: "ibm", "hp", "P6", "dec_alpha", "irix",
   !    "T3E", "T3Efhi", "vpp", "sun", "mac", "nec", "sr8k".
   ! Previously supported flags:  "ultrix". Might still work !
   !
   ! Calls machine-dependent "mclock" for "ibm" .
   ! Calls machine-dependent "second" for "T3E", "T3Efhi".
   ! Calls ANSI C subroutine "cclock" for "hp", "ultrix", "irix", "PGIWin".
   ! Calls machine-dependent "etime" for "P6", "mac", "dec_alpha", "sun", "nec" .
   ! Calls machine-dependent "clock" for "vpp"
   ! Calls machine-dependent "xclock" for "sr8k"
   !
   subroutine cti(tcpu)
     implicit none
     real(DP) tcpu
#if defined _OPENMP
     real(DP), external :: qe_cclock
     tcpu=qe_cclock()      
#else
#  if defined _irix
     external cclock
     call cclock(tcpu)      
#  endif 
#  if defined _linux || defined _apple
     call cpu_time(tcpu)  ! standard fortran 95 function
#  endif
#  if defined _ibm || defined _aix || defined _ppc_linux
     integer :: mclock
     tcpu = mclock()*0.01d0
#  endif
#endif
   ! 
   end subroutine
   !
end module 

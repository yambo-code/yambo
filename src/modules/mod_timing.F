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
module timing
 !
 use pars,     ONLY:SP,DP,schlen,lchlen
 implicit none
 !
 ! What is running ?
 !
 character(schlen) :: what_is_running
 !
 ! Checked
 !
 integer           :: date_time_at_start(6)
 logical           :: live_timing_is_on,parallel_timing
 !
 ! Live Timing
 !
 integer             :: nhash
 integer             :: time_steps
 integer             :: steps_done
 integer             :: hashes_now
 integer             :: hashes_done
 real(SP),allocatable:: cput_seg(:,:)
 real(SP),allocatable:: cput_sec(:,:)
 real(SP),allocatable:: cput_tot(:,:)
 real(SP)            :: cput_save
 character(schlen)   :: timing_name
 !
 ! l_* files
 !
 logical           :: log_line_to_dump
 character(lchlen) :: log_line
 !
 contains
   !
   character(lchlen) function date_and_time_string(dt_in,dt_out)
     use par_proc_m,  ONLY:ncpu
     integer,optional    ::dt_in(6),dt_out(6)
     integer      :: dz(8)
     character(5) :: cz
     character(8) :: cd
     character(10):: ctz
     character(schlen) :: host_name
     integer           :: hn_len
     !
     if (present(dt_in)) then
       dz(:6)=dt_in
     else
       call date_and_time(cd,ctz,cz,dz)
     endif
     call igethname(host_name,hn_len)
     write (date_and_time_string,&
&          '(4a,i3.3,a,2(i2.2,a),i4.4,2x,i2.2,a,i2.2)') trim(what_is_running),'@',&
&           host_name(:hn_len),' x ',ncpu,' CPUs * ',&
&           dz(2),'/',dz(3),'/',dz(1),dz(5),':',dz(6)
     if (present(dt_out)) dt_out=dz(:6)
   end function
   !
   subroutine live_timing(message,steps,SERIAL)
     character(*),optional :: message
     integer     ,optional :: steps
     logical     ,optional :: SERIAL
     if (present(message).and.present(steps)) then
       if (present(SERIAL)) call live_timing_activate(message,steps,.TRUE.)
       if (.not.present(SERIAL)) call live_timing_activate(message,steps,.FALSE.)
     endif
     if (.not.present(message).and.present(steps)) call live_timing_add(steps)
     if (.not.present(message).and..not.present(steps)) call live_timing_finalize()
   end subroutine
   !
   subroutine ct(INIT,INIT_SEG,SEG,INIT_SEC,SEC,FIN)
     !
     ! cput_seg = Segment CPUT
     ! cput_sec = Section CPUT
     !
     use par_proc_m, ONLY : myid,ncpu
     implicit none
     logical, optional :: INIT,INIT_SEG,SEG,INIT_SEC,SEC,FIN
     !
     real(DP) :: cput_now
     !
     call cti(cput_now)
     if (present(INIT)) then
       allocate(cput_seg(ncpu,2),cput_sec(ncpu,2),cput_tot(ncpu,2))
       cput_seg=0.d0
       cput_sec=0.d0
       cput_tot=0.d0
       cput_tot(myid+1,2)=cput_now
     endif
     cput_tot(myid+1,1)=cput_now-cput_tot(myid+1,2)
     !
     if (present(INIT_SEC)) cput_sec(myid+1,:)=(/0.d0,cput_now/)
     if (present(INIT_SEG)) cput_seg(myid+1,:)=(/0.d0,cput_now/)
     !
     if (present(SEC)) cput_sec(myid+1,1)=cput_now-cput_sec(myid+1,2)
     if (present(SEG)) cput_seg(myid+1,1)=cput_now-cput_seg(myid+1,2)
     !
     if (present(FIN)) deallocate(cput_seg,cput_sec,cput_tot)
     !
   end subroutine
   !
   character(schlen) function time_string(tcpu)
     implicit none
     real(SP) tcpu
     ! 
     ! Work Space
     !
     real(SP) ltcpu
     integer:: d,h,m,s
     integer, parameter :: st=1
     ltcpu=abs(tcpu)
     d=int(ltcpu/86400.d0)
     ltcpu=ltcpu-d*86400.d0
     h=int(ltcpu/3600.d0)
     ltcpu=ltcpu-h*3600.d0
     m=int(ltcpu/60.d0)
     s=int(ltcpu-m*60.d0)
     time_string=' '
     if (s>=st) write (time_string,'(i2.2,a)') s,'s'
     if (m/=0)  write (time_string,'(2(i2.2,a))') m,'m-',s,'s'
     if (h/=0)  write (time_string,'(3(i2.2,a))') h,'h-',m,'m-',s,'s'
     if (d/=0)  write (time_string,'(4(i2.2,a))') d,'d-',h,'h-',m,'m-',s,'s'
   end function
   !
   subroutine live_time_msg(lfmt,message,rfmt,sfmt,NO_CPUT)
     !
     !C simple message:
     !
     ! lfmt    =left formatting (n nn r)
     ! message =Message
     ! rfmt    =right formatting (n nn r)
     ! sfmt    =Message formatting
     !
     use stderr,     ONLY : c_print,logfile,tty_size,write_to_log
     use par_proc_m, ONLY : myid,ncpu,master_cpu
     implicit none
     character(*)      :: lfmt,message,rfmt,sfmt
     logical, optional :: NO_CPUT
     ! 
     ! Work Space
     !
     character(lchlen):: lch,fmt,message_composed,time_is_now_string 
     logical          :: add_cput
     !
     if (.not.write_to_log) return
     !
     ! Update the reference CPUT
     !
     message_composed=trim(message)
     if (ncpu/=1.and.parallel_timing.and.(index(lfmt,"n")>0.or.index(lfmt,"r")>0.)) then
       write (message_composed,'(a,i2.2,2a)') 'P',myid+1,': ',trim(message)
     endif
     add_cput=live_timing_is_on
     if (present(NO_CPUT)) then
       add_cput=.not.NO_CPUT
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
     if (tty_size>0) then
       call c_print(lfmt,trim(message_composed),rfmt,sfmt)
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
     if (index(trim(lfmt),"n")>0) then
       open(unit=13,file=trim(logfile),position='append')
       if (trim(lfmt)=="nn") write (13,'(a/)') ""
       if (master_cpu) write (13,'(a)') trim(log_line)
       if (.not.master_cpu.and.len_trim(log_line)>0) write (13,'(a)') trim(log_line)
       log_line=trim(message_composed) 
       if (trim(rfmt)=="n".or.trim(rfmt)=="nn") log_line=""
       if (trim(rfmt)=="nn") write (13,'(a/)') ""
       close(13)
       return
     endif
     fmt='(2a)'
     if (trim(message)=='|'.and.nhash/=hashes_done) then
       write (fmt,'(a,i2.2,a)') '(a,',nhash-hashes_done,'x,a)'
     endif
     write (lch,trim(fmt)) trim(log_line),trim(message_composed)
     log_line=lch
     if (index(trim(rfmt),"n")>0) then
       open(unit=13,file=trim(logfile),position='append')
       if (master_cpu) write (13,'(a)') trim(log_line)
       if (.not.master_cpu.and.len_trim(log_line)>0) write (13,'(a)') trim(log_line)
       if (trim(rfmt)=="nn") write (13,'(a/)') ""
       close(13)
       log_line=""
     endif
     !
   end subroutine
   !
   subroutine live_timing_activate(name,steps,SERIAL)
     use stderr,     ONLY : tty_size,write_to_log
     implicit none
     character(*)      :: name
     logical           :: SERIAL
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
     cput_save=0.
     call ct(INIT_SEG=.true.)
     if (tty_size>0) call live_time_msg("n",trim(name),"","%s")
     call live_timing_update("--","--")
     if (.not.SERIAL) then
       write_to_log=.true.
       parallel_timing=.true.
     endif
   end subroutine
   !
   subroutine live_timing_add(steps)
     use par_proc_m, ONLY : myid,master_cpu
     implicit none
     integer :: steps
     ! 
     ! Work Space
     !
     character(schlen)  :: xtch,etch
     real(SP),parameter :: rts=5.,p_delay=10.
     real(SP) :: lts
     logical  :: time_report
     !
     time_report=.false.
     !
     call ct(SEG=.true.)
     steps_done=steps_done+steps
     hashes_now=int(real(steps_done)/real(time_steps)*real(nhash))
     if (hashes_now==hashes_done) return
     !
     etch=time_string(cput_seg(myid+1,1)) 
     if (len_trim(etch)==0) etch='--'
     time_report=cput_seg(myid+1,1)-cput_save>=rts+p_delay*myid
     if (.not.time_report) time_report=all((/master_cpu,steps_done==time_steps/))
     if (time_report) cput_save=cput_seg(myid+1,1)
     !
     if (time_report) then
       lts=cput_seg(myid+1,1)*real(time_steps)/real(steps_done)
       xtch=time_string(lts)
       if (len_trim(xtch)==0) xtch='--'
       if (trim(xtch)/='--'.or.master_cpu) call live_timing_update(trim(xtch),trim(etch))
     endif
     !
   end subroutine
   !
   subroutine live_timing_update(xch,ech)
     use stderr,     ONLY : tty_size
     use par_proc_m, ONLY : master_cpu
     implicit none
     character(*)::xch,ech
     ! 
     ! Work Space
     !
     integer          :: perc,i1
     character(schlen):: lch,lfmt,lmsg
     !
     if (hashes_now==hashes_done.and.steps_done/=0) return
     if (steps_done==0.and..not.master_cpu) return
     !
     ! Write the descriptor 
     !
     write (lch,'(2a)') trim(timing_name),' |'
     if (tty_size>0) call live_time_msg("r",lch,"","%s")
     if (tty_size<0) call live_time_msg("n",lch,"","%s")
     !
     ! Write the progression bar
     !
     if (hashes_now/=hashes_done) hashes_done=hashes_now
     do i1=1,hashes_done
       call live_time_msg("","#","","%s",NO_CPUT=.true.)
     enddo
     i1=nhash-hashes_done+1 
     if (hashes_done==nhash) i1=0
     if (i1>=10) write (lfmt,'(a,i2.2,a)') '%',nhash-hashes_done+1,'s'
     if (i1< 10) write (lfmt,'(a,i1.1,a)') '%',nhash-hashes_done+1,'s'
     call live_time_msg("","|","",lfmt,NO_CPUT=.true.)
     perc=int(real(steps_done)/real(time_steps)*100.)
     write (lmsg,'(1x,a,i3.3,5a)') '[',perc,'%] ',ech,'(E) ',xch,'(X)'
     call live_time_msg("",lmsg,"","%s",NO_CPUT=.true.)
     !
   end subroutine
   !
   subroutine live_timing_finalize
     use stderr,     ONLY : write_to_log
     use par_proc_m, ONLY : master_cpu
     implicit none
     parallel_timing=.false.
     write_to_log=master_cpu
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
#if defined irix
     external cclock
     call cclock(tcpu)      
#endif 
#if defined linux 
     real(SP) tmp(2) 
     real(DP) etime
     tcpu=etime(tmp)
#endif
#if defined ibm || defined aix || defined ppc_linux
     integer :: mclock
     tcpu = mclock()*0.01d0
#endif
   end subroutine
   !
end module 

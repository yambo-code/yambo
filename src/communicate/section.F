!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine section(mode,name)
 !
 ! mode = * 	: new basic level
 !      = +     : increase level
 !      = -     : decrease level
 !      = p     : increase level only in this section
 !      = #     : increase level #
 !      = r     : reset to zero level end exit
 !
 use pars,       ONLY:lchlen,schlen,DP
 use it_tools,   ONLY:ofiles_append
 use stderr,     ONLY:STRING_pack
 use LIVE_t,     ONLY:time_string,ct,date_time_at_start,cput_tot,cput_sec,&
&                     date_and_time_string
 use parallel_m, ONLY:PP_redux_wait,ncpu
 use com,        ONLY:msg,depth,previous_secmode,isec,secnm,&
&                     write_to_report,repfile_unit
 use timing_m,   ONLY:timing_upto_now
 !
 implicit none
 !
 character(1)::mode
 character(*)::name
 ! 
 ! Work Space
 !
 character(schlen)::chs
 character(lchlen)::ch,chp
 integer      :: i1,isec_save(5)
 real(DP)     :: cput_here(ncpu)
 !
 ! Final Timing Strings
 !
 character(schlen):: average_time,min_time,max_time
 !
 if (mode=='r') then
   depth=0
   return
 endif
 !
 call PP_redux_wait()
 !
 ! Get HostName date & time etc...
 !
 if (depth==-1) then
   ch=date_and_time_string(dt_out=date_time_at_start)
   if (write_to_report) write (repfile_unit,'(1x,a)') trim(ch)
   call write_the_logo(6,' ')
   previous_secmode=' '
 endif
 !
 ! To handle menu's like sections I need to reset 
 ! the depth when the previous section is 'p'-like
 !
 if (previous_secmode=='p') depth=depth-1
 !
 isec_save=isec
 !
 ! Update Section Depth & Timing 
 !
 previous_secmode=' '
 !
 select case (mode)
 case ('X','*')
   depth=0
   isec(2:)=0
   if (len_trim(name)==0) return
   call ct(SEC=.TRUE.)
   cput_here=cput_sec(:,1)
   call PP_redux_wait(cput_here)
   min_time=time_string(minval(cput_here))
   max_time=time_string(maxval(cput_here))
   average_time =time_string(sum(cput_here)/real(ncpu,DP))
   if (all((/len_trim(min_time)>0,len_trim(max_time)>0,len_trim(average_time)>0/))) then
     write (ch,'(6a)') 'Timing [Min/Max/Average]: ',trim(min_time),'/',trim(max_time),'/',trim(average_time)
     call msg('nr',trim(ch))
   endif
   call ct(INIT_SEC=.true.)
 case ('+')
   depth=depth+1
 case ('p')
   previous_secmode='p'
   depth=depth+1
 case ('2')
   depth=1
 case ('3')
   depth=2
 case ('-')
   depth=depth-1
   isec(depth+2:)=0
 end select
 !
 ! Write out the Section header (in secnm)
 !
 isec(depth+1)=isec(depth+1)+1
 write (secnm,'(a,i2.2)') '[',isec(1)
 do i1=2,5
  if (isec(i1)/=0) then
   write (chs,'(2a,i2.2)') trim(secnm),'.',isec(i1)
   secnm=chs
  endif
 enddo
 write (chs,'(3a)') trim(secnm),'] ',trim(name)
 secnm=chs
 call msg('nrs',trim(secnm))
 call msg('rn',repeat('=',len_trim(secnm)))
 !
 ! Finalize
 !
 if (mode=='X') then
   !
   cput_here=cput_tot(:,1)
   call PP_redux_wait(cput_here)
   !
   ch=date_and_time_string(dt_in=date_time_at_start)
   chp= STRING_pack(trim(ch),' [start]')
   !
   if (write_to_report) write (repfile_unit,'(1x,a)') chp(:len_trim(chp))
   call ofiles_append(line='')
   call ofiles_append(line=chp(:len_trim(chp)))
   !
   ch = date_and_time_string(skip_host=.TRUE.)
   chp= STRING_pack(trim(ch),repeat(' ',len_trim(chp)-len_trim(ch)-7)//'[end]')
   if (write_to_report) write (repfile_unit,'(1x,a)') chp(:len_trim(chp))
   call ofiles_append(line=chp(:len_trim(chp)))
   !
   min_time=time_string(minval(cput_here))
   max_time=time_string(maxval(cput_here))
   average_time =time_string(sum(cput_here)/real(ncpu,DP))
   !
   if (all((/trim(min_time)/='',trim(max_time)/='',trim(average_time)/=''/))) then
     !
     write (ch,'(6a)')     'Timing   [Min/Max/Average]: ',trim(min_time),'/',trim(max_time),'/',trim(average_time)
     !
     call msg('nr',trim(ch))
     call timing_upto_now()
     !
     call ofiles_append(line=" ")
     call ofiles_append(line=trim(ch))
     !
   else
     !
     call msg('nr'," ")
     !
   endif
   !
   call acknowledge_yambo()
   !
   call ct(FIN=.TRUE.)
   !
 endif
 !
 ! If mode is p
 !
 if (mode=='p') isec=isec_save
 !
 call PP_redux_wait
 !
end subroutine

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
subroutine section(mode,name)
 !
 ! mode = * 	: keep level
 !      = +      : increase level
 !      = -      : decrease level
 !      = p      : increase level only in this section
 !
 use pars
 use it_m,       ONLY:ofiles_append
 use memory_m,   ONLY:mem_est
 use stderr,     ONLY:string_pack
 use timing,     ONLY:time_string,ct,date_time_at_start,cput_tot,cput_sec,&
&                     live_time_msg,date_and_time_string
 use par_proc_m, ONLY:pp_redux_wait,ncpu
 use com,        ONLY:msg,depth,previous_secmode,isec,secnm,&
&                     write_to_report,title
 implicit none
 character(1)::mode
 character(*)::name
 ! 
 ! Work Space
 !
 character(lchlen)::ch,chp
 integer      :: i1,isec_save(5)
 real(SP)     :: cput_here(ncpu)
 !
 ! Final Timing Strings
 !
 character(schlen):: average_time,min_time,max_time
 !
 call pp_redux_wait
 !
 ! Get HostName date & time etc...
 !
 if (depth==-1) then
   ch=date_and_time_string(dt_out=date_time_at_start)
   if (write_to_report) write (11,'(1x,a)') trim(ch)
   call title(6,' ')
   previous_secmode=' '
 endif
 !
 ! To handle menu's like sections I need to reset 
 ! the depth when the previous section is 'p'-like
 !
 if (previous_secmode=='p') then
   depth=depth-1
   previous_secmode=' '
 endif
 isec_save=isec
 !
 ! Update Section Depth & Timing 
 !
 select case (mode)
 case ('X','*')
   depth=0
   isec(2:)=0
   if (len_trim(name)==0) return
   call ct(SEC=.TRUE.)
   cput_here=cput_sec(:,1)
   call pp_redux_wait(cput_here)
   min_time=time_string(minval(cput_here))
   max_time=time_string(maxval(cput_here))
   average_time =time_string(sum(cput_here)/real(ncpu))
   if (all((/len_trim(min_time)>0,len_trim(max_time)>0,len_trim(average_time)>0/))) then
     write (ch,'(6a)') 'CpuTiming [Min/Max/Average]: ',trim(min_time),&
&    '/',trim(max_time),'/',trim(average_time)
     call msg('nr',trim(ch))
   endif
   call ct(INIT_SEC=.true.)
 case ('+')
   depth=depth+1
 case ('p')
   previous_secmode='p'
   depth=depth+1
 case ('-')
   previous_secmode=' '
   depth=depth-1
 end select
 !
 ! Write out the Section header (in secnm)
 !
 ! MEM DEBUG
 ! call mem_est(trim(name),overall_report=.true.)
 !
 isec(depth+1)=isec(depth+1)+1
 write (secnm,'(a,i2.2)') '[',isec(1)
 do i1=2,5
  if (isec(i1)/=0) then
   write (ch,'(2a,i2.2)') trim(secnm),'.',isec(i1)
   secnm=ch
  endif
 enddo
 write (ch,'(3a)') trim(secnm),'] ',trim(name)
 secnm=ch
 call msg('nrs',trim(secnm))
 call msg('rn',repeat('=',len_trim(secnm)))
 if (mode=='X') call live_time_msg("","","n","%s",NO_CPUT=.true.)
 !
 ! Finalize
 !
 if (mode=='X') then
   cput_here=cput_tot(:,1)
   call pp_redux_wait(cput_here)
   !
   ch=date_and_time_string(dt_in=date_time_at_start)
   chp= string_pack(trim(ch),' [start]')
   if (write_to_report) write (11,'(1x,a)') chp(:len_trim(chp))
   call ofiles_append(line='')
   call ofiles_append(line=chp(:len_trim(chp)))
   !
   ch = date_and_time_string()
   chp= string_pack(trim(ch),' [end]')
   do i1=1,index(ch,'CPUs')+5
     chp(i1:i1)=" "
   enddo
   if (write_to_report) write (11,'(1x,a)') chp(:len_trim(chp))
   call ofiles_append(line=chp(:len_trim(chp)))
   !
   min_time=time_string(minval(cput_here))
   max_time=time_string(maxval(cput_here))
   average_time =time_string(sum(cput_here)/real(ncpu))
   if (all((/trim(min_time)/='',trim(max_time)/='',trim(average_time)/=''/))) then
     !
     ch = 'CpuTiming Min/Max/Average:'
     write (ch,'(6a)') 'Cpu Timing [Min/Max/Average]: ',trim(min_time),&
&     '/',trim(max_time),'/',trim(average_time)
     call msg('nrn',trim(ch))
     call ofiles_append(line='')
     call ofiles_append(line=trim(ch))
     !
   endif
   !
   call ct(FIN=.TRUE.)
   !
 endif
 !
 ! If mode is p
 !
 if (mode=='p') isec=isec_save
 !
 call pp_redux_wait
 !
 end subroutine

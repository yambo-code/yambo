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
subroutine section(mode,name)
 !
 ! mode =  * 	 : new basic level
 !      =  +     : increase level
 !      =  -     : decrease level
 !      =  p     : increase the level ONLY in this section
 !      = +P     : increase the level and save the current level
 !      = -P     : restore the level saved with +P and exit
 !      =  #     : increase level #
 !      =  r     : reset to zero level end exit
 !
 use pars,            ONLY:lchlen,schlen,DP
 use it_tools,        ONLY:ofiles_append
 use stderr,          ONLY:STRING_pack
 use LIVE_t,          ONLY:time_string,ct,date_time_at_start,cput_tot,cput_sec,date_and_time_string
 use parallel_m,      ONLY:ncpu
 use parallel_int,    ONLY:PP_redux_wait
 use com,             ONLY:msg,depth,isec,secnm,depth_SAVE_pp,isec_SAVE_pp,depth_SAVE_p,isec_SAVE_p,&
&                          write_to_report,repfile_unit,repfile,previous_secmode
 use timing_m,        ONLY:timing_upto_now
#if defined _YAML_OUTPUT
 use pars,            ONLY:repfile_index
 use com,             ONLY:of_yaml_IDs,depth_yaml
 use yaml_output,     ONLY:yaml_sequence_open,yaml_sequence_close,yaml_new_document,&
&                          yaml_comment,yaml_release_document!
#endif
 !
 implicit none
 !
 character(*)::mode
 character(*)::name
 ! 
 ! Work Space
 !
 character(schlen)::chs
 character(lchlen)::ch,chp
 integer      :: i1
 real(DP)     :: cput_here(ncpu)
 !
 ! Final Timing Strings
 !
 character(schlen):: average_time,min_time,max_time
 !
 if (trim(mode)=='r') then
   depth=0
   return
 endif
 !
 ! To handle menu's like sections I need to reset 
 ! the depth when the previous section is 'p'-like
 !
 if (trim(mode)=='-P') then
   isec=isec_SAVE_pp
   depth=depth_SAVE_pp
   previous_secmode=' '
   return
 endif
 if (trim(previous_secmode)=='p') then
   isec=isec_SAVE_p
   depth=depth_SAVE_p
   isec(depth+1)=isec(depth+1)+1
 endif
 if (trim(mode)=="+P") then
   isec_SAVE_pp=isec
   depth_SAVE_pp=depth 
 endif
 if (trim(mode)=="p") then
   isec_SAVE_p=isec
   depth_SAVE_p=depth 
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
 case ('+','+P')
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
#if defined _YAML_OUTPUT
 do while(depth_yaml>depth)
   call yaml_sequence_close(unit=of_yaml_IDs(repfile_index))
   depth_yaml=depth_yaml-1
   if(depth_yaml==0) call yaml_release_document(of_yaml_IDs(repfile_index))
 end do
 if(depth_yaml==0) call yaml_new_document(of_yaml_IDs(repfile_index))
#endif 
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
#if defined _YAML_OUTPUT
 !if(depth_yaml>0) call yaml_newline(unit=of_yaml_IDs(repfile_index))
 if(depth_yaml>0) call yaml_comment(" ",unit=of_yaml_IDs(repfile_index))
 call yaml_sequence_open(mapname=trim(name),unit=of_yaml_IDs(repfile_index))
 depth_yaml=depth_yaml+1
#endif
 !
 ! Finalize
 !
 if (trim(mode)=='X') then
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
 call PP_redux_wait
 !
end subroutine

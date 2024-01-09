! 
!        Copyright (C) 2004 WanT Group
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AF
! 
subroutine TIMING_clock_write(obj,what)
 !
 use pars,          ONLY:lchlen,DP,schlen
 use LIVE_t,        ONLY:LIVE_message,time_string
 use com,           ONLY:msg
 USE parallel_m,    ONLY:master_cpu,ncpu
 use stderr,        ONLY:intc
 use timing_m,      ONLY:clock,MAX_name_length,MAX_calls_length
 !
 implicit none
 !
 type(clock),            intent(in) :: obj(2) ! Min-Max clocks
 character(*),           intent(in) :: what
 ! 
 ! Work Space
 !
 character(schlen)                  :: sec_str,pre_str,cpu_str
 character(lchlen)                  :: rep_ch,log_ch,name
 integer                            :: call_number(2),i_c
 real(DP)                           :: total_time(2),effective_total_time(2),average_time(2)
 !
 ! Setup
 total_time     = obj(:)%total_time
 call_number    = obj(:)%call_number
 name           = obj(1)%name
 !
 pre_str="(a"//trim(intc(MAX_name_length+5))
 sec_str=" sec avg)"
 !
 do i_c=1,2
   effective_total_time(i_c) = total_time(i_c)
   if (total_time(i_c)<1._DP) then
     sec_str=" msec avg)"
     effective_total_time(i_c)=total_time(i_c)*1000._DP
   endif
   average_time(i_c)=effective_total_time(i_c) / REAL( call_number(i_c), DP )
 enddo
 !
 ! Log line
 !----------
 if (trim(what)=="LOG".and.ncpu>1) then
   cpu_str="CPU"
   WRITE (log_ch, trim(pre_str)//'," :")') TRIM(name)
   call LINE_total_time( 1, log_ch )
   IF ( call_number(1) > 1 )  call LINE_add_call_and_average( 1, log_ch )
   call LIVE_message(message="[TIMING]"//log_ch(:LINE_length(log_ch)),lfmt="n")
 else if (trim(what)=="REP") then
   ! Report line
   !-------------
   if (master_cpu) then
     WRITE (rep_ch, trim(pre_str)//'," :")') TRIM(name)
     do i_c=2,1,-1
       if (total_time(2)==total_time(1).and.i_c==1) cycle
       if (ncpu==1) cpu_str="CPU"
       if (ncpu> 1) cpu_str="P"//trim(intc(obj(i_c)%cpu_id))
       call LINE_total_time( i_c, rep_ch )
       IF ( call_number(i_c) > 1 )  call LINE_add_call_and_average( i_c, rep_ch )
       if (i_c==1) rep_ch=rep_ch(:LINE_length(rep_ch))//" [min]"
       if (i_c==2.and.total_time(2)/=total_time(1)) rep_ch=rep_ch(:LINE_length(rep_ch))//" [MAX]"
     enddo
     call msg('r',rep_ch(:LINE_length(rep_ch)))
   endif
 endif
 !
 contains
   !
   subroutine LINE_add_call_and_average( i_c, chin )
     integer           :: i_c
     CHARACTER(lchlen) :: ch,chin
     ch=chin
     WRITE(ch,'(a," (",i'//trim(intc(MAX_calls_length+1))//'," calls,",f8.3,a)') &
&          chin(:LINE_length(chin)),call_number(i_c),average_time(i_c),trim(sec_str)
     chin=ch
   end subroutine LINE_add_call_and_average
   !
   subroutine LINE_total_time( i_c , chin )
     integer           :: i_c
     CHARACTER(lchlen) :: ch,time_str,chin
     ch=chin
     if (total_time(i_c)<600.) then
       WRITE (chin, '(a,1x,f11.4,"s ",a)') trim(ch),total_time(i_c),trim(cpu_str)
     else
       time_str=time_string(total_time(i_c))
       chin=trim(ch)//" "//repeat(" ",12-len_trim(time_str))//trim(time_str)//" "//trim(cpu_str)
     endif
   end subroutine LINE_total_time
   !
   integer function LINE_length( chin )
     integer           :: i_s
     CHARACTER(lchlen) :: chin
     LINE_length=lchlen
     do i_s=lchlen-1,1,-1
       if (chin(i_s:i_s)/=" ") then
         LINE_length=i_s
         return
       endif
     enddo
   end function LINE_length
   !
end subroutine TIMING_clock_write        

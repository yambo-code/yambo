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
integer function ydb_i(lnstr,instr)
 !
 use pars,                ONLY:schlen,lchlen
 use IO_m,                ONLY:mk_dir 
 use com,                 ONLY:file_exists
 use LOGO,                ONLY:pickup_a_random
 use stderr,              ONLY:string_split,write_to_log,log_as_a_file,intc,cstr
 use LIVE_t,              ONLY:LIVE_message
 use parser_lib,          ONLY:iparse_init,iparse_end
 use parallel_m,          ONLY:ncpu
 !
 implicit none
 integer,          intent(in) :: lnstr
 character(lnstr), intent(in) :: instr
 !
 ! Work Space
 !
 integer,parameter::n_max_pieces=100
 character(schlen)::rstr_piece(n_max_pieces),known_runlevels(200)
 character(lchlen)::PATH,file_name,host_name
 integer          ::hn_len,n_pieces,i1,i_random,i_err
 integer          ::dz(8),year,day,month,hour,minutes
 character(5)     ::cz
 character(8)     ::cd
 character(10)    ::ctz
 !
 ! Presets
 !
 ydb_i =0
 ncpu  =1
 write_to_log =.TRUE.
 log_as_a_file=.FALSE.
 known_runlevels=' '
 !
 ! INPUT string splitter
 !
 call string_split(instr(:lnstr),rstr_piece)
 n_pieces=0
 do i1=1,n_max_pieces
   if (len_trim(rstr_piece(i1))>0) n_pieces=n_pieces+1
 enddo
 if (n_pieces==0) return
 !
 ! Date and Time
 !
 call date_and_time(cd,ctz,cz,dz)
 call igethname(host_name,hn_len)
 year=dz(1)
 month=dz(2)
 day=dz(3)
 hour=dz(5)
 minutes=dz(6)
 call LIVE_message('YDB running @'//host_name(:hn_len)//' '//trim(intc(day))//&
&                  '-'//trim(intc(month))//'-'//trim(intc(year))//' '//&
&                  trim(intc(hour))//':'//trim(intc(minutes)),"n","n","%s")
 !
 ! PATH
 !
 if (file_exists('~/.ydb/PATH')) then
   open(unit=1,file='~/.ydb/PATH',form='formatted')
   read (1,'(a)') PATH
   close(1)
 else 
   call LIVE_message('[ERROR] DB not found ',"n","n","%s")
   stop
 endif
 call LIVE_message('DB is in '//trim(PATH))
 !
 ! ADD
 !
 if (check_for_word("add")>0) then
   call LIVE_message('Adding ... '//trim(file_name),"n"," ","%s")
   call mk_dir(trim(PATH)//'/'//trim(intc(year)))
   call mk_dir(trim(PATH)//'/'//trim(intc(year))//'/'//trim(intc(month)))
   call mk_dir(trim(PATH)//'/'//trim(intc(year))//'/'//trim(intc(month))//'/'//trim(intc(day)))
   if (check_for_word("in")>0) then
     read(rstr_piece(check_for_word("in")+1),'(a)') file_name
     call LIVE_message('... INPUT file '//trim(file_name),"n"," ","%s")
     i_random=pickup_a_random(10000.)
     i_err=iparse_init(cstr(trim(file_name)),cstr('-'))
     write (*,*) i_random
     !call YDB_scan_file(trim(file_name))
     call iparse_end()
   endif
 endif
 !
 call LIVE_message(' ',"n","n","%s")
 !
 contains
   !
   integer function check_for_word(word)
     character(*) :: word
     integer      :: ic
     check_for_word=-1
     do ic=1,n_max_pieces
       if (trim(rstr_piece(ic))==word) check_for_word=ic
     enddo
   end function
end function

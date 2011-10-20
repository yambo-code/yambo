!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team
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
subroutine Fragments_Synchronize(ID,db_name,i_fragment,j_fragment)
 !
 use pars,           ONLY:schlen
 use com,            ONLY:core_io_path,more_io_path,msg
 use IO_m,           ONLY:synchronize_db,write_is_on,io_mode,VERIFY 
 use stderr,         ONLY:intc
 use fragments,      ONLY:last_syncronized,fragment_status
 use timing,         ONLY:hashes_now
 implicit none
 !
 integer          ::ID,i_fragment
 character(*)     ::db_name
 integer,optional ::j_fragment
 !
 ! Work Space
 !
 integer           ::i_loop
 character(schlen) ::fragment_filename 
 !
 if (more_io_path==core_io_path) return
 !
 if (i_fragment<0) then
   if (.not.io_mode(ID)==VERIFY) return
   call msg('s','[FRAGMENTs] Start-Up Synchronization ...')
   call synchronize_db(db_name)
   do i_loop=1,iabs(i_fragment)
     call build_the_filename()
     call synchronize_db(fragment_filename)
   enddo
   call msg('l','done')
   return
 endif
 !
 if (last_syncronized==hashes_now.or..not.write_is_on(ID)) return
 !
 if (last_syncronized==0) call synchronize_db(db_name)
 !
 do i_loop=1,i_fragment
   if (fragment_status(i_loop)==1) cycle
   call build_the_filename()
   call synchronize_db(fragment_filename)
   fragment_status(i_loop)=1
 enddo
 !
 last_syncronized=hashes_now
 !
 contains
   !
   subroutine build_the_filename
     fragment_filename=trim(db_name)//"_fragment_"//trim(intc(i_loop))
     if (present(j_fragment)) then
       fragment_filename=trim(db_name)//"_fragments_"//trim(intc(i_loop))//"_"//trim(intc(j_fragment))
     endif
   end subroutine
   !
end subroutine 

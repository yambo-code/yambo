!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine MEM_global_mesg(what,xkind,HOST_SIZE)
 !
 use pars,         ONLY:SP,DP
 use parallel_m,   ONLY:host_name
 use stderr,       ONLY:real2ch
 use units,        ONLY:Kilobyte
 use LIVE_t,       ONLY:LIVE_message
 !
 implicit none
 !
 character(*), intent(in) :: what
 integer,      intent(in) :: xkind
 integer,      intent(in) :: HOST_SIZE(:)
 !
 integer   :: i1
 real(DP)  :: MEM_size
 !
 MEM_size=0
 do i1=1,size(HOST_SIZE)
   MEM_size=MEM_size+real(HOST_SIZE(i1),DP)
 enddo
 MEM_size=MEM_size*real(xkind,DP)/real(Kilobyte,DP)/real(1000,DP)
 !
 ! Error Message
 !===============
 call LIVE_message("[MEMORY] Parallel distribution of "//what//" on HOST "//trim(host_name)//&
&                  " with size "//trim(real2ch(real(MEM_size,SP)))//" [Mb] ")
 !
 ! 2020/05/20: Here I do not use mem_string since it wants an integer in input
 !             It is not a good solution since integers are limited to their maximum value
 !             A real would be much better for the size in Kb ...
!&                  " with size "//trim(mem_string(real(MEM_size,SP))))
 !
 call MEM_error("GLOBAL_"//what)
 !
end subroutine MEM_global_mesg

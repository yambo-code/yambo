!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine MEM_report( REPORT )
 !
 use pars,         ONLY:lchlen
 use memory,       ONLY:MEMs,N_MEM_elements,mem_string,MEM_element,MAX_MEM_bytes,N_MEM_SAVE_elements,&
&                       LARGE_MEMs
 use units,        ONLY:Kilobyte
 use com,          ONLY:msg
 implicit none
 !
 character(*) :: REPORT
 !
 character(lchlen) :: msg_,prefix
 character(1)      :: where
 integer :: i_mem,i_el,ORPHAN_TOT,MEM
 !
 if (REPORT=="check") then
   where="s"
   prefix="[MEMORY]"
 else
   prefix=" "
   where="r"
   call msg("nrn","Memory Usage: global (Only MASTER cpu here). [O] stands for group 'O'")
 endif
 !
 ORPHAN_TOT=0
 msg_=" "
 if (N_MEM_elements>0) then
   !
   if (REPORT=="check") then
     WRITE(msg_,'(a9,a40)') trim(prefix),"=== Unallocated ==="
     call msg("s",msg_(1:len_trim(msg_)))
   else
     WRITE(msg_,'(a40)') "Unallocated components"
     call msg("rn",msg_(1:len_trim(msg_)))
   endif
   !
   do i_mem=1,N_MEM_elements
     !
     ORPHAN_TOT=ORPHAN_TOT+MEMs(i_mem)%use
     call plot_the_element(MEMs(i_mem))
     !
   enddo
   !
 endif
 !
 if (ORPHAN_TOT>0) then
   WRITE(msg_,'(a8,a70," : ",a,3x)') trim(prefix),"TOTAL",trim(mem_string(ORPHAN_TOT*int(Kilobyte)))
   call msg(where,msg_(1:len_trim(msg_)))
 endif
 !
 if (REPORT=="check") return
 !
 call msg("nrn","MAX allocations")
 if (N_MEM_SAVE_elements>0) then
   do i_mem=1,N_MEM_SAVE_elements
     call plot_the_element(LARGE_MEMs(i_mem))
   enddo
 endif
 !
 WRITE(msg_,'(1x,a8,a40," : ",3x,i10,a)') trim(prefix),"Max memory used",int(MAX_MEM_bytes/Kilobyte),"Kb"
 call msg("nr",trim(msg_))
 !
 contains
   !
   subroutine plot_the_element(M)
     type(MEM_element) :: M
     MEM=M%use*int(Kilobyte)
     do i_el=1,M%N
       MEM=M%size(i_el)*M%kind(i_el)
       if (M%N>1.or.(M%N==1.and.trim(M%shelf)/=trim(M%name(1)))) then
         WRITE(msg_,'(a8,a50)') trim(prefix),trim(M%name(i_el))//" "//trim(M%shelf)
       else
         WRITE(msg_,'(a8,a50)') trim(prefix),trim(M%name(i_el))
       endif
       WRITE(msg_,'(a," : ",a)') trim(msg_),trim(mem_string(MEM))
       if (len_trim(M%section(i_el))>0) then
         WRITE(msg_,'(a,a)')  trim(msg_)," in Section: "//trim(M%section(i_el))
       endif
       call msg(where,msg_(1:len_trim(msg_)))
     enddo
   end subroutine
   !
end subroutine

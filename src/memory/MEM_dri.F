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
subroutine MEM_dri(what,Sz,Kn,Er,quiet,REPORT)
 !
 use pars,         ONLY:SP
 use drivers,      ONLY:infile_editing
 use com,          ONLY:msg,error
 use openmp,       ONLY:master_thread
 use units,        ONLY:Kilobyte
 use stderr,       ONLY:intc
 use memory,       ONLY:TOT_MEM_kilobytes,TOT_MEM_kilobytes_MEMSTAT,MEM_element_name,&
&                       MEM_element_use,N_MEM_max,i_element_free,MEM_err,MEM_treshold,&
&                       N_MEM_elements
 !
 implicit none
 !
 character(*)         :: what
 integer,optional     :: Sz
 integer,optional     :: Kn
 integer,optional     :: Er
 logical,optional     :: quiet
 logical,optional     :: REPORT
 ! 
 ! Work Space
 !
 logical :: adding,deliver_a_msg,found
 integer :: i_mem,i_element,MEM_now
 !
 if (.not.master_thread) return
 !
 adding=present(Sz)
 !
 ! Error Message
 !=================
 if (MEM_err/=0) call error("Allocation of "//what//" failed")
 !
 ! Database Update
 !=================
 if (adding) then
   found=.TRUE.
   MEM_loopA: do i_mem=1,N_MEM_max
     if ( len_trim(MEM_element_name(i_mem))==0 ) then
       i_element_free=i_mem
       exit MEM_loopA
     endif
   enddo MEM_loopA
   MEM_now=real(Sz*Kn)/Kilobyte
   MEM_element_name(i_element_free)=what
   MEM_element_use(i_element_free) =MEM_now
   TOT_MEM_kilobytes=TOT_MEM_kilobytes+MEM_now
   N_MEM_elements=N_MEM_elements+1
 else
   found=.FALSE.
   MEM_loopB: do i_mem=1,N_MEM_max
     if ( trim(MEM_element_name(i_mem))==what ) then
       MEM_element_name(i_mem)=" "
       MEM_now=MEM_element_use(i_mem)
       MEM_element_use(i_mem) =0
       N_MEM_elements=N_MEM_elements-1
       TOT_MEM_kilobytes=TOT_MEM_kilobytes-MEM_now
       found=.TRUE.
       exit MEM_loopB
     endif
   enddo MEM_loopB
 endif
 !
 ! Process Memory Update
 !=======================
 call memstat( TOT_MEM_kilobytes_MEMSTAT )
 !
 ! Messages
 !==========
 deliver_a_msg=MEM_now>MEM_treshold.and..not.infile_editing.and.found
 if (deliver_a_msg) then
   if (adding     ) call msg("sr","[MEMORY] "//what//" allocated "//trim(intc(MEM_now))//" Kb")
   if (.not.adding) call msg("sr","[MEMORY] "//what//" freed "//trim(intc(MEM_now))//" Kb")
   call msg("sr","[MEMORY] SUM:"//trim(intc(TOT_MEM_kilobytes))//"Kb - MEMSTAT "//trim(intc(TOT_MEM_kilobytes_MEMSTAT))//" Kb")
 endif
 !
end subroutine

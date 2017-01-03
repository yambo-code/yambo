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
subroutine MEM_dri(what,Sz,Kn,Er,REPORT)
 !
 use pars,         ONLY:lchlen
 use drivers,      ONLY:infile_editing
 use com,          ONLY:msg,error
 use openmp,       ONLY:master_thread
 use units,        ONLY:Kilobyte,Megabyte
 use stderr,       ONLY:intc
 use memory,       ONLY:TOT_MEM_kilobytes,TOT_MEM_kilobytes_MEMSTAT,MEM_element_name,&
&                       MEM_element_use,N_MEM_max,i_element_free,MEM_err,MEM_treshold,&
&                       N_MEM_elements,TOT_MEM_kilobytes_MEMSTAT_SAVE
 !
 implicit none
 !
 character(*)         :: what
 integer,optional     :: Sz
 integer,optional     :: Kn
 integer,optional     :: Er
 logical,optional     :: REPORT
 ! 
 ! Work Space
 !
 logical :: adding,deliver_a_msg,found
 integer :: i_mem,i_element,MEM_now,delta_MEM,MEM_prefactor
 character(lchlen) :: msg_
 !
 if (.not.master_thread) return
 !
 adding=present(Sz)
 !
 ! Error Message
 !===============
 if (MEM_err/=0) call error("Allocation of "//what//" failed")
 !
 ! Process Memory Update
 !=======================
 call memstat( TOT_MEM_kilobytes_MEMSTAT )
 delta_MEM=TOT_MEM_kilobytes_MEMSTAT-TOT_MEM_kilobytes_MEMSTAT_SAVE
 TOT_MEM_kilobytes_MEMSTAT_SAVE=TOT_MEM_kilobytes_MEMSTAT
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
   if (MEM_now>0) then
     MEM_prefactor=nint(real(delta_MEM)/real(MEM_now))
     MEM_now=MEM_now*MEM_prefactor
     MEM_element_name(i_element_free)=what
     MEM_element_use(i_element_free) =MEM_now
     TOT_MEM_kilobytes=TOT_MEM_kilobytes+MEM_now
     N_MEM_elements=N_MEM_elements+1
   endif
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
 ! Messages
 !==========
 deliver_a_msg=MEM_now>MEM_treshold.and..not.infile_editing.and.found
 if (deliver_a_msg) then
   msg_="[MEMORY]"
   if (     adding) msg_=trim(msg_)//" Alloc "//what
   if (.not.adding) msg_=trim(msg_)//" Free "//what
   msg_=trim(msg_)//"("//trim(intc(int(MEM_now*Kilobyte/Megabyte)))//"Mb)"
   msg_=trim(msg_)//" TOTAL: "//trim(intc(int(TOT_MEM_kilobytes*Kilobyte/Megabyte)))//"Mb (sum)"
   msg_=trim(msg_)//" "//trim(intc(int(TOT_MEM_kilobytes_MEMSTAT*Kilobyte/Megabyte)))//"Mb (memstat)"
   call msg("sr",trim(msg_))
 endif
 !
end subroutine

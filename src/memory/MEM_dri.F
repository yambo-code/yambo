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
subroutine MEM_dri(what,Sz,Kn,REPORT)
 !
 use pars,         ONLY:lchlen
 use drivers,      ONLY:infile_editing
 use com,          ONLY:msg,error
 use openmp,       ONLY:master_thread
 use units,        ONLY:Kilobyte,Megabyte
 use stderr,       ONLY:intc
 use memory,       ONLY:TOT_MEM_kilobytes,TOT_MEM_kilobytes_MEMSTAT,MEM_element_name,&
&                       MEM_element_use,N_MEM_max,MEM_err,MEM_treshold,&
&                       N_MEM_elements,TOT_MEM_kilobytes_MEMSTAT_SAVE,MEM_jump_save,&
&                       N_MEM_SAVE_elements,MEM_element_name_SAVE,MEM_element_use_SAVE,&
&                       MEM_SAVE_treshold,MEM_jump_treshold,MAX_MEM_kilobytes_MEMSTAT
 !
 implicit none
 !
 character(*)         :: what
 integer,optional     :: Sz
 integer,optional     :: Kn
 character(*),optional:: REPORT
 ! 
 ! Work Space
 !
 logical :: adding,deliver_a_msg,found
 integer :: i_mem,MEM_now,delta_MEM,MEM_prefactor,i_mem_p,ORPHAN_TOT,i_same
 character(lchlen) :: msg_
 !
 if (.not.master_thread) return
 !
#if defined _MEM_CHECK
 !
 ! REPORTs 
 !=========
 if (present(REPORT)) then
   ORPHAN_TOT=0
   if (N_MEM_elements>0) then
     WRITE(msg_,'(1x,a8,a18," : ",3x)') "[MEMORY]","(still) Allocated"
     do i_mem=1,N_MEM_elements
       i_same=0
       do i_mem_p=i_mem-1,1,-1
         if (MEM_element_name(i_mem_p)==MEM_element_name(i_mem)) i_same=1
       enddo
       if  (i_same==0) msg_=trim(msg_)//" "//trim(MEM_element_name(i_mem))
       if (len_trim(msg_)>100) then
         call msg("r",msg_(1:len_trim(msg_)))
         WRITE(msg_,'(1x,a8,a18," : ",3x)') "[MEMORY]"
         ORPHAN_TOT=ORPHAN_TOT+MEM_element_use(i_mem)
       endif
     enddo
   endif
   if (ORPHAN_TOT>0) then
     WRITE(msg_,'(1x,a8,a18," : ",3x,i6,a)') "[MEMORY]","(still) Allocated",ORPHAN_TOT,"Kb"
     call msg("r",msg_(1:len_trim(msg_)))
   endif
   if (REPORT=="final") then
     if (N_MEM_SAVE_elements>0) then
       do i_mem=1,N_MEM_SAVE_elements
         WRITE(msg_,'(1x,a8,a18," : ",3x,i6,a)') "[MEMORY]",trim(MEM_element_name_SAVE(i_mem)),&
&                                                           int(MEM_element_use_SAVE(i_mem)),"Kb (cleaned)"
         call msg("r",trim(msg_))
       enddo
     endif
     WRITE(msg_,'(1x,a8,a18," : ",3x,i6,a)') "[MEMORY]","Max memory used",MAX_MEM_kilobytes_MEMSTAT,"Kb"
     call msg("r",trim(msg_))
   endif
   return
 endif
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
 if (TOT_MEM_kilobytes_MEMSTAT>MAX_MEM_kilobytes_MEMSTAT) MAX_MEM_kilobytes_MEMSTAT=TOT_MEM_kilobytes_MEMSTAT
 !
 ! Database Update
 !=================
 if (adding) then
   found=.TRUE.
   MEM_now=real(Sz*Kn)/Kilobyte
   if (MEM_now>0) then
     ! Complex value correction
     MEM_prefactor=nint(real(delta_MEM)/real(MEM_now))
     if (MEM_prefactor==2) MEM_now=MEM_now*MEM_prefactor
     N_MEM_elements=N_MEM_elements+1
     MEM_element_name(N_MEM_elements)=what
     MEM_element_use(N_MEM_elements) =MEM_now
     TOT_MEM_kilobytes=TOT_MEM_kilobytes+MEM_now
     if (MEM_now>MEM_SAVE_treshold) then
       N_MEM_SAVE_elements=N_MEM_SAVE_elements+1
       MEM_element_name_SAVE(N_MEM_SAVE_elements)=what
       MEM_element_use_SAVE(N_MEM_SAVE_elements) =MEM_now
     endif
   endif
 else
   found=.FALSE.
   MEM_loop: do i_mem=1,N_MEM_max
     if ( trim(MEM_element_name(i_mem))==what ) then
       MEM_now=MEM_element_use(i_mem)
       TOT_MEM_kilobytes=TOT_MEM_kilobytes-MEM_now
       found=.TRUE.
       do i_mem_p=i_mem,N_MEM_elements
         MEM_element_name(i_mem_p)=MEM_element_name(i_mem_p+1)
         MEM_element_use(i_mem_p)=MEM_element_use(i_mem_p+1)
       enddo    
       N_MEM_elements=N_MEM_elements-1
       exit MEM_loop
     endif
   enddo MEM_loop
 endif
 !
 ! Messages
 !==========
 !#1
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
 !#2
 deliver_a_msg=(TOT_MEM_kilobytes_MEMSTAT-MEM_jump_save)>int(MEM_jump_treshold*Megabyte/Kilobyte).and.adding
 if (deliver_a_msg) then
   msg_="[MEMORY] In use: "
   msg_=trim(msg_)//" TOTAL: "//trim(intc(int(TOT_MEM_kilobytes*Kilobyte/Megabyte)))//"Mb (sum)"
   msg_=trim(msg_)//" "//trim(intc(int(TOT_MEM_kilobytes_MEMSTAT*Kilobyte/Megabyte)))//"Mb (memstat)"
   call msg("sr",trim(msg_))
   MEM_jump_save=TOT_MEM_kilobytes_MEMSTAT
 endif
 !
#endif
 !
end subroutine

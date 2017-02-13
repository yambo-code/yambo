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
subroutine MEM_manager(what,Sz,Kn,REPORT)
 !
 use pars,         ONLY:lchlen,schlen
 use drivers,      ONLY:infile_editing
 use com,          ONLY:msg,error
 use openmp,       ONLY:master_thread
 use units,        ONLY:Kilobyte,Megabyte
 use stderr,       ONLY:intc
 use memory,       ONLY:TOT_MEM_bytes,TOT_MEM_kilobytes_MEMSTAT,MEM_element_name,&
&                       MEM_element_use,N_MEM_max,MEM_err,MEM_treshold,&
&                       N_MEM_elements,MEM_element_size,&
&                       N_MEM_SAVE_elements,MEM_element_name_SAVE,MEM_element_use_SAVE,&
&                       MEM_SAVE_treshold,MEM_jump_treshold,MAX_MEM_bytes,&
&                       MEM_element_kind,TOT_MEM_bytes_SAVE
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
 logical :: adding,deliver_a_msg,removing
 integer :: i_mem,MEM_now,i_mem_p,ORPHAN_TOT,i_same,i_FOUND,i_FOUND_SAVE
 character(lchlen) :: msg_
 !
 ! BlackList (not reported)
 !
 integer           :: i_bl,N_bl
 character(schlen) :: BlackList(100)
 !
 BlackList    =" "
 BlackList(1) ="g_vec"
 BlackList(2) ="dl_sop"
 BlackList(3) ="wf_igk"
 BlackList(4) ="g_rot"
 BlackList(5) ="rl_sop"
 BlackList(6) ="sop_tab"
 BlackList(7) ="minus_G"
 BlackList(8) ="k%star"
 BlackList(9) ="Eo%E"
 BlackList(10)="Eo%f"
 BlackList(11)="qindx_X"
 BlackList(12)="qindx_S"
 BlackList(13)="qindx_B"
 BlackList(14)="bare_qpg"
 BlackList(15)="k%k_table"
 BlackList(16)="k%s_table"
 BlackList(17)="G_m_G"
 BlackList(18)="E%E"
 BlackList(19)="E%f"
 BlackList(20)="E_of_shell"
 N_bl=20
 !
 ! Error Message
 !===============
 if (MEM_err/=0) call error("Allocation of "//what//" failed")
 !
 if (.not.present(Sz).and..not.present(Kn).and..not.present(REPORT)) return
 if (.not.master_thread) return
 !
#if defined _MEM_CHECK
 !
 ! REPORTs 
 !=========
 if (present(REPORT)) then
   !
   if (REPORT=="check") then
     ORPHAN_TOT=0
     msg_=" "
     if (N_MEM_elements>0) then
       WRITE(msg_,'(a8,a18," : ",3x)') "[MEMORY]","(still) Allocated"
       do i_mem=1,N_MEM_elements
         i_same=0
         do i_mem_p=i_mem-1,1,-1
           if (MEM_element_name(i_mem_p)==MEM_element_name(i_mem)) i_same=1
         enddo
         do i_bl=1,N_bl
           if (trim(MEM_element_name(i_mem))==trim(BlackList(i_bl))) i_same=1
         enddo
         if (i_same==0) msg_=trim(msg_)//" "//trim(MEM_element_name(i_mem))
         if (len_trim(msg_)>100) then
           call msg("s",msg_(1:len_trim(msg_)))
           msg_="[MEMORY]                 ..."
           ORPHAN_TOT=ORPHAN_TOT+MEM_element_use(i_mem)
         endif
       enddo
     endif
     if (len_trim(msg_)>30) call msg("s",msg_(1:len_trim(msg_)))
     if (ORPHAN_TOT>0) then
       WRITE(msg_,'(a8,a18," : ",3x,i10,a)') "[MEMORY]","(still) Allocated",ORPHAN_TOT,"Kb"
       call msg("s",msg_(1:len_trim(msg_)))
     endif
   else if (REPORT=="final") then
     if (N_MEM_SAVE_elements>0) then
       do i_mem=1,N_MEM_SAVE_elements
         WRITE(msg_,'(1x,a8,a18," : ",3x,i10,a)') "[MEMORY]",trim(MEM_element_name_SAVE(i_mem)),&
&                                                           int(MEM_element_use_SAVE(i_mem)),"Kb (cleaned)"
         call msg("r",trim(msg_))
       enddo
     endif
     WRITE(msg_,'(1x,a8,a18," : ",3x,i10,a)') "[MEMORY]","Max memory used",int(MAX_MEM_bytes/Kilobyte),"Kb"
     call msg("r",trim(msg_))
   endif
   return
 endif
 !
 adding  =     present(Kn)
 removing=.not.present(Kn)
 !
 ! Search the element
 !====================
 call MEM_element_finder(what,i_FOUND,i_FOUND_SAVE)
 !
 ! Database Update
 !=================
 if (adding) then
   !
   MEM_now=nint(real(Sz*Kn)/Kilobyte)
   TOT_MEM_bytes=TOT_MEM_bytes+Sz*Kn
   if (TOT_MEM_bytes>MAX_MEM_bytes) MAX_MEM_bytes=TOT_MEM_bytes
   !
   if (MEM_now>0) then
     if (i_FOUND==0) then
       N_MEM_elements=N_MEM_elements+1
       MEM_element_name(N_MEM_elements)=what
       MEM_element_use(N_MEM_elements) =MEM_now
       MEM_element_kind(N_MEM_elements)=Kn
       MEM_element_size(N_MEM_elements)=Sz
       i_FOUND=N_MEM_elements
     else
       MEM_element_use(i_FOUND) = MEM_element_use(i_FOUND)+MEM_now
     endif
     !
     if (MEM_element_use(i_FOUND)>MEM_SAVE_treshold) then
       if (i_FOUND_SAVE==0) then
         N_MEM_SAVE_elements=N_MEM_SAVE_elements+1
         MEM_element_name_SAVE(N_MEM_SAVE_elements)=what
         MEM_element_use_SAVE(N_MEM_SAVE_elements) =MEM_element_use(i_FOUND)
       else
         if (MEM_element_use(i_FOUND)>MEM_element_use_SAVE(i_FOUND_SAVE)) then
           MEM_element_use_SAVE(i_FOUND_SAVE) = MEM_element_use(i_FOUND)
         endif
       endif
     endif
     !
   else
     !
     return
     !
   endif
   !
 else if (i_FOUND/=0) then
   !
   MEM_now=nint(real(MEM_element_size(i_FOUND)*MEM_element_kind(i_FOUND))/Kilobyte)
   !
   MEM_element_use(i_FOUND) = MEM_element_use(i_FOUND)-MEM_now
   TOT_MEM_bytes=TOT_MEM_bytes-MEM_element_size(i_FOUND)*MEM_element_kind(i_FOUND)
   !
   if ( MEM_element_use(i_FOUND) <= 0 ) then
     !
     do i_mem=i_FOUND,N_MEM_elements
       MEM_element_name(i_mem)=MEM_element_name(i_mem+1)
       MEM_element_use(i_mem) =MEM_element_use(i_mem+1)
       MEM_element_kind(i_mem)=MEM_element_kind(i_mem+1)
       MEM_element_size(i_mem)=MEM_element_size(i_mem+1)
     enddo    
     !
     N_MEM_elements=N_MEM_elements-1
     !
   endif
   !
 else
   !
   return
   !
 endif
 !
 ! Messages
 !==========
 !#1
 deliver_a_msg=MEM_now>MEM_treshold.and..not.infile_editing
 if (deliver_a_msg) then
   call MEM_STAT_update( )
   msg_="[MEMORY]"
   if (     adding) msg_=trim(msg_)//" Alloc "//what
   if (.not.adding) msg_=trim(msg_)//" Free "//what
   msg_=trim(msg_)//"("//trim(intc(int(MEM_now*Kilobyte/Megabyte)))//"Mb)"
   msg_=trim(msg_)//" TOTAL: "//trim(intc(int(TOT_MEM_bytes/Megabyte)))//"Mb (sum)"
   msg_=trim(msg_)//" "//trim(intc(int(TOT_MEM_kilobytes_MEMSTAT*Kilobyte/Megabyte)))//"Mb (memstat)"
   call msg("s",trim(msg_))
 endif
 !#2
 deliver_a_msg=(TOT_MEM_bytes-TOT_MEM_bytes_SAVE)>int(MEM_jump_treshold*Megabyte/Kilobyte).and.adding
 if (deliver_a_msg) then
   call MEM_STAT_update( )
   msg_="[MEMORY] In use: "
   msg_=trim(msg_)//" TOTAL: "//trim(intc(int(TOT_MEM_bytes/Megabyte)))//"Mb (sum)"
   msg_=trim(msg_)//" "//trim(intc(int(TOT_MEM_kilobytes_MEMSTAT*Kilobyte/Megabyte)))//"Mb (memstat)"
   call msg("s",trim(msg_))
   TOT_MEM_bytes_SAVE=TOT_MEM_bytes
 endif
 !
#endif
 !
 contains
   !
   subroutine MEM_element_finder( name, i_FOUND, i_FOUND_SAVE )
     integer      :: i_m,i_FOUND,i_FOUND_SAVE
     character(*) :: name
     i_FOUND=0
     i_FOUND_SAVE=0
     LOOP_1: do i_m=1,N_MEM_max
       if ( trim(MEM_element_name(i_m))==name .and. MEM_element_size(i_m)==Sz ) then
         i_FOUND=i_m
         exit LOOP_1
       endif
     enddo LOOP_1
     LOOP_2: do i_m=1,N_MEM_SAVE_elements
       if ( trim(MEM_element_name_SAVE(i_m))==name ) then
         i_FOUND_SAVE=i_m
         exit LOOP_2
       endif
     enddo LOOP_2
   end subroutine
   !
   subroutine MEM_STAT_update
     ! Process Memory Update
     !=======================
     call memstat( TOT_MEM_kilobytes_MEMSTAT )
     if (TOT_MEM_kilobytes_MEMSTAT<0) then
       TOT_MEM_kilobytes_MEMSTAT=0
       return
     endif
   end subroutine
end subroutine

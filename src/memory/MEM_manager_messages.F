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
subroutine MEM_manager_messages(what,adding,MEM_now_Kb,where)
 !
 use pars,         ONLY:lchlen
 use com,          ONLY:msg,fat_log
 use memory,       ONLY:TOT_MEM_Kb,TOT_MEM_Kb_MEMSTAT,TOT_MEM_Kb_SAVE,MEM_treshold,&
&                       MEM_jump_treshold,mem_string,HOST_,DEV_
 !
 implicit none
 !
 character(*), intent(in) :: what
 logical,      intent(in) :: adding
 integer,      intent(in) :: MEM_now_Kb
 integer,      intent(in) :: where
 !
 ! Work space
 !
 logical           :: deliver_a_msg
 character(lchlen) :: msg_,label
 !
 label=" "
#if defined _CUDA
 if (where==HOST_) label=" (HOST)" 
 if (where==DEV_)  label=" (DEV)" 
#endif
 !
#if defined _MEM_CHECK
 !
 ! Messages
 !==========
 !#1
 deliver_a_msg=MEM_now_Kb>MEM_treshold
 if (fat_log) deliver_a_msg=MEM_now_Kb>MEM_treshold*10
 if (deliver_a_msg) then
   call MEM_STAT_update( )
   msg_="[MEMORY]"
   if (     adding) msg_=trim(msg_)//" Alloc "//what
   if (.not.adding) msg_=trim(msg_)//"  Free "//what
   msg_=trim(msg_)//"("//trim(mem_string(MEM_now_Kb))//")"
   msg_=trim(msg_)//trim(label)//" TOTAL: "//trim(mem_string(TOT_MEM_Kb(where)))//" (traced)"
   if (TOT_MEM_Kb_MEMSTAT(where)>0) then
     msg_=trim(msg_)//" "//trim(mem_string(TOT_MEM_Kb_MEMSTAT(where)))//" (memstat)"
   endif
   call msg("s",trim(msg_))
 endif
 !#2
 deliver_a_msg=int((TOT_MEM_Kb(where)-TOT_MEM_Kb_SAVE(where))/1000)>int(MEM_jump_treshold/10.).and.adding
 if (deliver_a_msg) then
   call MEM_STAT_update( )
   msg_="[MEMORY] In use: "
   msg_=trim(msg_)//trim(label)//" TOTAL: "//trim(mem_string(TOT_MEM_Kb(where)))//" (traced)"
   if (TOT_MEM_Kb_MEMSTAT(where)>0) then
     msg_=trim(msg_)//" "//trim(mem_string(TOT_MEM_Kb_MEMSTAT(where)))//" (memstat)"
   endif
   call msg("s",trim(msg_))
   TOT_MEM_Kb_SAVE(where)=TOT_MEM_Kb(where)
 endif
 !
#endif
 !
end subroutine MEM_manager_messages

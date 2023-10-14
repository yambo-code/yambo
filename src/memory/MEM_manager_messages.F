!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
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

!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine MEM_report( REPORT )
 !
 use pars,         ONLY:lchlen
 use memory,       ONLY:MEMs,N_MEM_elements,mem_string,MEM_element,MAX_MEM_Kb,N_MEM_SAVE_elements,&
&                       LARGE_MEMs,MEM_treshold,MEM_SAVE_treshold,HOST_,DEV_
 use units,        ONLY:Kilobyte
 use com,          ONLY:msg,fat_log
 !
 implicit none
 !
 character(*) :: REPORT
 !
 character(lchlen) :: msg_,PREFIX
 character(1)      :: loc
 integer :: i_mem,i_el,ORPHAN_TOT,i_title
 !
 if (REPORT=="check") then
   loc="s"
   PREFIX="[MEMORY]"
 else
   PREFIX=" "
   loc="r"
   call msg("r" ,"Memory Usage: global (Only MASTER cpu here). [O] stands for group 'O'")
   call msg("rn","Memory treshold are:"//trim(mem_string(MEM_treshold))//" (basic treshold) "//&
&                                       trim(mem_string(MEM_SAVE_treshold))//" (SAVEs treshold) ")
 endif
 !
 ORPHAN_TOT=0
 msg_=" " 
 if (N_MEM_elements>0) then
   !
   i_title=0
   do i_mem=1,N_MEM_elements
     !
     ORPHAN_TOT=ORPHAN_TOT+MEMs(i_mem)%use
     call plot_the_element(MEMs(i_mem),"Not deallocated components:")
     !
   enddo
   !
 endif
 !
 if (ORPHAN_TOT>0.and.fat_log) then
   if (loc=="r") call msg(loc,"")
   WRITE(msg_,'(a8,a," : ",a,3x)') trim(PREFIX),"TOTAL not deallocated",trim(mem_string(ORPHAN_TOT))
   call msg(loc,msg_(1:len_trim(msg_)))
 endif
 !
 if (REPORT=="check") return
 !
 if (N_MEM_SAVE_elements>0) then
   i_title=0
   do i_mem=1,N_MEM_SAVE_elements
     call plot_the_element(LARGE_MEMs(i_mem),"MAX allocations:")
   enddo
 endif
 !
#ifdef _CUDA
 write(msg_,'(a8,a," : ",a,3x)') trim(PREFIX),  "Max memory used (HOST) ",trim(mem_string(MAX_MEM_Kb(HOST_)))
 call msg("nr",msg_(1:len_trim(msg_)))
 write(msg_,'(a8,a," : ",a,3x)') trim(PREFIX),  "Max memory used (DEV)  ",trim(mem_string(MAX_MEM_Kb(DEV_)))
 call msg("r",msg_(1:len_trim(msg_)))
#else
 write(msg_,'(a8,a," : ",a,3x)') trim(PREFIX),  "Max memory used  ",trim(mem_string(MAX_MEM_Kb(HOST_)))
 call msg("nr",msg_(1:len_trim(msg_)))
#endif
 !
 contains
   !
   subroutine plot_the_element(M,what)
     type(MEM_element) :: M
     character(*)      :: what
     integer           :: MEM,MEM_now_kb
     integer           :: where
     !
     do i_el=1,M%N
       !
       MEM=M%size(i_el)*M%kind(i_el)
       MEM_now_kb=int(real(MEM)/Kilobyte)
       where=M%where(i_el)
       if (MEM_now_kb<MEM_treshold) return
       !
       if (REPORT/="check".and.i_title==0) call msg("nrn",what)
       i_title=1
       !
       if (REPORT=="check") then
         WRITE(msg_,'(a,1x,a," : ",a)') trim(PREFIX),trim(mem_string(MEM_now_kb,where))
         if (M%N>1.or.(M%N==1.and.trim(M%shelf)/=trim(M%name(1)))) then
           WRITE(msg_,'(2a)') trim(msg_),trim(M%name(i_el))//" "//trim(M%shelf)
         else
           WRITE(msg_,'(2a)') trim(msg_),trim(M%name(i_el))
         endif
         if (len_trim(M%desc(i_el))>0) then
           WRITE(msg_,'(2a)')  trim(msg_)," ("//trim(M%desc(i_el))//")"
         endif
       else
         if (M%N>1.or.(M%N==1.and.trim(M%shelf)/=trim(M%name(1)))) then
           WRITE(msg_,'(a8,a27)') trim(PREFIX),trim(M%name(i_el))//" "//trim(M%shelf)
         else
           WRITE(msg_,'(a8,a27)') trim(PREFIX),trim(M%name(i_el))
         endif
         WRITE(msg_,'(a," : ",a)') trim(msg_),trim(mem_string(MEM_now_kb,where))
         if (len_trim(M%desc(i_el))>0) then
           WRITE(msg_,'(a,a)')  trim(msg_)," ("//trim(M%desc(i_el))//")"
         endif
       endif
       !
       call msg(loc,msg_(1:len_trim(msg_)))
       !
     enddo
   end subroutine
   !
end subroutine

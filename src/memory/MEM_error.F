!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine MEM_error(what)
 !
 use memory,       ONLY:MEM_err,MEM_msg
 use LIVE_t,       ONLY:LIVE_message
 use stderr,       ONLY:intc
 !
 implicit none
 !
 character(*), intent(in) :: what
 !
 ! Error Message
 !===============
 if (MEM_err/=0) then
   call LIVE_message("[ERROR] Allocation of "//what//" failed with code "//trim(intc(MEM_err)))
   call error(trim(MEM_msg))
 endif
 !
end subroutine MEM_error

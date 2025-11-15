!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine MEM_global_mesg(what,xkind,HOST_SIZE)
 !
 use pars,         ONLY:SP,DP,IPL
 use parallel_m,   ONLY:host_name
 use stderr,       ONLY:real2ch
 use units,        ONLY:Kilobyte
 use LIVE_t,       ONLY:LIVE_message
 use y_memory,     ONLY:mem_string
 !
 implicit none
 !
 character(*), intent(in) :: what
 integer,      intent(in) :: xkind
 integer(IPL), intent(in) :: HOST_SIZE(:)
 !
 integer   :: i1
 real(SP)  :: MEM_size
 !
 MEM_size=real(HOST_SIZE(1),SP)
 do i1=2,size(HOST_SIZE)
   MEM_size=MEM_size*real(HOST_SIZE(i1),SP)
 enddo
 MEM_size=MEM_size*real(xkind,SP)/Kilobyte
 !
 ! Error Message
 !===============
 call LIVE_message("[PAR-MEMORY] Parallel distribution of "//what//" on HOST "//trim(host_name)//&
&                  " with total size "//trim(mem_string(MEM_size)))
 !
 call MEM_error("GLOBAL_"//what)
 !
end subroutine MEM_global_mesg

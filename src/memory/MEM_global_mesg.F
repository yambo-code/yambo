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
 !
 implicit none
 !
 character(*), intent(in) :: what
 integer,      intent(in) :: xkind
 integer(IPL), intent(in) :: HOST_SIZE(:)
 !
 integer   :: i1
 real(DP)  :: MEM_size
 !
 MEM_size=0
 do i1=1,size(HOST_SIZE)
   MEM_size=MEM_size+real(HOST_SIZE(i1),DP)
 enddo
 MEM_size=MEM_size*real(xkind,DP)/real(Kilobyte,DP)/real(1000,DP)
 !
 ! Error Message
 !===============
 call LIVE_message("[MEMORY] Parallel distribution of "//what//" on HOST "//trim(host_name)//&
&                  " with size "//trim(real2ch(real(MEM_size,SP)))//" [Mb] ")
 !
 ! 2020/05/20: Here I do not use mem_string since it wants an integer in input
 !             It is not a good solution since integers are limited to their maximum value
 !             A real would be much better for the size in Kb ...
!&                  " with size "//trim(mem_string(real(MEM_size,SP))))
 !
 call MEM_error("GLOBAL_"//what)
 !
end subroutine MEM_global_mesg

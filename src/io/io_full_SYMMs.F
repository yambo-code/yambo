!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
integer function io_full_SYMMs(ID)
 !
 use pars,          ONLY:schlen,SP,IP
 !
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
#include<y_memory.h>
 !
 integer,      intent(in) :: ID
 !
 ! Work Space
 !
#if defined _TIMING
 call timing('io_full_SYMMs',OPR='start')
#endif
 !
 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_full_SYMMs',OPR='stop')
#endif
 !
end function

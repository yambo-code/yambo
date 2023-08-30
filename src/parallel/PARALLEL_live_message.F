!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_live_message(WHAT,ENVIRONMENT,LOADED,TOTAL,LOADED_r,TOTAL_r,NC)
 !
 use pars,       ONLY:lchlen,SP
 use parallel_m, ONLY:ncpu
 use LIVE_t,     ONLY:LIVE_message
 use stderr,     ONLY:intc,real2ch
 !
 implicit none
 !
 character(*)               :: WHAT
 character(*),optional      :: ENVIRONMENT
 integer,optional           :: LOADED,TOTAL,NC
 real(SP),optional          :: LOADED_r,TOTAL_r
 !
 ! Work Space
 integer               :: PERCENTUAL
 character(lchlen)     :: local_message,local_env
 !
 local_message=WHAT
 !
 if (ncpu==1) return
 !
 local_env="distribution"
 if (present(ENVIRONMENT)) local_env=ENVIRONMENT
 !
 if(present(LOADED).or.present(LOADED_r)) then
   !
   local_message="[PARALLEL "//trim(local_env)//" for "//WHAT
   if (     present(NC)) local_message=trim(local_message)//" on "//trim(intc(NC))//" CPU] Loaded/Total (Percentual): "
   if (.not.present(NC)) local_message=trim(local_message)//"] Loaded/Total(Percentual): "
   !
   if(present(LOADED_r).and.present(TOTAL_r)) then
     PERCENTUAL=nint(LOADED_r/TOTAL_r*100._SP)
     local_message=trim(local_message)//trim(real2ch(LOADED_r))//"/"//trim(real2ch(TOTAL_r))//" ("//trim(intc(PERCENTUAL))//"%)"
   else if(present(LOADED).and.present(TOTAL)) then
     PERCENTUAL=nint(real(LOADED,SP)/real(TOTAL,SP)*100._SP)
     local_message=trim(local_message)//trim(intc(LOADED))//"/"//trim(intc(TOTAL))//"("//trim(intc(PERCENTUAL))//"%)"
   endif
   !
 else if(present(NC)) then
   !
   local_message="[PARALLEL "//trim(local_env)//" for "//WHAT//" on "//trim(intc(NC))//" CPU]"
 else
   !
   return
   !
 endif
 !
 call LIVE_message(message=trim(local_message),lfmt="n")
 !
end subroutine

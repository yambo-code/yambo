!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!                  
!> @brief Write/Check/Read restart point in time 
!!
!! @param[in]       what                        what="write | check| load "
!! @param[in]       en                          electronic levels
!! @param[in]       i_f                         run index
!!
!! @param[out]      io_NL_WF                    write/read WF on disk together with polarization and current
!!
subroutine NL_restart(what,en,i_f)
 !
 use pars,          ONLY:cONE
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:REP,OP_WR_CL,VERIFY,OP_APP_CL,OP_RD_CL,DUMP 
 use nl_optics,     ONLY:ID,NE_steps_done,V_bands,NE_i_restart
 use electrons,     ONLY:levels,n_sp_pol
 use stderr,        ONLY:intc,STRING_match
 !
 implicit none
 !
 integer, intent(in)      :: i_f
 type(levels), intent(in) :: en
 character(*), intent(in) :: what
 !
 ! Working Space
 !
 integer, external    :: io_NL_WF
 integer              :: io_err,i_sp_pol,i1
 logical              :: checking,loading,writing
 integer              :: OP
 !
 checking  =STRING_match(what,"check")
 writing   =STRING_match(what,"write")
 loading   =STRING_match(what,"load")
 !
 NE_steps_done=0
 !
 ! No restart for NE_i_time < 0
 !
 if(NE_i_restart<0.0) return 
 !
 if(.not.checking.and..not.writing.and..not.loading) call error("Wrong call to restart")
 !
 if(writing) then
   OP=OP_WR_CL
   call io_control(ACTION=OP,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID(2))
   io_err=io_NL_WF(en,i_f,ID(2))
 else
   OP=OP_RD_CL
   if(checking) then
     call io_control(ACTION=OP,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID(2))
   elseif(loading) then
     call io_control(ACTION=OP,COM=REP,SEC=(/1,2/),MODE=DUMP,ID=ID(2))
   endif
   io_err=io_NL_WF(en,i_f,ID(2))
 endif
 !
end subroutine NL_restart

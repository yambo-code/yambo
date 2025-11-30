!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!                  
subroutine NL_databases_IO(i_f,read_or_write)
 !
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:REP,OP_WR_CL,VERIFY,OP_APP_CL,OP_RD_CL         
 use nl_optics,     ONLY:ID,NL_P_t
 use parallel_m,    ONLY:master_cpu
 !
 implicit none
 !
 integer, intent(in)      :: i_f
 character(5), intent(in) :: read_or_write
 !
 ! Working Space
 !
 integer              :: io_Nonlinear,IO_ACTION
 integer, external    :: io_NL
 character(9)         :: dbname
 logical              :: reading,writing
 !
 reading=trim(read_or_write)=="read"
 writing=trim(read_or_write)=="write"
 !
 dbname="Nonlinear"
 !    
 if(i_f==0) then
   if(master_cpu) then
     !
     if(reading) IO_ACTION=OP_RD_CL
     if(writing) IO_ACTION=OP_WR_CL
     !
     call io_control(ACTION=IO_ACTION,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID(1))
     io_Nonlinear=io_NL(dbname,ID(1))
   endif
 else
   !         
   if(reading) IO_ACTION=OP_RD_CL
   if(writing) IO_ACTION=OP_APP_CL
   !
   call io_control(ACTION=IO_ACTION,COM=REP,SEC=(/i_f+1/),MODE=VERIFY,ID=ID(1))
   io_Nonlinear=io_NL(dbname,ID(1))
   !
 endif
 !
end subroutine NL_databases_IO

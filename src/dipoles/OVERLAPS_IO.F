!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine OVERLAPS_IO(Xk,Xen,Dip,check_read_or_write,io_err,PAR_K_scheme_kind)
 !
 use pars,        ONLY:SP,cI,cZERO
 use parser_m,    ONLY:parser
 use stderr,      ONLY:intc
 use com,         ONLY:msg
 use IO_m,        ONLY:io_DIP
 use R_lattice,   ONLY:bz_samp
 use electrons,   ONLY:levels,n_sp_pol
 use DIPOLES,     ONLY:DIPOLE_t,covariant_obs,der_k_obs
 use IO_int,      ONLY:io_control
 use IO_m,        ONLY:manage_action,VERIFY,DUMP,REP,OP_WR_CL,OP_WR,OP_RD_CL,&
&                      OP_IF_START_APP_CL_IF_END,OP_IF_START_RD_CL_IF_END
 use parallel_int,ONLY:PP_wait
 use parallel_m,  ONLY:master_cpu,PAR_K_scheme,PP_indexes,PP_indexes_reset
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(DIPOLE_t),intent(inout) :: Dip
 character(*),  intent(in)    :: check_read_or_write
 character(*),  intent(in)    :: PAR_K_scheme_kind
 integer,       intent(out)   :: io_err
 !
 ! Work space
 !
 logical           :: write_header,read_header,check,reading,writing,IO_do_it
 integer           :: ID_S,IO_ACTION,IO_ACT_NOW,IO_MODE,ik_mem,ik,i_sp_pol,i_fragment,iv,ic,max_count,i_count
 integer, external :: io_Overlaps
#if !defined _PAR_IO
 type(PP_indexes)  :: IO_par_index
#endif
 !
 if (.not.IO_dip) return
 !
 check  =trim(check_read_or_write)=="check"
 reading=trim(check_read_or_write)=="read"
 writing=trim(check_read_or_write)=="write"
 !
 if(.not.any((/check,reading,writing/))) call error("Wrong call to DIPOLES_IO")
 !
 ID_S=0
 io_err=0
 !
 write_header  =  (master_cpu.and.writing)
 read_header   =  .not.write_header 
 !
 if(write_header) then
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID_S)
   io_err=io_Overlaps(Dip,ID_S)
 endif
 !
 ! To fix problems with asynchronous I/O 
 !
 call PP_wait(COMM=PAR_K_scheme%comm_world)
 !
 IO_MODE=VERIFY
 if(read_header.and.writing) IO_MODE=DUMP
 !
 if(read_header) then
   !
   io_err=-1
   i_count=0
   !
   ! Wait until the ndb.dipoles file is available to all cores
   ! In writing mode the loop is needed to fix for asynchronous I/O
   !
   if(writing)          max_count=120
   if(reading.or.check) max_count=1
   !
   do while(io_err/=0)
     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=IO_MODE,ID=ID_S)
     io_err=io_Overlaps(Dip,ID_S)
     if(io_err/=0) call sleep(1)
     i_count=i_count+1
     if((i_count==max_count).and.io_err/=0) then
       if(writing) call error(" [OVLPs] Header not found while writing overlaps ")
       if(reading) call error(" [OVLPs] Header not correct or missing while reading overlaps ")
       if(  check) call msg("s","[OVLPs] Overlaps not correct or missing. To be computed")
       return
     endif
   enddo
 endif
 !
 if (check)   return
 if (writing) call PP_wait(COMM=PAR_K_scheme%comm_world)
 !
 if(reading) IO_ACTION=OP_IF_START_RD_CL_IF_END
 if(writing) IO_ACTION=OP_IF_START_APP_CL_IF_END
 !
 IO_do_it = reading .or. (writing.and.PAR_K_scheme%COM_ibz_A2A%CPU_id==0)
 !
 ! DIPOLES I/O
 !==============
 !
#if !defined _PAR_IO
 ! IO parallel indexes needed below by manage_action
 !---------------------
 call PP_indexes_reset(IO_par_index)
 allocate(IO_par_index%element_1D(n_sp_pol*Xk%nibz))
 IO_par_index%element_1D=.FALSE.
 do i_sp_pol=1,n_sp_pol
   do ik=1,Xk%nibz
     i_fragment=ik+(i_sp_pol-1)*Xk%nibz
     if ( .not.PAR_K_scheme%IND_ibz%element_1D(ik)) cycle
     IO_par_index%element_1D(i_fragment)=.TRUE.
   enddo
 enddo
#endif
 !
#if defined _PAR_IO
 IO_ACT_NOW=manage_action(IO_ACTION,0,0,2)
 call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/0/),ID=ID_S,COMM=PAR_K_scheme%COM_ibz_index,DO_IT=IO_do_it)
 io_err=io_Overlaps(Dip,ID_S)
 IO_ACT_NOW=manage_action(IO_ACTION,1,0,2)
#endif
 do i_sp_pol=1,n_sp_pol
   do ik=1,Xk%nbz
     i_fragment=ik+(i_sp_pol-1)*Xk%nbz
     if (.not.PAR_K_scheme%IND_bz%element_1D(ik).and.writing) cycle
#if !defined _PAR_IO
     if (.not.PAR_K_scheme%COM_ibz_A2A%CPU_id==0 .and.writing) cycle
     if (writing) IO_ACT_NOW=manage_action(IO_ACTION,i_fragment,1,Xk%nbz*n_sp_pol,IO_par_index)
     if (reading) IO_ACT_NOW=manage_action(IO_ACTION,i_fragment,1,Xk%nbz*n_sp_pol)
#endif
     call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/1+i_fragment/),ID=ID_S)
     io_err=io_Overlaps(Dip,ID_S)
     if (io_err/=0.and.IO_do_it) then
       call error(" OVERLAPS I/O error when (i_sp_pol,ik)= ("//trim(intc(i_sp_pol))//","//trim(intc(ik))//")")
     endif
   enddo
 enddo
#if defined _PAR_IO
 IO_ACT_NOW=manage_action(IO_ACTION,2,0,2)
 call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/2+Xk%nbz*n_sp_pol/),ID=ID_S)
 io_err=io_Overlaps(Dip,ID_S)
#endif
 !
 call PP_wait(COMM=PAR_K_scheme%comm_world)
 !
#if !defined _PAR_IO
 call PP_indexes_reset(IO_par_index)
#endif
 !
end subroutine OVERLAPS_IO

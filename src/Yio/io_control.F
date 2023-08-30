!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine io_control(ACTION,MODE,COM,SEC,ID,COMM,DO_IT)
 !
 ! Handle different kind of I/O accesses:
 !
 ! a) DO_IT=.TRUE. => IO PAR COMM = WORLD
 !                    IO PAR CPU  = DO_IT
 !
 ! b) DO_IT+COMM => IO PAR COMM = COMM
 !                  IO PAR CPU  = DO_IT
 !
 ! c) ( ) + ( ) => IO PAR COMM = null
 !                 IO PAR CPU  = IO_read/IO_write defaults
 !
 use parallel_int,  ONLY:PP_bcast,PP_redux_wait
 use parallel_m,    ONLY:master_cpu,mpi_comm_null,yMPI_comm,mpi_comm_world,ncpu,myid
 use IO_m,          ONLY:io_unit,io_mode,io_com,io_action,io_sec,max_io_units, &
&                        RD,WR,RD_CL,WR_CL,OP_APP,OP_APP_CL,OP_WR,OP_RD,OP_WR_CL,OP_RD_CL,&
&                        io_reset,io_PAR_comm,io_PAR_cpu,IO_read,IO_write,&
&                        read_is_on,write_is_on
 !
 implicit none
 !
 integer,intent(in)                   :: ACTION
 integer,intent(inout)                :: ID
 integer,optional,intent(in)          :: MODE,COM,SEC(:)
 type(yMPI_comm),optional,intent(in)  :: COMM
 logical,optional,intent(in)          :: DO_IT
 !
 ! Work Space
 !
 integer :: i1,local_COMM,my_max_ID
 logical :: define_ID_RD,define_ID_WR,HEAD_cpu
 !
 if ( .not. any( (/ ACTION==RD_CL, ACTION==OP_RD_CL, ACTION==OP_WR_CL,   &
&                   ACTION==OP_APP_CL, ACTION==OP_RD, ACTION==OP_APP,    &
&                   ACTION==OP_WR, ACTION==RD, ACTION==WR,ACTION==WR_CL/) ) ) &
&  call error("io_control called with a non proper action")
 !
 ! Assign a new ID if the unit is not already open
 !
 define_ID_WR=any((/ACTION==OP_WR_CL, ACTION==OP_WR, ACTION==OP_APP_CL, ACTION==OP_APP/))
 define_ID_RD=any((/ACTION==OP_RD_CL, ACTION==OP_RD/))
 !
 ! Except for the cases where the ID is defined, io_control is a trivial subrtouine
 !
 if (.not. (define_ID_RD .or. define_ID_WR) ) then
   call set_action_mode_com_and_sec()
   return
 endif
 !
 ! Define the HEAD cpu
 !
 HEAD_cpu=.TRUE.
#if defined _PAR_IO
 if (present(COMM)) then
   HEAD_cpu=COMM%CPU_id==0
   local_COMM=COMM%COMM
 else if (present(DO_it)) then
   HEAD_cpu=master_cpu
   local_COMM=mpi_comm_world
 endif
#endif
 !
 ID=0
 do i1=1,max_io_units
   my_max_ID=i1-1
   if ( io_unit(i1)/=0 ) cycle
   if ( .not. (define_ID_RD .or. HEAD_cpu) ) exit
   ID=i1
   call io_reset(ID)
   io_unit(ID)=40+i1  ! This is not used with netcdf since nf90_create assigns the unit
   exit
 enddo
 if ( (define_ID_RD .or. HEAD_cpu) .and. ID==0 ) call error(" Recompile Yambo with a larger: max_io_units")
 !
 ! Define the parallel COMM needed to perform parallel I/O
 !
 if(ID>0) io_PAR_comm(ID)=mpi_comm_null
#if defined _PAR_IO
 if (ncpu>1.and.define_ID_WR.and.(present(COMM).or.present(DO_IT))) then
   ! DEBUG <
   !write(*,*) myid,"before redux ",ID,my_max_ID,HEAD_cpu,local_COMM,mpi_comm_world
   ! DEBUG >
   call PP_bcast(ID,0,COMM=local_COMM)
   call PP_bcast(io_unit(ID),0,local_COMM)
   io_PAR_comm(ID)=local_COMM
   ! DEBUG <
   !write(*,*) myid,"after redux ",ID,my_max_ID,io_PAR_comm(ID)
   ! DEBUG >
 endif
#endif
 !
 ! Define the processors allowed to do the specific action
 !
 io_PAR_cpu(ID)=0
 if (define_ID_RD.and.IO_read ) io_PAR_cpu(ID)=1
 if (define_ID_WR.and.IO_write) io_PAR_cpu(ID)=1
#if defined _PAR_IO
 if (present(DO_IT).and.define_ID_WR) then
   io_PAR_cpu(ID)=0
   if (DO_IT) io_PAR_cpu(ID)=1
 endif
#endif
 !
 call set_action_mode_com_and_sec()
 !
 contains
   !
   subroutine set_action_mode_com_and_sec()
     !
     io_action(ID)=ACTION
     if (present(MODE)) io_mode(ID)=MODE
     if (present(COM))  io_com(ID) =COM
     if (present(SEC)) then
       io_sec(ID,:)=0
       io_sec(ID,:size(SEC))=SEC
     endif
     !
   end subroutine set_action_mode_com_and_sec
   !
end subroutine

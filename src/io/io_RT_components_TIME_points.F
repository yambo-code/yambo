!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
#if defined _RT
subroutine io_RT_components_TIME_points(ID,VAR_SZ,load_RESTART_info,UNLIMITED_time,TYP)
 !
 use pars,           ONLY:IP,SP,schlen
 use drivers,        ONLY:l_real_time
 use units,          ONLY:AUT2FS
 use RT_output_m,    ONLY:RT_desc
 use RT_control,     ONLY:RT_IO_t,RT_do_IO
 use netcdf,         ONLY:NF90_UNLIMITED
 use real_time,      ONLY:MEM_pointer,G_MEM_steps,NE_i_time,i_MEM_now,i_MEM_prev,i_MEM_old,&
&                         RT_dyn_step,RT_step
 use IO_m,           ONLY:write_is_on,read_is_on
 use IO_int,         ONLY:def_variable_bulk,io_variable_bulk,def_variable_elemental,io_variable_elemental
 !
#include<memory.h>
 !
 integer,       intent(in)    :: ID,VAR_SZ
 logical,       intent(in)    :: load_RESTART_info,UNLIMITED_time
 type(RT_IO_t), intent(inout) :: TYP
 !
 integer                      :: IPOS(1),IO_LIM(2),i_min_index,i_mem
 character(schlen)            :: dim_name(1)
 !
 if (write_is_on(ID)) TYP%last_point=NE_i_time
 !
 ! ... for RESTART purposes...
 if (load_RESTART_info) then
   call def_variable_elemental(ID,"IO_TIMEs",2,SP,0) 
   call io_variable_elemental(ID,VAR=" [RT] I/O time          (input) [fs]",&
                        R0=TYP%INTERVAL_time_INPUT,CHECK=.TRUE.,OP=(/"=="/),  &
&                       DESCRIPTOR=RT_desc,UNIT=AUT2FS)
   call io_variable_elemental(ID,VAR=" [RT] I/O time            (now) [fs]",&
                        R0=TYP%INTERVAL_time     ,CHECK=.FALSE.,             &
&                       DESCRIPTOR=RT_desc,UNIT=AUT2FS)
 endif
 !
 call def_variable_elemental(ID,"IO_TIME_steps_last_nsteps",3,IP+100,0) 
 call io_variable_elemental(ID,I0=TYP%INTERVAL_steps)
 call io_variable_elemental(ID,I0=TYP%last_point)
 call io_variable_elemental(ID,I0=TYP%N)
 !
 if (RT_do_IO("G_lesser_RESTART").and. l_real_time) then
   dim_name="G_MEM_steps"
   call def_variable_bulk(ID,"MEMORY_TIME_POINTS",2,(/G_MEM_steps/),IP+100,dim_name,silent=.true.)
   call io_variable_bulk(ID,2,I1=MEM_pointer)
   if (read_is_on(ID)) then
     i_min_index=minval(MEM_pointer)
     do i_mem=1,G_MEM_steps
       if (MEM_pointer(i_mem)==NE_i_time  )                         i_MEM_now =i_mem
       if (MEM_pointer(i_mem)==NE_i_time-nint(RT_dyn_step/RT_step)) i_MEM_prev=i_mem
       if (G_MEM_steps==2) cycle
       if (MEM_pointer(i_mem)==i_min_index)                         i_MEM_old =i_mem
     enddo
   endif
 endif
 !
 if (.not.allocated(TYP%Time)) then
   YAMBO_ALLOC(TYP%Time,(TYP%N))
 endif
 !
 IPOS=TYP%N-VAR_SZ+1
 IO_LIM=(/TYP%N-VAR_SZ+1,TYP%N/)
 if(TYP%first_IO(2).or.RT_do_IO("ANY_G").or.read_is_on(ID)) then
   dim_name="IO_TIME_n_points"
   if (UNLIMITED_time) then
     call def_variable_bulk(ID,"IO_TIME_points",3,(/NF90_UNLIMITED/),SP,dim_name,silent=.true.,par_io_kind="collective")
   else
     call def_variable_bulk(ID,"IO_TIME_points",3,(/G_MEM_steps/),   SP,dim_name,silent=.true.,par_io_kind="collective")
   endif
 endif
 !
 if(write_is_on(ID)) call io_variable_bulk(ID,3,R1=TYP%Time(IO_LIM(1):IO_LIM(2)),IPOS=IPOS)
 if( read_is_on(ID)) call io_variable_bulk(ID,3,R1=TYP%Time(:TYP%N))
 !
 if (write_is_on(ID)) TYP%first_IO(2)=.FALSE.
 !
end subroutine io_RT_components_TIME_points
#endif

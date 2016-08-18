!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine DIPOLE_IO(Xk,X,check_read_or_write,io_err)
 !
 use com,        ONLY:msg
 use IO_m,       ONLY:io_DIP
 use R_lattice,  ONLY:bz_samp
 use electrons,  ONLY:n_sp_pol
 use X_m,        ONLY:X_t
 use IO_m,       ONLY:io_control,OP_RD_CL,VERIFY,REP,OP_WR_CL,OP_APP_WR_CL,OP_RD
 use parallel_m, ONLY:master_cpu,PAR_COM_DIPk_ibz_INDEX,PAR_COM_DIPk_ibz_A2A,ncpu,&
&                     PAR_IND_DIPk_ibz,PP_redux_wait
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk
 type(X_t),     intent(inout) :: X
 character(5),  intent(in)    :: check_read_or_write
 integer,       intent(out)   :: io_err
 !
 ! Work space
 !
 logical           :: write_header,read_header,check,reading,writing,l_vnl(1)
 integer           :: ID,IO_ACTION,ik,i_sp_pol
 integer, external :: io_DIPOLES
 !
 if (.not.IO_dip) return 
 !
 !
 check  =trim(check_read_or_write)=="check"
 reading=trim(check_read_or_write)=="read"
 writing=trim(check_read_or_write)=="write"
 !
 ID=0
 write_header  =(     master_cpu.and.writing)
 read_header   =(.not.master_cpu.and.writing).or.reading.or.check
 !
 ! AM March 2016. The next lines are in the nasty case where only a fraction of all CPU's compute the
 ! dipoles. In this case only those cpu's know the right value of X%Vnl_included that is defined in the
 ! calculation process. If, in the same run, all cpu's must access the dipoles the other CPU's will have
 ! the wrong value of X%Vnl_included and the code will get stuck.
 !
 if ((reading.or.check).and.PAR_COM_DIPk_ibz_INDEX%n_CPU==ncpu)then
   l_vnl=X%Vnl_included
   call PP_redux_wait(l_vnl)
   X%Vnl_included=l_vnl(1)
 endif
 !
 if(write_header) then
   call msg('s','[DIP] Writing dipoles header ')
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_err=io_DIPOLES(X,ID)
 endif
 !
 call PP_redux_wait(COMM=PAR_COM_DIPk_ibz_INDEX%COMM)
 !
 if(read_header) then
   call msg('s','[DIP] Checking dipoles header ')
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_err=io_DIPOLES(X,ID)
 endif
 !
 if (check) return
 !
 if (io_err==0) then
   !
   if(reading) IO_ACTION=OP_RD_CL
   if(writing) IO_ACTION=OP_APP_WR_CL
   !
   do ik=1,Xk%nibz
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)        ) cycle
     if (.not.PAR_COM_DIPk_ibz_A2A%CPU_id==0 .and. writing) cycle
     do i_sp_pol=1,n_sp_pol
       call io_control(ACTION=IO_ACTION,COM=REP,SEC=(/1+ik+(i_sp_pol-1)*Xk%nibz/),ID=ID)
       io_err=io_DIPOLES(X,ID)
    enddo
   enddo
   !
 endif
 !
end subroutine DIPOLE_IO

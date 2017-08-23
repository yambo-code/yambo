!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine DIPOLE_IO(Xk,Xen,Dip,check_read_or_write,io_err)
 !
 use pars,       ONLY:SP,cI,cZERO
 use com,        ONLY:msg
 use IO_m,       ONLY:io_DIP
 use R_lattice,  ONLY:bz_samp
 use electrons,  ONLY:levels,n_sp_pol
 use DIPOLES,    ONLY:DIPOLE_t,DIP_P,P_square
 use IO_m,       ONLY:io_control,OP_RD_CL,VERIFY,REP,OP_WR_CL,OP_APP_WR_CL,OP_RD
 use parallel_m, ONLY:master_cpu,PAR_COM_DIPk_ibz_INDEX,PAR_COM_DIPk_ibz_A2A,ncpu,&
&                     PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index,PP_redux_wait
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(DIPOLE_t),intent(inout) :: Dip
 character(5),  intent(in)    :: check_read_or_write
 integer,       intent(out)   :: io_err
 !
 ! Work space
 !
 logical           :: write_header,read_header,check,reading,writing,l_vnl(1)
 integer           :: ID,IO_ACTION,ik_mem,ik,i_sp_pol,iv,ic
 integer, external :: io_DIPOLES
 real(SP)          :: Dipole_Energy_treshold,Ev_m_Ec,Ev_m_Ec_QP,W_
 !
 if (.not.IO_dip) return
 !
 check  =trim(check_read_or_write)=="check"
 reading=trim(check_read_or_write)=="read"
 writing=trim(check_read_or_write)=="write"
 !
 ID=0
 write_header  =(     master_cpu.and.writing)
 read_header   =(.not.master_cpu.and.writing).or.reading.or.check
 !
 if ((reading.or.check).and.PAR_COM_DIPk_ibz_INDEX%n_CPU==ncpu) then
   l_vnl=Dip%Vnl_included
   call PP_redux_wait(l_vnl)
   Dip%Vnl_included=l_vnl(1)
 endif
 !
 if(write_header) then
   call msg('s','[DIP] Writing dipoles header ')
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_err=io_DIPOLES(Dip,ID)
 endif
 !
 call PP_redux_wait(COMM=PAR_COM_DIPk_ibz_INDEX%COMM)
 !
 if(read_header) then
   call msg('s','[DIP] Checking dipoles header ')
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_err=io_DIPOLES(Dip,ID)
 endif
 !
 if (check.or.io_err/=0) return
 !
 if(reading) IO_ACTION=OP_RD_CL
 if(writing) IO_ACTION=OP_APP_WR_CL
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,Xk%nibz
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)        ) cycle
     if (.not.PAR_COM_DIPk_ibz_A2A%CPU_id==0 .and. writing) cycle
     call io_control(ACTION=IO_ACTION,COM=REP,SEC=(/1+ik+(i_sp_pol-1)*Xk%nibz/),ID=ID)
     io_err=io_DIPOLES(Dip,ID)
   enddo
 enddo
 !
 !
 if(.not.(allocated(Xen%W).or.allocated(Xen%Eo)).or.writing) return
 !
 ! Now I need to correct DIP_P and P_square in case I'll use it
 ! See for this PRB 48, 11789 (1993), definition of p_eff
 !
 W_=0._SP
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,Xk%nibz
     !
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
     ik_mem=PAR_DIPk_ibz_index(ik)
     !
     do iv=Dip%ib(1),Dip%ib_lim(1) 
       do ic=Dip%ib_lim(2),Dip%ib(2)
         !
         if (allocated(Xen%W )) W_=abs(Xen%W(iv,ik,i_sp_pol))+abs(Xen%W(ic,ik,i_sp_pol))
         !
         Ev_m_Ec   =Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
         Ev_m_Ec_QP=Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)+cI*W_
         !
         if (allocated(Xen%Eo)) Ev_m_Ec=Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)
         !
         if (abs(Ev_m_Ec)<=Dipole_Energy_treshold) DIP_P(:,ic,iv,ik_mem,i_sp_pol)=cZERO
         if (abs(Ev_m_Ec)> Dipole_Energy_treshold) DIP_P(:,ic,iv,ik_mem,i_sp_pol)= &
&                                                        DIP_P(:,ic,iv,ik_mem,i_sp_pol)*Ev_m_Ec_QP/Ev_m_Ec
         !
         if (allocated(P_square)) then
           if (abs(Ev_m_Ec)<=Dipole_Energy_treshold) P_square(ic,iv,ik_mem,i_sp_pol)=cZERO
           if (abs(Ev_m_Ec)> Dipole_Energy_treshold) P_square(ic,iv,ik_mem,i_sp_pol)=&
&                                                      P_square(ic,iv,ik_mem,i_sp_pol)*(Ev_m_Ec_QP/Ev_m_Ec)**2
         endif
         !
       enddo
     enddo
     !
   enddo
 enddo
 !
end subroutine DIPOLE_IO

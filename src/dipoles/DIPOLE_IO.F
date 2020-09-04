!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine DIPOLE_IO(Xk,Xen,Dip,check_read_or_write,io_err,PAR_K_scheme_kind)
 !
 use pars,       ONLY:SP,cI,cZERO
 use com,        ONLY:msg
 use IO_m,       ONLY:io_DIP
 use R_lattice,  ONLY:bz_samp
 use electrons,  ONLY:levels,n_sp_pol
 use DIPOLES,    ONLY:DIPOLE_t,DIP_v,use_covariant_approach
 use IO_m,       ONLY:io_control,manage_action,VERIFY,REP,OP_WR_CL,OP_WR,OP_RD_CL,&
&                     OP_IF_START_APP_CL_IF_END,OP_IF_START_RD_CL_IF_END
 use parallel_m, ONLY:master_cpu,PP_redux_wait,PAR_K_scheme,PP_bcast
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
 logical           :: write_header,     read_header,     check,reading,writing,&
&                     write_header_ovlp,read_header_ovlp
 integer           :: ID,ID_S,IO_ACTION,IO_ACT_NOW,ik_mem,ik,i_sp_pol,i_fragment,iv,ic
 integer, external :: io_DIPOLES
 integer, external :: io_Overlaps
 !
 if (.not.IO_dip) return
 !
 check  =trim(check_read_or_write)=="check"
 reading=trim(check_read_or_write)=="read"
 writing=trim(check_read_or_write)=="write"
 !
 if(.not.any((/check,reading,writing/))) call error("Wrong call to DIPOLES_IO")
 !
 call SETUP_PAR_K_scheme()
 !
 ID=0
 ID_S=0
 io_err=0
 !
#if defined _PAR_IO
 write_header  =       PAR_K_scheme%COM_ibz_A2A%CPU_id==0.and.writing
 read_header   =(.not.(PAR_K_scheme%COM_ibz_A2A%CPU_id==0.and.writing)).or.reading.or.check
#else
 write_header  =(      master_cpu.and.writing )
 read_header   =.not.writing.and.(reading.or.check)
#endif
 write_header_ovlp  =write_header.and.use_covariant_approach
 read_header_ovlp   = read_header.and.use_covariant_approach
 !
 if(write_header) then
   call msg('s','[DIP] Writing dipoles header ')
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_err=io_DIPOLES(Dip,ID)
 endif
 if(write_header_ovlp) then
   call msg('s','[DIP] Writing overlaps header ')
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID_S)
   io_err=io_err+io_Overlaps(Dip,ID_S)
 endif
 !
 ! Master node bcast matrix dimensions for the other cores
 !
 if(writing) then
   call PP_bcast(Dip%ib,0)
   call PP_bcast(Dip%db_ib,0)
   call PP_bcast(Dip%ib_lim,0)
   call PP_bcast(Dip%db_ib_lim,0) 
   call PP_bcast(Dip%db_ib_lim,0) 
 endif
 !
 call PP_redux_wait(COMM=PAR_K_scheme%comm_world)
 !
 if(read_header) then
   call msg('s','[DIP] Checking dipoles header ')
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_err=io_DIPOLES(Dip,ID)
 endif
 if(read_header_ovlp) then
   call msg('s','[DIP] Checking overlaps header ')
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID_S)
   io_err=io_err+io_Overlaps(Dip,ID_S)
 endif
 !
 if (check.or.io_err/=0) then
   if (io_err/=0.and.     check) call warning(" DIPOLES database not correct or missing")
   if (io_err/=0.and..not.check) call   error(" DIPOLES database not correct or missing")
   return
 endif
 !
 if(writing) call PP_redux_wait(COMM=PAR_K_scheme%comm_world)
 !
 if(writing) call fix_velocity(Dip,-1)
 !
 if(reading) IO_ACTION=OP_IF_START_RD_CL_IF_END
 if(writing) IO_ACTION=OP_IF_START_APP_CL_IF_END
 !
#if defined _PAR_IO
 IO_ACT_NOW=manage_action(IO_ACTION,0,0,2)
 call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/0/),ID=ID)
 io_err=io_DIPOLES(Dip,ID)
 IO_ACT_NOW=manage_action(IO_ACTION,1,0,2)
#endif
 do i_sp_pol=1,n_sp_pol
   do ik=1,Xk%nibz
     i_fragment=ik+(i_sp_pol-1)*Xk%nibz
     if ( .not.PAR_K_scheme%IND_ibz%element_1D(ik)             ) cycle
     if ((.not.PAR_K_scheme%COM_ibz_A2A%CPU_id==0).and.writing ) cycle
#if !defined _PAR_IO
     IO_ACT_NOW=manage_action(IO_ACTION,i_fragment,1,Xk%nibz*n_sp_pol)
#endif
     call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/1+i_fragment/),ID=ID)
     io_err=io_DIPOLES(Dip,ID)
   enddo
 enddo
#if defined _PAR_IO
 IO_ACT_NOW=manage_action(IO_ACTION,2,0,2)
 call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/2+Xk%nibz*n_sp_pol/),ID=ID)
 io_err=io_DIPOLES(Dip,ID)
#endif
 !
 if(use_covariant_approach) then
#if defined _PAR_IO
   IO_ACT_NOW=manage_action(IO_ACTION,0,0,2)
   call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/0/),ID=ID_S)
   io_err=io_Overlaps(Dip,ID_S)
   IO_ACT_NOW=manage_action(IO_ACTION,1,0,2)
#endif
   do i_sp_pol=1,n_sp_pol
     do ik=1,Xk%nbz
       i_fragment=ik+(i_sp_pol-1)*Xk%nibz
       if ((.not.PAR_K_scheme%IND_bz%element_1D(ik)).and.writing) cycle
       if ((.not.PAR_K_scheme%COM_ibz_A2A%CPU_id==0).and.writing) cycle
#if !defined _PAR_IO
       IO_ACT_NOW=manage_action(IO_ACTION,i_fragment,1,Xk%nibz*n_sp_pol)
#endif
       call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/1+i_fragment/),ID=ID_S)
       io_err=io_err+io_Overlaps(Dip,ID_S)
     enddo
   enddo
#if defined _PAR_IO
   IO_ACT_NOW=manage_action(IO_ACTION,2,0,2)
   call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/2+Xk%nibz*n_sp_pol/),ID=ID_S)
   io_err=io_Overlaps(Dip,ID_S)
#endif
 endif
 !
 call PP_redux_wait(COMM=PAR_K_scheme%comm_world)
 !
 call fix_velocity(Dip,1)
 !
 contains
 !
 subroutine fix_velocity(Dip,dir)
   !
   ! Now I need to correct DIP_v in case
   ! (a) I'll use it    (dir=+1), since I'll need v_eff
   ! (b) before writing (dir=-1), since I always write v and never v_eff
   !     (at present this is not used since all approaces compute v.
   !      However it maybe needed in the future when avoiding dipoles IO)
   !
   ! For a definition of v_eff see PRB 48, 11789 (1993)
   !
   integer,       intent(in)    :: dir
   type(DIPOLE_t),intent(inout) :: Dip
   !
   real(SP)    :: Ev_m_Ec_KS,W_
   complex(SP) :: Ev_m_Ec_QP,den,num
   !
   if(.not.(allocated(Xen%W).or.allocated(Xen%Eo))) return
   !
   if( (.not.Dip%v_eff) .and. dir==-1 )             return
   !
   W_=0._SP
   !
   do i_sp_pol=1,n_sp_pol
     do ik=1,Xk%nibz
       !
       if (.not.PAR_K_scheme%IND_ibz%element_1D(ik)) cycle
       ik_mem=PAR_K_scheme%ibz_index(ik)
       !
       do iv=Dip%ib(1),Dip%ib_lim(1) 
         do ic=Dip%ib_lim(2),Dip%ib(2)
           !
           if (allocated(Xen%W )) W_=abs(Xen%W(iv,ik,i_sp_pol))+abs(Xen%W(ic,ik,i_sp_pol))
           !
           Ev_m_Ec_KS=Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
           Ev_m_Ec_QP=Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)+cI*W_
           !
           if (allocated(Xen%Eo)) Ev_m_Ec_KS=Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)
           !
           if (dir== 1) then
             ! Define v_eff, see
             ! - Eq. (14-16) PRB 48, 11789 (1993)
             ! - Eqs. (55)-(58) PRB 95, 155203 (2017)
             num=Ev_m_Ec_QP
             den=Ev_m_Ec_KS
           endif
           !
           if (dir==-1) then
             num=Ev_m_Ec_KS
             den=Ev_m_Ec_QP
           endif
           !
           if (abs(den)<=Dip%Energy_treshold) DIP_v(:,ic,iv,ik_mem,i_sp_pol)=cZERO
           if (abs(den)> Dip%Energy_treshold) DIP_v(:,ic,iv,ik_mem,i_sp_pol)=DIP_v(:,ic,iv,ik_mem,i_sp_pol)*(num/den)
           !
         enddo
       enddo
       !
     enddo
   enddo
   !
   if( dir==-1) Dip%v_eff=.false.
   if( dir==+1) Dip%v_eff=.true.
   !
 end subroutine
 !
 subroutine SETUP_PAR_K_scheme()
   !
   use parallel_m,   ONLY:COMM_copy,PAR_INDEX_copy,COMM_reset,PP_indexes_reset
   ! DIP
   use parallel_m,  ONLY:PAR_COM_DIPk_ibz_INDEX,PAR_COM_DIPk_ibz_A2A,PAR_IND_DIPk_ibz,PAR_IND_DIPk_bz,&
   &                     PAR_DIPk_ibz_index,PAR_DIPk_bz_index,PAR_DIPk_nibz,PAR_DIPk_nbz,             &
   &                     mpi_comm_world
   ! X
   use parallel_m,  ONLY:PAR_COM_Xk_ibz_INDEX,PAR_COM_Xk_ibz_A2A,PAR_IND_Xk_ibz,PAR_IND_Xk_bz,&
   &                     PAR_Xk_ibz_index,PAR_Xk_bz_index,PAR_Xk_nibz,PAR_Xk_nbz,             &
   &                     PAR_COM_X_WORLD
   !
   implicit none
   !
   call COMM_reset(PAR_K_scheme%COM_ibz_INDEX)
   call COMM_reset(PAR_K_scheme%COM_ibz_A2A)
   call PP_indexes_reset(PAR_K_scheme%IND_ibz)
   call PP_indexes_reset(PAR_K_scheme%IND_bz)
   if (allocated(PAR_K_scheme%ibz_index)) deallocate(PAR_K_scheme%ibz_index)
   if (allocated(PAR_K_scheme%bz_index))  deallocate(PAR_K_scheme%bz_index)
   !
   select case(trim(PAR_K_scheme_kind))
   case ("DIP","RT","SC")
     call COMM_copy(PAR_COM_DIPk_ibz_INDEX,PAR_K_scheme%COM_ibz_INDEX)
     call COMM_copy(PAR_COM_DIPk_ibz_A2A,  PAR_K_scheme%COM_ibz_A2A)
     call PAR_INDEX_copy(PAR_IND_DIPk_ibz,PAR_K_scheme%IND_ibz)
     call PAR_INDEX_copy(PAR_IND_DIPk_bz, PAR_K_scheme%IND_bz )
     PAR_K_scheme%nibz=PAR_DIPk_nibz
     PAR_K_scheme%nbz =PAR_DIPk_nbz
     if (allocated(PAR_DIPk_ibz_index)) then
       allocate(PAR_K_scheme%ibz_index(size(PAR_DIPk_ibz_index)))
       PAR_K_scheme%ibz_index=PAR_DIPk_ibz_index
     endif
     if (allocated(PAR_DIPk_bz_index)) then
       allocate(PAR_K_scheme%bz_index(size(PAR_DIPk_bz_index)))
       PAR_K_scheme%bz_index=PAR_DIPk_bz_index
     endif
     PAR_K_scheme%comm_world=mpi_comm_world
   case ("X","NL")
     call COMM_copy(PAR_COM_Xk_ibz_INDEX,PAR_K_scheme%COM_ibz_INDEX)
     call COMM_copy(PAR_COM_Xk_ibz_A2A,  PAR_K_scheme%COM_ibz_A2A)
     call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_K_scheme%IND_ibz)
     call PAR_INDEX_copy(PAR_IND_Xk_bz, PAR_K_scheme%IND_bz )
     PAR_K_scheme%nibz=PAR_Xk_nibz
     PAR_K_scheme%nbz =PAR_Xk_nbz
     if (allocated(PAR_Xk_ibz_index)) then
       allocate(PAR_K_scheme%ibz_index(size(PAR_Xk_ibz_index)))
       PAR_K_scheme%ibz_index=PAR_Xk_ibz_index
     endif
     if (allocated(PAR_Xk_bz_index)) then
       allocate(PAR_K_scheme%bz_index(size(PAR_Xk_bz_index)))
       PAR_K_scheme%bz_index=PAR_Xk_bz_index
     endif
     PAR_K_scheme%comm_world=PAR_COM_X_WORLD%COMM
   case ("K","QED")
     call COMM_copy(PAR_COM_Xk_ibz_INDEX,PAR_K_scheme%COM_ibz_INDEX)
     call COMM_copy(PAR_COM_Xk_ibz_A2A,  PAR_K_scheme%COM_ibz_A2A)
     call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_K_scheme%IND_ibz)
     call PAR_INDEX_copy(PAR_IND_Xk_bz, PAR_K_scheme%IND_bz )
     PAR_K_scheme%nibz=PAR_Xk_nibz
     PAR_K_scheme%nbz =PAR_Xk_nbz
     if (allocated(PAR_Xk_ibz_index)) then
       allocate(PAR_K_scheme%ibz_index(size(PAR_Xk_ibz_index)))
       PAR_K_scheme%ibz_index=PAR_Xk_ibz_index
     endif
     if (allocated(PAR_Xk_bz_index)) then
       allocate(PAR_K_scheme%bz_index(size(PAR_Xk_bz_index)))
       PAR_K_scheme%bz_index=PAR_Xk_bz_index
     endif
     PAR_K_scheme%comm_world=mpi_comm_world
   end select
   !
 end subroutine SETUP_PAR_K_scheme
 !
end subroutine DIPOLE_IO

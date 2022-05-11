!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
 use pars,        ONLY:SP,cI,cZERO
 use parser_m,    ONLY:parser
 use stderr,      ONLY:intc
 use com,         ONLY:msg
 use IO_m,        ONLY:io_DIP
 use R_lattice,   ONLY:bz_samp
 use electrons,   ONLY:levels,n_sp_pol
 use DIPOLES,     ONLY:DIPOLE_t,DIP_v,use_covariant_approach
 use IO_int,      ONLY:io_control
 use IO_m,        ONLY:manage_action,VERIFY,DUMP,REP,OP_WR_CL,OP_WR,OP_RD_CL,&
&                      OP_IF_START_APP_CL_IF_END,OP_IF_START_RD_CL_IF_END
 use parallel_int,ONLY:PP_redux_wait
 use parallel_m,  ONLY:master_cpu,PAR_K_scheme
#if defined _RT
 use RT_control,  ONLY:l_project_spin_dipoles
#endif
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
&                     write_header_ovlp,read_header_ovlp,l_project_dipoles
 integer           :: ID,ID_S,IO_ACTION,IO_ACT_NOW,IO_MODE,ik_mem,ik,i_sp_pol,i_fragment,iv,ic,max_count
 integer, external :: io_DIPOLES
 integer, external :: io_Overlaps
 integer  :: i_count
 !
 if (.not.IO_dip) return
 !
 check  =trim(check_read_or_write)=="check"
 reading=trim(check_read_or_write)=="read"
 writing=trim(check_read_or_write)=="write"
 !
 if(.not.any((/check,reading,writing/))) call error("Wrong call to DIPOLES_IO")
 !
#if defined _RT
 call parser("SPINprojected",l_project_spin_dipoles)
 call parser("DIPprojected", l_project_dipoles)
#endif
 !
 call PARALLEL_SETUP_K_scheme(PAR_K_scheme_kind)
 !
 ID=0
 ID_S=0
 io_err=0
 !
 write_header  =  (master_cpu.and.writing)
 read_header   =  .not.write_header 
 !
 write_header_ovlp  =write_header.and.use_covariant_approach
 read_header_ovlp   = read_header.and.use_covariant_approach
 !
 if(write_header) then
   call msg('s','[DIP] Writing dipoles header ')
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_err=io_DIPOLES(Dip,ID)
 endif
 !
 if(write_header_ovlp) then
   call msg('s','[DIP] Writing overlaps header ')
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID_S)
   io_err=io_err+io_Overlaps(Dip,ID_S)
 endif
 !
 ! To fix problems with asynchronous I/O 
 !
 call PP_redux_wait(COMM=PAR_K_scheme%comm_world)
 !
 IO_MODE=VERIFY
 if((read_header.or.read_header_ovlp).and.writing) IO_MODE=DUMP
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
     if(i_count==0) call msg('s','[DIP] Checking dipoles header ')
     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=IO_MODE,ID=ID)
     io_err=io_DIPOLES(Dip,ID)
     if(read_header_ovlp) then
       if(i_count==0) call msg('s','[DIP] Checking overlaps header ')
       call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=IO_MODE,ID=ID_S)
       io_err=io_err+io_Overlaps(Dip,ID_S)
     endif
     if(io_err/=0) call sleep(1)
     i_count=i_count+1
     if((i_count==max_count).and.io_err/=0) then
       if(writing) call error(" [DIP] Header not found while writing dipoles ")
       if(reading) call error(" [DIP] Header not found while reading dipoles ")
       if(  check) call msg("s","[DIP] Database not correct or missing. To be computed ")
       return
     endif
   enddo
 endif
 !
 if(check)   return
 !
 if(writing) call PP_redux_wait(COMM=PAR_K_scheme%comm_world)
 !
 if(writing) call fix_velocity(Dip,-1)
 !
 if(reading) IO_ACTION=OP_IF_START_RD_CL_IF_END
 if(writing) IO_ACTION=OP_IF_START_APP_CL_IF_END
 !
#if defined _PAR_IO
 if ( (PAR_K_scheme%COM_ibz_A2A%CPU_id==0.and.writing).or.reading ) then
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
     IO_ACT_NOW=manage_action(IO_ACTION,i_fragment,1,Xk%nibz*n_sp_pol,PAR_K_scheme%IND_ibz)
#endif
     call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/1+i_fragment/),ID=ID)
     io_err=io_DIPOLES(Dip,ID)
     if (io_err/=0) call error(" Reading dipoles with (i_sp_pol,ik)= ("//trim(intc(i_sp_pol))//","//trim(intc(ik))//")")
   enddo
 enddo
#if defined _PAR_IO
   IO_ACT_NOW=manage_action(IO_ACTION,2,0,2)
   call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/2+Xk%nibz*n_sp_pol/),ID=ID)
   io_err=io_DIPOLES(Dip,ID)
 endif
#endif
 !
 if(use_covariant_approach) then
#if defined _PAR_IO
   if ( (PAR_K_scheme%COM_ibz_A2A%CPU_id==0.and.writing).or.reading ) then
     IO_ACT_NOW=manage_action(IO_ACTION,0,0,2)
     call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/0/),ID=ID_S)
     io_err=io_Overlaps(Dip,ID_S)
     IO_ACT_NOW=manage_action(IO_ACTION,1,0,2)
#endif
   do i_sp_pol=1,n_sp_pol
     do ik=1,Xk%nbz
       i_fragment=ik+(i_sp_pol-1)*Xk%nbz
       if ((.not.PAR_K_scheme%IND_bz%element_1D(ik)) .and.writing) cycle
       if ((.not.PAR_K_scheme%COM_ibz_A2A%CPU_id==0) .and.writing) cycle
#if !defined _PAR_IO
       if (writing) IO_ACT_NOW=manage_action(IO_ACTION,i_fragment,1,Xk%nbz*n_sp_pol,PAR_K_scheme%IND_bz)
       if (reading) IO_ACT_NOW=manage_action(IO_ACTION,i_fragment,1,Xk%nbz*n_sp_pol)
#endif
       call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/1+i_fragment/),ID=ID_S)
       io_err=io_Overlaps(Dip,ID_S)
       if (io_err/=0) call error(" Reading overlaps with (i_sp_pol,ik)= ("//trim(intc(i_sp_pol))//","//trim(intc(ik))//")")
     enddo
   enddo
#if defined _PAR_IO
     IO_ACT_NOW=manage_action(IO_ACTION,2,0,2)
     call io_control(ACTION=IO_ACT_NOW,COM=REP,SEC=(/2+Xk%nibz*n_sp_pol/),ID=ID_S)
     io_err=io_Overlaps(Dip,ID_S)
   endif
#endif
 endif
 !
 call PP_redux_wait(COMM=PAR_K_scheme%comm_world)
 !
 call fix_velocity(Dip,1)
 !
#if defined _RT
 if(reading.and.l_project_spin_dipoles) call project_spin_dipoles(Dip)
 if(reading.and.l_project_dipoles)      call project_dipoles(Dip)
#endif
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
     use DIPOLES,   ONLY:DIP_v
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
#if defined _RT
   subroutine project_spin_dipoles(Dip)
     use DIPOLES,   ONLY:DIP_spin
     type(DIPOLE_t),  intent(in) :: Dip
     !
     if(.not.allocated(DIP_spin)) return
     !
     do i_sp_pol=1,n_sp_pol
       do iv=Dip%ib(1),Dip%ib_lim(1) 
         do ic=Dip%ib_lim(2),Dip%ib(2)
           !
           if (ic<=Xen%nbf(i_sp_pol) .and. iv<=Xen%nbf(i_sp_pol)) then
             DIP_spin(:,ic,iv,:,i_sp_pol,2)=DIP_spin(:,ic,iv,:,i_sp_pol,1)
           endif
           if (ic> Xen%nbf(i_sp_pol) .and. iv> Xen%nbf(i_sp_pol) ) then
             DIP_spin(:,ic,iv,:,i_sp_pol,3)=DIP_spin(:,ic,iv,:,i_sp_pol,1)
           endif
           !
         enddo
       enddo
     enddo
     !
   end subroutine project_spin_dipoles
   !
   subroutine project_dipoles(Dip)
     use DIPOLES,   ONLY:DIP_iR,DIP_P,DIP_v
     use pars,      ONLY:cZERO
     type(DIPOLE_t),  intent(in) :: Dip
     !
     if(.not.allocated(DIP_iR)) return
     !
     do i_sp_pol=1,n_sp_pol
       do iv=Dip%ib(1),Dip%ib_lim(1) 
         do ic=Dip%ib_lim(2),Dip%ib(2)
           !
           if (ic<=Xen%nbf(i_sp_pol) .and. iv<=Xen%nbf(i_sp_pol) ) then
             DIP_iR(:,ic,iv,:,i_sp_pol)=cZERO
             DIP_P (:,ic,iv,:,i_sp_pol)=cZERO
             DIP_v (:,ic,iv,:,i_sp_pol)=cZERO
           endif
           if (ic> Xen%nbf(i_sp_pol) .and. iv> Xen%nbf(i_sp_pol) ) then
             DIP_iR(:,ic,iv,:,i_sp_pol)=cZERO
             DIP_P (:,ic,iv,:,i_sp_pol)=cZERO
             DIP_v (:,ic,iv,:,i_sp_pol)=cZERO
           endif
           !
         enddo
       enddo
     enddo
     !
   end subroutine project_dipoles
#endif
   !
end subroutine DIPOLE_IO

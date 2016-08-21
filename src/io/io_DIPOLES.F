!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
integer function io_DIPOLES(X,ID)
 !
 use pars,          ONLY:SP,schlen
 use X_m,           ONLY:X_t,DIP_iR,DIP_P,X_alloc
 use electrons,     ONLY:n_spin
 use R_lattice,     ONLY:nXkibz
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,frag_DIP,&
&                        io_bulk,read_is_on,write_is_on,io_header,io_extension,&
&                        RD_CL_IF_END,RD_CL,io_action,&
&                        IO_NOT_ALLOWED,io_DIP,io_fragment
 use parallel_m,    ONLY:PAR_DIPk_ibz_index,PAR_DIPk_nibz
#if defined _RT
 use X_m,           ONLY:DIP_spin
#endif
#if defined _SC
 use X_m,           ONLY:P_square
 use drivers,       ONLY:l_real_time,l_sc_run
#endif
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
 implicit none
 type(X_t)   ::X
 integer     ::ID
 !
 ! Work Space
 !
 integer :: ik,ik_mem,i_fragment,i_sp_pol,DIP_size(4),P2_size(3)
 integer, external    :: variables_DIPOLES
 character(schlen)    :: VAR_name
 complex(SP),allocatable :: disk_data(:,:,:)
 !
 if (.not.io_DIP) then
   io_DIPOLES=IO_NOT_ALLOWED
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
#if defined _TIMING
 call timing('io_DIPOLES',OPR='start')
#endif
 !
 io_extension(ID)='dip_iR_and_P'
#if defined _SC
 if (l_sc_run)    io_extension(ID)='dip_iR_P_and_P2'
#endif
#if defined _RT
 if (l_real_time) io_extension(ID)='dip_iR_P_spin_and_P2'
#endif
 !
 io_DIPOLES=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID)
 if (io_DIPOLES/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_DIPOLES=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,T_EL=.true.,FRAG=frag_DIP)
   if (io_DIPOLES/=0) goto 1
   !
   io_DIPOLES=variables_DIPOLES(X,ID)
   if (io_DIPOLES/=0) goto 1
   !
 endif
 !
 i_fragment=maxval(io_sec(ID,:))-1
 if(i_fragment<=nXkibz) then
   i_sp_pol=1
   ik=i_fragment
 else
   i_sp_pol=2
   ik=i_fragment-nXkibz
 endif
 !
 if (ik==0) goto 1
 !
 ik_mem=PAR_DIPk_ibz_index(ik)
 !
 allocate(disk_data(3,X%db_ib_lim(2):X%db_ib(2),X%db_ib(1):X%db_ib_lim(1)))
 DIP_size=(/2,3,X%db_ib(2)-X%db_ib_lim(2)+1,X%db_ib_lim(1)-X%db_ib(1)+1/)
 P2_size(:)=DIP_size(2:4)
 !
 if (read_is_on(ID)) then
   call X_alloc('DIP_iR',(/3,X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
   call X_alloc('DIP_P' ,(/3,X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
#if defined _RT
   if (l_real_time.and.n_spin>1) call X_alloc('DIP_spin',(/3,X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
#endif
#if defined _SC
   if (l_sc_run.or.l_real_time)  call X_alloc('P_square',(/X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
#endif
 endif
 !
 ! Fragmentation
 !
 call io_fragment(ID,i_fragment=i_fragment)
 !
 ! Manage RD_CL_IF_END
 !
 if (io_action(ID)==RD_CL_IF_END.and.ik==nXkibz) io_action(ID)=RD_CL
 !
 ! iR
 !
 if(write_is_on(ID)) disk_data=DIP_iR(:,:,:,ik_mem,i_sp_pol)
 write (VAR_name,'(3(a,i4.4))') 'DIP_iR_k_',ik,'_spin_',i_sp_pol
 call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=DIP_size)
 call io_bulk(ID,C3=disk_data )
 if(read_is_on(ID)) DIP_iR(:,:,:,ik_mem,i_sp_pol)=disk_data(:,X%ib_lim(2):X%ib(2),X%ib(1):X%ib_lim(1))
 !
 ! P
 !
 if(write_is_on(ID)) disk_data=DIP_P(:,:,:,ik_mem,i_sp_pol)
 write (VAR_name,'(3(a,i4.4))') 'DIP_P_k_',ik,'_spin_',i_sp_pol
 call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=DIP_size)
 call io_bulk(ID,C3=disk_data )
 if(read_is_on(ID)) DIP_P(:,:,:,ik_mem,i_sp_pol)=disk_data(:,X%ib_lim(2):X%ib(2),X%ib(1):X%ib_lim(1))
 !
#if defined _RT
 if (l_real_time.and.n_spin>1) then
   !
   ! Spin
   !
   if(write_is_on(ID)) disk_data=DIP_spin(:,:,:,ik_mem,i_sp_pol)
   write (VAR_name,'(3(a,i4.4))') 'DIP_spin_k_',ik,'_spin_',i_sp_pol
   call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=DIP_size)
   call io_bulk(ID,C3=disk_data )
   if(read_is_on(ID)) DIP_spin(:,:,:,ik_mem,i_sp_pol)=disk_data(:,X%ib_lim(2):X%ib(2),X%ib(1):X%ib_lim(1))
   !
 endif
#endif
 !
#if defined _SC
 if (l_sc_run.or.l_real_time) then
   !
   ! P2
   !
   if(write_is_on(ID)) disk_data(1,:,:)=P_square(:,:,ik_mem,i_sp_pol)
   write (VAR_name,'(3(a,i4.4))') 'P_square_k_',ik,'_spin_',i_sp_pol
   call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=P2_size)
   call io_bulk(ID,C2=disk_data(1,:,:) )
   if(read_is_on(ID)) P_square(:,:,ik_mem,i_sp_pol)=disk_data(1,X%ib_lim(2):X%ib(2),X%ib(1):X%ib_lim(1))
   !
 endif
#endif
 !
1 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 if (io_DIP) call timing('io_DIPOLES',OPR='stop')
#endif
 !
end function

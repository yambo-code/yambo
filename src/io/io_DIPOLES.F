!
!        Copyright (C) 2000-2015 the YAMBO team
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
integer function io_DIPOLES(X,Xen,ID)
 !
 use pars,          ONLY:SP,schlen
 use X_m,           ONLY:X_t,DIP_iR,DIP_P,P_square,X_alloc,Dipole_bands_ordered
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:nXkibz
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,io_fragmented,frag_DIP,&
&                        io_bulk,read_is_on,write_is_on,io_header,io_extension,&
&                        RD_CL_IF_END,RD_CL,io_action,&
&                        IO_NOT_ALLOWED,io_DIP,io_fragment
 use parallel_m,    ONLY:PAR_Xk_ibz_index,PAR_Xk_nibz
#if defined _SC
 use drivers,       ONLY:l_real_time,l_sc_run
#endif
#if defined _RT
 use drivers,       ONLY:l_eval_collisions
#endif
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
 implicit none
 type(X_t)   ::X
 type(levels)::Xen
 integer     ::ID
 !
 ! Work Space
 !
 integer :: ik,ik_mem,ixyz,sec_size,i_sp_pol,db_nbm,db_nb(2)
 integer, external    :: variables_DIPOLES
 character(schlen)    :: VAR_name
 real(SP),allocatable :: disk_data(:,:,:)
 !
#if defined _RT
 if (l_real_time.or.l_eval_collisions) io_DIP=.FALSE.
#endif
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
 if (l_sc_run.or.l_real_time) io_extension(ID)='dip_iR_P_and_P2'
#endif
 !
 io_DIPOLES=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID)
 if (io_DIPOLES/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   if(write_is_on(ID)) io_fragmented(ID)=frag_DIP
   io_DIPOLES=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,T_EL=.true.)
   if(read_is_on(ID)) frag_DIP=io_fragmented(ID)
   if (io_DIPOLES/=0) goto 1
   !
   io_DIPOLES=variables_DIPOLES(X,Xen,ID,db_nbm,db_nb)
   if (io_DIPOLES/=0) goto 1
   !
 endif
 !
 db_nb=X%ib
 db_nbm=Xen%nbm
 !
 if (.not.Dipole_bands_ordered) db_nbm=db_nb(2)
 !
 ! On disk the size is DIP_iR_or_P(3,X%ib(2),X%ib(2),nXkibz)
 !
 sec_size=3*db_nb(2)*X%ib(2)
 !
 i_sp_pol=1
 ik=maxval(io_sec(ID,:))-1
 if(ik>nXkibz) then
   i_sp_pol=2
   ik=ik-nXkibz
 endif
 !
 if (ik==0) goto 1
 !
 ik_mem=PAR_Xk_ibz_index(ik)
 !
 allocate(disk_data(X%ib(2),X%ib(2),2))
 !
 if (read_is_on(ID)) then
   call X_alloc('DIP_iR',(/3,X%ib(2),X%ib(2),PAR_Xk_nibz/))
   call X_alloc('DIP_P' ,(/3,X%ib(2),X%ib(2),PAR_Xk_nibz/))
 endif
 !
#if defined _SC
 if (l_sc_run.or.l_real_time.and.read_is_on(ID)) call X_alloc('P_square',(/X%ib(2),X%ib(2),PAR_Xk_nibz/))
#endif
 !
 ! Fragmentation
 !
 call io_fragment(ID,i_fragment=ik)
 !
 ! Manage RD_CL_IF_END
 !
 if (io_action(ID)==RD_CL_IF_END.and.ik==nXkibz) io_action(ID)=RD_CL
 !
 do ixyz=1,3
   !
   ! iR
   !
   if (write_is_on(ID)) call mat_c2r(DIP_iR(ixyz,:,:,ik_mem,i_sp_pol),disk_data)
   write (VAR_name,'(3(a,i4.4))') 'DIP_iR_k_',ik,'_xyz_',ixyz,'_spin_',i_sp_pol
   call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(disk_data))
   call io_bulk(ID,R3=disk_data)
   if (read_is_on(ID)) call mat_r2c(disk_data,DIP_iR(ixyz,:,:,ik_mem,i_sp_pol))
   !
   ! P
   !
   if (write_is_on(ID)) call mat_c2r(DIP_P(ixyz,:,:,ik_mem,i_sp_pol),disk_data)
   write (VAR_name,'(3(a,i4.4))') 'DIP_P_k_',ik,'_xyz_',ixyz,'_spin_',i_sp_pol
   call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(disk_data))
   call io_bulk(ID,R3=disk_data)
   if (read_is_on(ID)) call mat_r2c(disk_data,DIP_P(ixyz,:,:,ik_mem,i_sp_pol))
   !
 enddo
 !
#if defined _SC
 if (l_sc_run.or.l_real_time) then
   !
   ! P2
   !
   if (write_is_on(ID)) call mat_c2r(P_square(:,:,ik_mem,i_sp_pol),disk_data)
   write (VAR_name,'(2(a,i4.4))') 'P_square_k_',ik,'_spin_',i_sp_pol
   call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(disk_data))
   call io_bulk(ID,R3=disk_data)
   if (read_is_on(ID)) call mat_r2c(disk_data,P_square(:,:,ik_mem,i_sp_pol))
   !
 endif
#endif
 !
 deallocate(disk_data)
 !
1 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 if (io_DIP) call timing('io_DIPOLES',OPR='stop')
#endif
 !
end function

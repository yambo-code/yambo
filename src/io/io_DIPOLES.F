!
!        Copyright (C) 2000-2018 the YAMBO team
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
 use pars,          ONLY:schlen
 use X_m,           ONLY:X_t,DIP_iR,DIP_P,X_alloc
 use electrons,     ONLY:n_spin
 use R_lattice,     ONLY:nXkibz
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,frag_DIP,io_DIP,&
&                        io_bulk,read_is_on,io_header,io_extension,&
&                        IO_NOT_ALLOWED,io_fragment,manage_OP_IF_START_and_CL_IF_END
 use parallel_m,    ONLY:PAR_DIPk_ibz_index,PAR_DIPk_nibz
#if defined _RT
 use X_m,           ONLY:DIP_spin,DIP_orbital,DIP_P_spinor
 use electrons,     ONLY:n_spinor
 use com,           ONLY:secnm
#endif
#if defined _SC || defined _RT
 use X_m,           ONLY:P_square
 use drivers,       ONLY:l_sc_run
 use drivers,       ONLY:l_real_time,l_sc_run
#endif
 use timing_m,      ONLY:timing
 !
 implicit none
 type(X_t)   ::X
 integer     ::ID
 !
 ! Work Space
 !
 integer :: ik,ik_mem,i_fragment,i_sp_pol,DIP_size(4),DIP_ipos(4)
#if defined _RT || defined _SC
 logical :: l_real_time_magn_dipoles
 integer :: P2_size(3),P2_ipos(3),Pspinor_size(6),Pspinor_ipos(6)
#endif
 integer, external    :: variables_DIPOLES
 character(schlen)    :: VAR_name
 !
 if (.not.io_DIP) then
   io_DIPOLES=IO_NOT_ALLOWED
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
 call timing('io_DIPOLES',OPR='start')
 !
 io_extension(ID)='dip_iR_and_P'
#if defined _RT
 l_real_time_magn_dipoles=l_real_time.and.n_spin>1.and.(.not.index(secnm,"Dielectric")/=0)
 if (l_real_time_magn_dipoles) io_extension(ID)='dip_iR_P_spin_and_P2'
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
 ! Reading size and position
 !
 DIP_size=(/2,3,X%db_ib(2)-X%db_ib_lim(2)+1,X%db_ib_lim(1)-X%db_ib(1)+1/)
 DIP_ipos=(/1,1,X%ib_lim(2)-X%db_ib_lim(2)+1,X%ib(1)-X%db_ib(1)+1/)
 !
#if defined _RT
 Pspinor_size(1:4)=(/2,3,n_spinor,n_spinor/)
 Pspinor_ipos(1:4)=(/1,1,1,1/)
 !
 Pspinor_size(5:6)=DIP_size(3:4)
 Pspinor_ipos(5:6)=DIP_ipos(3:4)
#endif
 !
#if defined _RT || defined _SC
 P2_size(1)=2
 P2_ipos(1)=1
 !
 P2_size(2:3)=DIP_size(3:4)
 P2_ipos(2:3)=DIP_ipos(3:4)
#endif
 !
 if (read_is_on(ID)) then
   call X_alloc('DIP_iR',(/3,X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
   call X_alloc('DIP_P' ,(/3,X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),PAR_DIPk_nibz/))
   !
   !
 endif
 !
 ! Manage OP_IF_START_and_RD_CL_IF_END
 !
 call manage_OP_IF_START_and_CL_IF_END(ID,ik,nXkibz)
 !
 ! Fragmentation
 !
 call io_fragment(ID,i_fragment=i_fragment)
 !
 ! iR
 !
 write (VAR_name,'(3(a,i4.4))') 'DIP_iR_k_',ik,'_spin_',i_sp_pol
 call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=DIP_size)
 call io_bulk(ID,C3=DIP_iR(:,:,:,ik_mem,i_sp_pol),IPOS=DIP_ipos)
 !
 ! P
 !
 write (VAR_name,'(3(a,i4.4))') 'DIP_P_k_',ik,'_spin_',i_sp_pol
 call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=DIP_size)
 call io_bulk(ID,C3=DIP_P(:,:,:,ik_mem,i_sp_pol),IPOS=DIP_ipos)
 !
 !
1 call io_disconnect(ID=ID)
 !
 if (io_DIP) call timing('io_DIPOLES',OPR='stop')
 !
end function

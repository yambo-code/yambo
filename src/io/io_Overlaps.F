!
!        Copyright (C) 2000-2014 the YAMBO team
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
integer function io_Overlaps(X,Xen,ID)
 !
 use pars,          ONLY:SP,schlen
 use X_m,           ONLY:X_t,X_alloc,P_square,DIP_S
 use electrons,     ONLY:levels,n_sp_pol
 use R_lattice,     ONLY:nXkibz,nXkbz
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,io_fragmented,&
&                        io_bulk,read_is_on,write_is_on,io_header,io_extension,&
&                        IO_NOT_ALLOWED,io_DIP,io_fragment,frag_DIP
 use parallel_m,    ONLY:PAR_Xk_bz_index,PAR_Xk_nbz,PAR_IND_Xk_bz
 !
 implicit none
 type(X_t)   ::X
 type(levels)::Xen
 integer     ::ID
 !
 ! Work Space
 !
 integer :: ik,ixyz,i_spin,db_nbm,db_nb(2),ik_mem
 character(schlen)    :: VAR_name
 real(SP),allocatable :: disk_data(:,:,:)
 integer, external    :: variables_DIPOLES
 !
 if (.not.io_DIP) then
   io_Overlaps=IO_NOT_ALLOWED
   goto 1
 endif
 !
 io_Overlaps=io_connect(desc='Overlaps',type=2,ID=ID)
 if (io_Overlaps/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   if(write_is_on(ID)) io_fragmented(ID)=frag_DIP
   io_Overlaps=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,T_EL=.true.)
   if(read_is_on(ID) ) frag_DIP=io_fragmented(ID)
   if (io_Overlaps/=0) goto 1
   !
   io_Overlaps=variables_DIPOLES(X,Xen,ID,db_nbm,db_nb)
   if (io_Overlaps/=0) goto 1
   !
 endif
 !
 db_nb =X%ib
 db_nbm=db_nb(2)
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   allocate(disk_data(db_nb(2),db_nbm,2))
   !
   if (read_is_on(ID)) call X_alloc('DIP_S',(/X%ib(2),db_nbm,3,PAR_Xk_nbz/))
   !
   do ik=1,nXkbz
     !
     if (.not.PAR_IND_Xk_bz%element_1D(ik)) cycle
     !
     ik_mem=PAR_Xk_bz_index(ik)
     !
     ! Fragmentation
     !
     io_extension(ID)='Overlaps'
     call io_fragment(ID,i_fragment=ik)
     !
     do ixyz=1,3
       !
       do i_spin=1,n_sp_pol
         !
         if (write_is_on(ID)) call mat_c2r(DIP_S(:,:,ixyz,ik_mem,i_spin),disk_data)
         !
         write (VAR_name,'(3(a,i4.4))') 'DIP_S_k_',ik,'_xyz_',ixyz,'_spin_',i_spin
         call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(disk_data))
         call io_bulk(ID,R3=disk_data)
         !
         if (read_is_on(ID)) call mat_r2c(disk_data,DIP_S(:,:,ixyz,ik_mem,i_spin))
         !
       enddo
       !
     enddo
     !
   enddo
   !
   deallocate(disk_data)
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function

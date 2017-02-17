!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
 use X_m,           ONLY:X_t,DIP_iR,X_alloc,Dipole_bands_ordered
 use electrons,     ONLY:levels,n_sp_pol
 use R_lattice,     ONLY:nXkibz
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,io_fragmented,&
&                        io_bulk,read_is_on,write_is_on,io_header,io_extension
 use fragments,     ONLY:io_fragment
 !
 implicit none
 type(X_t)   ::X
 type(levels)::Xen
 integer     ::ID
 !
 ! Work Space
 !
 integer :: i1,ixyz,sec_size,i_spin,db_nbm,db_nb(2)
 integer, external    :: variables_DIPOLES
 character(schlen)    :: VAR_name
 real(SP),allocatable :: DIP_iR_disk(:,:,:)
 !
 io_DIPOLES=io_connect(desc='dipoles',type=2,ID=ID)
 if (io_DIPOLES/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_DIPOLES=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,T_EL=.true.)
   if (io_DIPOLES/=0) goto 1
   !
   io_DIPOLES=variables_DIPOLES(X,Xen,ID,db_nbm,db_nb)
   if (io_DIPOLES/=0) goto 1
   !
 endif
 !
 if (.not.Dipole_bands_ordered) db_nbm=db_nb(2)
 !
 ! On disk the size is DIP_iR_or_P(3,db_nb(2),db_nbm,nXkibz)
 !
 sec_size=3*db_nb(2)*db_nbm
 if (any((/io_sec(ID,:)==2/))) then
   !
   allocate(DIP_iR_disk(db_nb(2),db_nbm,2))
   !
   if (read_is_on(ID)) call X_alloc('DIP_iR',(/3,X%ib(2),db_nbm,nXkibz/))
   !
   do i1=1,nXkibz
     !
     ! Fragmentation
     !
     io_extension(ID)='dipoles'
     if (io_fragmented(ID)) call io_fragment(ID,i_fragment=i1)
     !
     do ixyz=1,3
       !
       do i_spin=1,n_sp_pol
         !
         if (write_is_on(ID)) call mat_c2r(DIP_iR(ixyz,:,:,i1,i_spin),DIP_iR_disk)
         !
         write (VAR_name,'(3(a,i4.4))') 'DIP_iR_k_',i1,'_xyz_',ixyz,'_spin_',i_spin
         call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(DIP_iR_disk))
         call io_bulk(ID,R3=DIP_iR_disk)
         !
         if (read_is_on(ID)) call mat_r2c(DIP_iR_disk,DIP_iR(ixyz,:,:,i1,i_spin))
         !
       enddo
       !
     enddo
     !
   enddo
   !
   deallocate(DIP_iR_disk)
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function

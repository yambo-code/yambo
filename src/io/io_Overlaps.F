!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA MG
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
integer function io_Overlaps(Dip,ID)
 !
 use pars,          ONLY:SP,schlen
 use DIPOLES,       ONLY:DIPOLE_t,DIP_alloc,DIP_S
 use electrons,     ONLY:levels,n_sp_pol
 use R_lattice,     ONLY:nXkibz,nXkbz
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,&
&                        io_bulk,read_is_on,write_is_on,io_header,io_extension,&
&                        IO_NOT_ALLOWED,io_DIP,io_fragment,frag_DIP
 use parallel_m,    ONLY:PAR_DIPk_bz_index,PAR_DIPk_nbz,PAR_IND_DIPk_bz
 !
 implicit none
 type(DIPOLE_t) :: Dip
 integer        :: ID
 !
 ! Work Space
 !
 integer :: ik,ixyz,i_sp_pol,ik_mem,i_fragment
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
   io_Overlaps=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,T_EL=.true.,FRAG=frag_DIP)
   if (io_Overlaps/=0) goto 1
   !
   io_Overlaps=variables_DIPOLES(Dip,ID)
   if (io_Overlaps/=0) goto 1
   !
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
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
   allocate(disk_data(Dip%ib(2),Dip%ib(2),2))
   !
   if (read_is_on(ID)) call DIP_alloc('DIP_S',(/Dip%ib(2),Dip%ib(2),6,PAR_DIPk_nbz/))
   !
   do i_sp_pol=1,n_sp_pol
     !
     do ik=1,nXkbz
       !
       if (.not.PAR_IND_DIPk_bz%element_1D(ik)) cycle
       !
       ik_mem=PAR_DIPk_bz_index(ik)
       !
       ! Fragmentation
       !
       io_extension(ID)='Overlaps'
       call io_fragment(ID,i_fragment=i_fragment)
       !
       do ixyz=1,6
         !
         if (write_is_on(ID)) call mat_c2r(DIP_S(:,:,ixyz,ik_mem,i_sp_pol),disk_data)
         !
         write (VAR_name,'(3(a,i4.4))') 'DIP_S_k_',ik,'_xyz_',ixyz,'_spin_',i_sp_pol
         call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(disk_data))
         call io_bulk(ID,R3=disk_data)
         !
         if (read_is_on(ID)) call mat_r2c(disk_data,DIP_S(:,:,ixyz,ik_mem,i_sp_pol))
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

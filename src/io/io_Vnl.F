!
!        Copyright (C) 2000-2019 the YAMBO team
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
integer function io_Vnl(X,Xen,ID)
 !
 use pars,          ONLY:SP,schlen
 use X_m,           ONLY:X_t
 use electrons,     ONLY:levels,n_sp_pol
 use pseudo,        ONLY:Vnl
 use R_lattice,     ONLY:nXkibz
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,io_elemental,io_status,io_bulk,&
&                        read_is_on,write_is_on,io_header,io_extension,&
&                        io_fragment,frag_WF
 use global_XC,     ONLY:Dipole_WF_xc_string,loaded_WF_xc_string
 use com,           ONLY:warning
#include<memory.h>
 !
 type(X_t)   ::X
 type(levels)::Xen
 integer     ::ID
 !
 ! Work Space
 !
 integer :: i1,ixyz,sec_size,i_spin
 integer :: db_nbm,db_nbf,db_nb(2)
 character(schlen)    :: VAR_name
 real(SP),allocatable :: Vnl_disk(:,:,:) ! complex, ic, iv
 !
 io_Vnl=io_connect(desc='Vnl',type=0,ID=ID)
 !
 if (io_Vnl/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_Vnl=io_header(ID,IMPOSE_SN=.true.,FRAG=frag_WF)
   if (io_Vnl/=0) goto 1
   !
   sec_size=5
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=sec_size,MENU=0)
   call io_elemental(ID,DB_I1=db_nb,&
&       VAR=" X band range           :",I1=X%ib,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(ID,DB_I0=db_nbm,&
&       VAR=" Metallic bands         :",I0=Xen%nbm,CHECK=.true.,OP=(/"<="/))
   call io_elemental(ID,DB_I0=db_nbf,&
&       VAR=" Filled bands           :",I0=Xen%nbf,CHECK=.true.,OP=(/">="/))
   call io_elemental(ID,&
&       VAR=" RL vectors in the sum  :",I0=X%ngostnts,WARN=.true.,OP=(/"<="/))
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! Wavefunctions xc 
   !
   call io_elemental(ID,VAR='WAVE_FUNC_XC',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,DB_CH0=Dipole_WF_xc_string,CH0=loaded_WF_xc_string,&
&       VAR=' Wavefunctions          :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
   io_Vnl=io_status(ID)
   if (io_Vnl/=0) then
     call warning(' The commutator DB cannot be used. Try to lower ElecTemp in the input.')
     goto 1
   endif
 endif
 !
 ! On disk the size is Vnl(3,db_nb(2),db_nbm,nXkibz)
 !
 sec_size=3*db_nb(2)*db_nbm
 if (any((/io_sec(ID,:)==2/))) then
   !
   YAMBO_ALLOC(Vnl_disk,(db_nb(2),db_nbm,2))!Allocate to exactly the sizeo on disk)
   !
   if(read_is_on(ID)) then
      !
      ! Fill an array Vnl to size required by yambo, ignore extra elements
      !
      YAMBO_ALLOC(Vnl,(3,X%ib(2),db_nbm,nXkibz,n_sp_pol))
   endif
   !
   do i1=1,nXkibz
     !
     ! Fragmentation
     !
     io_extension(ID)='Vnl'
     call io_fragment(ID,i_fragment=i1)
     !
     do ixyz=1,3
       !
       do i_spin=1,n_sp_pol
         !
         if (write_is_on(ID)) call mat_c2r(Vnl(ixyz,:,:,i1,i_spin),Vnl_disk)
         !
         write (VAR_name,'(3(a,i4.4))') 'Vnl_k_',i1,'_xyz_',ixyz,'_spin_',i_spin
         call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(Vnl_disk))
         call io_bulk(ID,R3=Vnl_disk)
         !
         if (read_is_on(ID)) call mat_r2c(Vnl_disk,Vnl(ixyz,:,:,i1,i_spin))
         !
       enddo
       !
     enddo
     !
   enddo
   !
   YAMBO_FREE(Vnl_disk)
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function

!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA MG DS
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
 use pars,          ONLY:schlen
 use DIPOLES,       ONLY:DIPOLE_t,DIP_alloc,DIP_S
 use R_lattice,     ONLY:nXkbz
 use IO_int,        ONLY:io_connect,io_disconnect,io_fragment,io_header,io_bulk
 use IO_m,          ONLY:io_sec,frag_DIP,read_is_on,io_extension,&
&                        IO_NOT_ALLOWED,io_DIP,frag_DIP
 use timing_m,      ONLY:timing
 !
 implicit none
 type(DIPOLE_t) :: Dip
 integer        :: ID
 !
 ! Work Space
 !
 integer :: ik,ik_mem,i_fragment,i_sp_pol,ID_frag
 integer, external    :: variables_DIPOLES
 character(schlen)    :: VAR_name
 !
 if (.not.io_DIP) then
   io_Overlaps=IO_NOT_ALLOWED
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
 call timing('io_Overlaps',OPR='start')
 !
 ID_frag=0
 !
 io_extension(ID)='Overlap'
 !
 io_Overlaps=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID,FORCE_READ_MODE=(.not.any((/io_sec(ID,:)==1/))).and.frag_DIP)
 if (io_Overlaps/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_Overlaps=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,TEMP=.true.,FRAG=frag_DIP)
   if (io_Overlaps/=0) goto 1
   !
   io_Overlaps=variables_DIPOLES(Dip,ID)
   if (io_Overlaps/=0) goto 1
   !
 endif
 !
 i_fragment=maxval(io_sec(ID,:))-1
 if(i_fragment<=nXkbz) then
   i_sp_pol=1
   ik=i_fragment
 else
   i_sp_pol=2
   ik=i_fragment-nXkbz
 endif
 !
 if (ik==0) goto 1
 !
 ik_mem=ik 
 !
 if (read_is_on(ID)) call DIP_alloc('DIP_S',(/Dip%ib(2),Dip%ib(2),6,nXkbz/))
 !
 ! Fragmentation
 !
 call io_fragment(ID,ID_frag,i_fragment=i_fragment,ierr=io_Overlaps)
 if (io_Overlaps/=0) goto 1
 !
 ! DIP_S
 !
 write (VAR_name,'(3(a,i4.4))') 'DIP_S_k_',ik,'_sp_pol_',i_sp_pol
 call io_bulk(ID_frag,VAR=trim(VAR_name),VAR_SZ=(/2,Dip%ib(2),Dip%ib(2),6/))
 call io_bulk(ID_frag,C3=DIP_S(:,:,:,ik_mem,i_sp_pol))
 !
1 call io_fragment_disconnect(ID,ID_frag)
 !
 call io_disconnect(ID=ID)
 !
 call timing('io_Overlaps',OPR='stop')
 !
end function

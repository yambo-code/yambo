!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use pars,          ONLY:schlen,SP
 use DIPOLES,       ONLY:DIPOLE_t,DIP_alloc,DIP_S
 use R_lattice,     ONLY:nXkbz
 use electrons,     ONLY:n_sp_pol
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,frag_DIP,def_variable_bulk,&
&                        io_variable_bulk,read_is_on,io_header,io_extension,&
&                        manage_OP_IF_START_and_CL_IF_END,&
&                        IO_NOT_ALLOWED,io_DIP
 use parallel_m,    ONLY:PAR_K_scheme
 use timing_m,      ONLY:timing
 !
 implicit none
 type(DIPOLE_t) :: Dip
 integer        :: ID
 !
 ! Work Space
 !
 integer :: ik_bz,i_fragment,i_sp_pol,DIP_size(6),DIP_ipos(6),VAR_ID
 integer, external    :: variables_DIPOLES
 character(schlen)    :: VAR_name
 !
 if (.not.io_DIP) then
   io_Overlaps=IO_NOT_ALLOWED
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
 i_sp_pol=-1
 ik_bz=-1
 i_fragment=maxval(io_sec(ID,:))-1
 if(i_fragment<=nXkbz) then
   i_sp_pol=1
   ik_bz=i_fragment
 else if(n_sp_pol==2.and.i_fragment<=2*nXkbz) then
   i_sp_pol=2
   ik_bz=i_fragment-nXkbz
 endif
 !
 ! Manage OP_IF_START_and_RD_CL_IF_END
 !
 call manage_OP_IF_START_and_CL_IF_END(ID,NOW=i_fragment,FIRST=-1,LAST=(nXkbz*n_sp_pol+1))
 !
 call timing('io_Overlaps',OPR='start')
 !
 io_extension(ID)='Overlap'
 !
 io_Overlaps=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID,COMM=PAR_K_scheme%COM_ibz_index%COMM)
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
 if(io_Overlaps/=0) goto 1
 !
 if (i_fragment==0)               goto 1
 if (i_fragment> n_sp_pol*nXkbz) goto 1
 !
 ! if (i_fragment>=1) ik_mem=PAR_K_scheme%ibz_index(ik_bz)
 !
 if (read_is_on(ID)) call DIP_alloc('DIP_S',(/Dip%ib(2),Dip%ib(2),6,nXkbz/))
 !
 ! Reading size and position
 !
 DIP_size=(/2,Dip%db_ib(2),Dip%db_ib(2),6,nXkbz,n_sp_pol/)
 DIP_ipos=(/1,1,1,1,ik_bz,i_sp_pol/)
 !
 ! DIP_S
 !
 VAR_ID=1
 VAR_name='DIP_S'
 if (i_fragment< 1) call def_variable_bulk(ID,trim(VAR_name),VAR_ID,DIP_size,SP,par_io_kind='independent')
 if (i_fragment>=1) call  io_variable_bulk(ID,VAR_ID,C5=DIP_S(:,:,:,ik_bz:ik_bz,i_sp_pol:i_sp_pol),IPOS=DIP_ipos)
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_Overlaps',OPR='stop')
 !
end function

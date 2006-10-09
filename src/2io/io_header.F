!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 integer function io_header(ID,QPTS,R_LATT,WF,IMPOSE_SN,QP_ctl_K)
!================================================================
 use pars,          ONLY:SP,schlen
 use drivers,       ONLY:list_dbs
 use com,           ONLY:code_version,msg,warning
 use R_lattice,     ONLY:nqibz,nqbz,nkibz,nkbz,qpt
 use wave_func,     ONLY:wf_ng
 use vec_operate,   ONLY:v_is_zero
 use QP_m,          ONLY:QP_ctl_E
 use IO_m,          ONLY:io_elemental,io_bulk,io_code_version,io_status,&
&                        read_is_on,write_is_on,serial_number,&
&                        io_serial_number,io_file,io_com,io_mode,&
&                        DUMP,VERIFY,NONE,io_fragmented,dbs_fragment
 implicit none
 integer            :: ID
 logical,optional   :: QPTS,R_LATT,WF,IMPOSE_SN,QP_ctl_K
!ws
 integer             :: i1,nqibz_disk,MENU_here,R_LATT_vec(4),&
&                       R_LATT_vec_disk(4),MODE,MENU
 real(SP),allocatable:: l_qpt(:,:)
 logical             :: WARN
 character(schlen)   :: ch

 io_header=0

 WARN=.true.
 if (list_dbs) WARN=.false.

 MENU=1
 MODE=io_mode(ID)
 if (io_mode(ID)==DUMP) io_mode(ID)=VERIFY
!
! Things that CANNOT be dumped
!==============================
!
 call io_elemental(ID,VAR="FRAGMENTED",VAR_SZ=1,MENU=0)
 call io_elemental(ID,DB_L0=io_fragmented(ID),L0=dbs_fragment)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)

 call io_elemental(ID,VAR="HEAD_VERSION",VAR_SZ=3,MENU=0)
 call io_elemental(ID,DB_I1=io_code_version(ID,:),I1=code_version)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)

 call io_elemental(ID,VAR="SERIAL_NUMBER",VAR_SZ=1,MENU=0)
 call io_elemental(ID,DB_I0=io_serial_number(ID),I0=serial_number)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)

 if (io_serial_number(ID)/=serial_number) then
   if (present(IMPOSE_SN)) then 
     io_header=-1
     write (ch,'(2a)') 'Incompatible serial number for ',trim(io_file(ID))
     if (io_com(ID)/=NONE) call msg('sr',trim(ch))
     goto 1
   endif
   if (io_com(ID)/=NONE) then 
     write (ch,'(2a)') 'Wrong serial number for ',trim(io_file(ID))
     call warning(trim(ch))
   endif
 endif

 if (present(R_LATT)) then
   R_LATT_vec=(/nqibz,nqbz,nkibz,nkbz/)
   call io_elemental(ID,VAR="HEAD_R_LATT",VAR_SZ=4,MENU=MENU)
   MENU=0
   call io_elemental(ID,DB_I1=R_LATT_vec_disk,I1=R_LATT_vec,&
&       VAR=' Brillouin Zone Q/K grids (IBZ/BZ):',WARN=WARN,&
&       OP=(/"==","==","==","=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   nqibz_disk=R_LATT_vec_disk(1)
   io_header=io_status(ID)
   if (io_header/=0) goto 1
 endif

 if (present(WF)) then
   call io_elemental(ID,VAR="HEAD_WF",VAR_SZ=1,MENU=MENU)
   MENU=0
   call io_elemental(ID,I0=wf_ng,&
&       VAR=' RL vectors                   (WF):',WARN=WARN,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   io_header=io_status(ID)
   if (io_header/=0) goto 1
 endif

 if (present(QPTS)) then

   allocate(l_qpt(nqibz_disk,3))

   if (write_is_on(ID)) l_qpt=qpt

   call io_bulk(ID,VAR="HEAD_QPT",VAR_SZ=(/nqibz_disk,3/))
   call io_bulk(ID,R2=l_qpt)

   if (read_is_on(ID).and.allocated(qpt)) then
     do i1=1,nqibz
       if (.not.v_is_zero(qpt(i1,:)-l_qpt(i1,:))) io_header=-1
     enddo
     if (io_header/=0) goto 1
   endif

   deallocate(l_qpt)
 endif
!
! Things that may be dumped
!===========================
!
 if (present(QP_ctl_K)) then

   io_mode(ID)=MODE

   call io_elemental(ID,VAR="QP_CTL_K",VAR_SZ=3,MENU=MENU)
   MENU=0
   call io_elemental(ID,R1=QP_ctl_E(2,:),&
&       VAR=' Kernel QP corrections        :',CHECK=WARN,OP=(/"==","==","=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   io_header=io_status(ID)
   if (io_header/=0) goto 1
 endif

1 io_mode(ID)=MODE

 end function

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
 integer function ioWF_ver_2(ID,wf)
!==================================
 use pars,             ONLY:SP,schlen
 use electrons,        ONLY:n_bands,n_spin
 use R_lattice,        ONLY:nkibz
 use wave_func,        ONLY:wf_ng_disk,ng_wf_k
 use IO_m,             ONLY:io_connect, io_disconnect, io_sec, io_bulk,&
&                           io_elemental,io_unit,io_netcdf_support,io_file,&
&                           read_is_on
 implicit none
 integer,       intent(in) :: ID
 complex(SP)               :: wf(n_bands,wf_ng_disk,n_spin)
!
 integer :: I_on_disk(2),ik,ib
 character(schlen) :: VAR_NAME
 real(SP)          :: wf_on_disk(2,n_bands*wf_ng_disk)

 ik=maxval(io_sec(ID,:))

 ioWF_ver_2 =io_connect(desc="s.db2",type=-1,ID=ID)

 if (ioWF_ver_2/=0) goto 1

 if (ik==1) then
   call io_elemental(ID,VAR="HID",VAR_SZ=2)
   call io_elemental(ID,I1=I_on_disk)
 endif
!
 if (io_netcdf_support(ID)) then

   write (VAR_NAME,'(a,i5.5,a)') 'wf-k',ik,'CDr'
   call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/n_bands*ng_wf_k(ik)/))
   call io_bulk(ID,R1=wf_on_disk(1,:n_bands*ng_wf_k(ik)))
!
   write (VAR_NAME,'(a,i5.5,a)') 'wf-k',ik,'CDi'
   call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/n_bands*ng_wf_k(ik)/))
   call io_bulk(ID,R1=wf_on_disk(2,:n_bands*ng_wf_k(ik)))

   do ib=1,n_bands
     wf(ib,:ng_wf_k(ik),1)=cmplx(wf_on_disk(1,(ib-1)*ng_wf_k(ik)+1:ib*ng_wf_k(ik)),&
&                              wf_on_disk(2,(ib-1)*ng_wf_k(ik)+1:ib*ng_wf_k(ik)),SP)
     wf(ib,ng_wf_k(ik)+1:,1)=cmplx(0.,0.,SP)
   enddo
 else
   do ib=1,n_bands
     call io_bulk(ID,C1=wf(ib,:ng_wf_k(ik),1))
     wf(ib,ng_wf_k(ik)+1:,1)=cmplx(0.,0.,SP)
   enddo
 endif

1 call io_disconnect(ID=ID)

 end function ioWF_ver_2

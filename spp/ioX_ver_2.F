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
 integer function ioX_ver_2(X,Xw,ID)
!===================================
 use pars,             ONLY:SP,schlen
 use IO_m,             ONLY:io_connect, io_disconnect, io_netcdf_support, &
&                           io_elemental, read_is_on,io_sec,io_bulk
 use X_m,              ONLY:X_t,X_mat
 use frequency,        ONLY:w_samp
 implicit none
 integer     :: ID
 type(X_t)   :: X
 type(w_samp):: Xw
!
!WS on disk
!
 integer  :: iq,i1,i2,i3
 integer,    allocatable :: I_on_disk(:)
 real(SP),   allocatable :: R_on_disk(:)
 complex(SP),allocatable :: C_on_disk(:)
 character(schlen)    :: VAR_NAME
 character(100)       :: ch_on_disk

 if (X%whoami==2) ioX_ver_2 =io_connect(desc="db.em1s",type=-1,ID=ID)
 if (X%whoami==4) ioX_ver_2 =io_connect(desc="db.pp",type=-1,ID=ID)
 if (ioX_ver_2/=0) goto 1

 if (any ((/io_sec(ID,:)==1/)) ) then
!
! H_versionID 
!
   allocate(I_on_disk(4))
   call io_elemental(ID,VAR="H_versionID",VAR_SZ=size(I_on_disk))
   call io_elemental(ID,I1=I_on_disk)
   deallocate(I_on_disk)
!
! H_BZID 
!
   allocate(I_on_disk(2))
   call io_elemental(ID,VAR="H_BZID",VAR_SZ=size(I_on_disk))
   call io_elemental(ID,I1=I_on_disk)
   deallocate(I_on_disk)
!
! H_temperatureID 
!
   allocate(I_on_disk(2))
   call io_elemental(ID,VAR="H_temperatureID",VAR_SZ=size(I_on_disk))
   call io_elemental(ID,I1=I_on_disk)
   deallocate(I_on_disk)
!
! H_temperatureRD 
!
   allocate(R_on_disk(2))
   call io_elemental(ID,VAR="H_temperatureID",VAR_SZ=size(R_on_disk))
   call io_elemental(ID,R1=R_on_disk)
   deallocate(R_on_disk)
!
! H_fftID 
!
   allocate(I_on_disk(4))
   call io_elemental(ID,VAR="H_fftID",VAR_SZ=size(I_on_disk))
   call io_elemental(ID,I1=I_on_disk)
   deallocate(I_on_disk)

   do i1=1,3

     if (i1==1) VAR_NAME='H_XQPRD'
     if (i1==2) VAR_NAME='H_KQPRD'
     if (i1==3) VAR_NAME='H_GQPRD'
     allocate(R_on_disk(4))
     call io_elemental(ID,VAR=trim(VAR_NAME),VAR_SZ=size(R_on_disk))
     call io_elemental(ID,R1=R_on_disk)
     deallocate(R_on_disk)

     if (io_netcdf_support(ID)) then
       if (i1==1) VAR_NAME='H_XQPCDr'
       if (i1==2) VAR_NAME='H_KQPCDr'
       if (i1==3) VAR_NAME='H_GQPCDr'
       allocate(R_on_disk(1))
       call io_elemental(ID,VAR=trim(VAR_NAME),VAR_SZ=size(R_on_disk))
       call io_elemental(ID,R1=R_on_disk)
       deallocate(R_on_disk)

       if (i1==1) VAR_NAME='H_XQPCDi'
       if (i1==2) VAR_NAME='H_KQPCDi'
       if (i1==3) VAR_NAME='H_GQPCDi'
       allocate(R_on_disk(1))
       call io_elemental(ID,VAR=trim(VAR_NAME),VAR_SZ=size(R_on_disk))
       call io_elemental(ID,R1=R_on_disk)
       deallocate(R_on_disk)
     else
       allocate(C_on_disk(1))
       call io_bulk(ID,C1=C_on_disk)
       deallocate(C_on_disk)
     endif

     if (i1==1) VAR_NAME='H_XQPHD'
     if (i1==2) VAR_NAME='H_KQPHD'
     if (i1==3) VAR_NAME='H_GQPHD'
     call io_elemental(ID,VAR=trim(VAR_NAME),CH0="",VAR_SZ=1)
     call io_elemental(ID,CH0=ch_on_disk)

   enddo

 endif

 iq=maxval(io_sec(ID,:))-1

 if (iq==0) goto 1

 write (VAR_NAME,'(a,i4.4,a)') 'H_iq',iq,'_Wsec0001ID'
 allocate(I_on_disk(6))
 call io_elemental(ID,VAR=trim(VAR_NAME),VAR_SZ=size(I_on_disk))
 call io_elemental(ID,I1=I_on_disk)

 write (VAR_NAME,'(a,i4.4,a)') 'H_iq',iq,'_Wsec0001RD'
 allocate(R_on_disk(10))
 call io_elemental(ID,VAR=trim(VAR_NAME),VAR_SZ=size(R_on_disk))
 call io_elemental(ID,R1=R_on_disk)

 write (VAR_NAME,'(a,i4.4,a)') 'H_iq',iq,'_Wsec0001HD'
 call io_elemental(ID,VAR=trim(VAR_NAME),CH0="",VAR_SZ=1)
 call io_elemental(ID,CH0=ch_on_disk)

 X%ib=I_on_disk(:2)
 X%ng_db=I_on_disk(3)
 X%ng   =I_on_disk(3)
 Xw%n=I_on_disk(4:5)

 X%ehe=R_on_disk(:2)
 X%cg_percentual=R_on_disk(3)
 Xw%er=R_on_disk(4:5)
 Xw%dr=R_on_disk(6:7)
 X%q0 =R_on_disk(8:10)

 deallocate(R_on_disk)
 deallocate(I_on_disk)

 if (read_is_on(ID).and..not.allocated(X_mat)) then 
   allocate(X_mat(X%ng_db,X%ng_db,Xw%n(1)))
   call freqs_setup(Xw)
 endif

 if (io_netcdf_support(ID)) then
   allocate(R_on_disk(X%ng_db))

   write (VAR_NAME,'(a,i4.4,a)') 'X_iq',iq,'_Wsec0001CDr'
   call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/X%ng_db*X%ng_db*Xw%n(1)/))
   do i1=1,Xw%n(1)
     do i2=1,X%ng_db
       call io_bulk(ID,R1=R_on_disk)
       X_mat(:,i2,i1)=R_on_disk(:)
     enddo
   enddo

   write (VAR_NAME,'(a,i4.4,a)') 'X_iq',iq,'_Wsec0001CDi'
   call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/X%ng_db*X%ng_db*Xw%n(1)/))
   do i1=1,Xw%n(1)
     do i2=1,X%ng_db
       call io_bulk(ID,R1=R_on_disk)
       X_mat(:,i2,i1)=X_mat(:,i2,i1)+(0._SP,1._SP)*R_on_disk(:)
     enddo
   enddo

   deallocate(R_on_disk)
 else
   do i1=1,Xw%n(1)
     do i2=1,X%ng_db
       call io_bulk(ID,C1=X_mat(:,i2,i1))
     enddo
   enddo
 endif

1 call io_disconnect(ID=ID)

 end function ioX_ver_2

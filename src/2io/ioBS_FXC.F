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
 integer function ioBS_Fxc(iq,FXC_W,ID)
!======================================
 use pars,           ONLY:SP,schlen,HARTREE
 use memory_m,       ONLY:mem_est
 use BS,             ONLY:BS_identifier,BSS_q0,BS_W_is_diagonal,&
&                         BS_eh_en,BS_bands,BS_K_dim
 use TDDFT,          ONLY:FXC_n_g_corr,FXC_n_descs,FXC_description,&
&                         FXC_K_diagonal,FXC_n_mem_freqs,FXC,FXC_is_causal
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 use frequency,      ONLY:w_samp
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,&
&                         io_elemental,io_status,io_bulk,&
&                         read_is_on,write_is_on,io_header
 implicit none
 type(w_samp):: FXC_W
 integer     :: ID,iq
!ws
 integer             ::i1,FXC_n_g_db,w_group,&
&                      BS_identifier_disk,FXC_n_descs_disk
 character (schlen)  ::db_name,ch
 real(SP),allocatable::FXC_disk(:,:,:),W_disk(:,:)
!
 w_group=1
 if (maxval(io_sec(ID,:))>2.and.FXC_W%n(1)>1) then
   w_group=maxval(io_sec(ID,:)-2)/FXC_n_mem_freqs+1
 endif
!
 write (db_name,'(2(a,i2.2))') 'BS_FXC_Q',iq,'_W_group_',w_group
!
 ioBS_Fxc=io_connect(desc=trim(db_name),type=2,ID=ID)

 if (ioBS_Fxc/=0) goto 1
!
 if (any((/io_sec(ID,:)==1/))) then
!
! As this DB must respect the BS ideentifier I only need to
! check the K QP corrections
!
   ioBS_Fxc=io_header(ID,QP_ctl_K=.true.)

   call io_elemental(ID,VAR="PARS",VAR_SZ=19,MENU=0)

   BS_identifier_disk=BS_identifier
   call io_elemental(ID,&
&       VAR=" BSK|Identifier              :",I0=BS_identifier,CHECK=.true.,OP=(/"=="/))
!
! I cannot overwrite the BS_identifier here as this is strictly 
! allowed only in ioBS.
!
   if (BS_identifier_disk/=0) BS_identifier=BS_identifier_disk

   call io_elemental(ID,&
&       VAR="    |Dimension               :",I0=BS_K_dim,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR="    |Bands                   :",I1=BS_bands,CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,UNIT=HARTREE,&
&       VAR="    |E/h energy range    [ev]:",R1=BS_eh_en,CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,&
&       VAR="    |Kernel W is diagonal    :",L0=BS_W_is_diagonal,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,DB_I0=FXC_n_g_db,&
&       VAR=" FXC|Matrix size             :",I0=FXC_n_g_corr,CHECK=.true.,OP=(/"<="/))
   call io_elemental(ID,&
&       VAR="    |Kernel is causal        :",L0=FXC_is_causal,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,UNIT=HARTREE,&
&       VAR="    |Energy range        [ev]:",R1=FXC_W%er,CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,UNIT=HARTREE,&
&       VAR="    |Damping range       [ev]:",R1=FXC_W%dr,CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,&
&       VAR="    |Total Energy steps      :",I0=FXC_W%n(1),CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR="    |Memory Energy steps     :",I0=FXC_n_mem_freqs,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR="    |Field direction         :",R1=BSS_q0,CHECK=.true.,OP=(/"==","==","=="/))
   call io_elemental(ID,I0=FXC_n_descs,DB_I0=FXC_n_descs_disk)

   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
!
! Exit if header is wrong
!
   if (ioBS_Fxc/=0) goto 1

   if (read_is_on(ID)) FXC_n_descs=FXC_n_descs_disk
   do i1=1,FXC_n_descs_disk
     write (ch,'(a,i3.3)') 'DESC_strings_',i1
     call io_elemental(ID,VAR=trim(ch),CH0="",VAR_SZ=1,MENU=0)
     call io_elemental(ID,CH0=FXC_description(i1))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   enddo

   ioBS_Fxc=io_status(ID)
   if (ioBS_Fxc/=0.or.maxval(io_sec(ID,:))==1) goto 1

 endif

 if (any((/io_sec(ID,:)==2/))) then

   call io_bulk(ID,VAR='FREQUENCIES',VAR_SZ=(/FXC_W%n(1),2/))
   allocate(W_disk(FXC_W%n(1),2))

   if (write_is_on(ID)) then
     W_disk(:,1)=real(FXC_W%p(:))
     W_disk(:,2)=aimag(FXC_W%p(:))
   endif

   call io_bulk(ID,R2=W_disk)

   if (read_is_on(ID)) then
     if (associated(FXC_W%p)) then
       deallocate(FXC_W%p)
       call mem_est("W-p")
     endif
     allocate(FXC_W%p(FXC_W%n(1)))
     call mem_est("W-p",(/FXC_W%n(1)/))
     FXC_W%p(:)=cmplx(W_disk(:,1),W_disk(:,2))
   endif

   deallocate(W_disk)

   if (.not.allocated(FXC_K_diagonal)) allocate(FXC_K_diagonal(BS_K_dim))

   call io_bulk(ID,VAR='FXC_K_diagonal',VAR_SZ=(/BS_K_dim/))
   call io_bulk(ID,R1=FXC_K_diagonal)

 endif

 if (maxval(io_sec(ID,:))==2) goto 1

 call io_bulk(ID,VAR='FXC_RL_SIZE',VAR_SZ=(/1/))
 if (write_is_on(ID)) FXC_n_g_db=FXC_n_g_corr
 call io_bulk(ID,I0=FXC_n_g_db)

 allocate(FXC_disk(FXC_n_g_db,FXC_n_g_db,2))
!
 write (ch,'(a,i3.3)') "FXC_W_GROUP_",w_group
!
 call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/FXC_n_g_db,FXC_n_g_db,2,FXC_n_mem_freqs/))
!
 do i1=1,FXC_n_mem_freqs
!
   if (write_is_on(ID)) call mat_c2r(FXC(:,:,i1),FXC_disk)
!
   call io_bulk(ID,R3=FXC_disk,IPOS=(/1,1,1,i1/))
!
   if (read_is_on(ID)) call mat_r2c(FXC_disk,FXC(:,:,i1))
!
 enddo
!
  deallocate(FXC_disk)
!
1 call io_disconnect(ID)
!
 end function

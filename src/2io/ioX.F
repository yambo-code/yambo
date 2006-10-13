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
 integer function ioX(X,Xw,ID)
!=============================
 use pars,          ONLY:SP,schlen,HARTREE
 use memory_m,      ONLY:mem_est
 use stderr,        ONLY:string_pack
 use electrons,     ONLY:BZ_RIM_tot_nkpts
 use R_lattice,     ONLY:nqibz
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use X_m,           ONLY:X_t,X_mat,use_X_RIM
 use frequency,     ONLY:w_samp
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,io_com,&
&                        io_elemental,io_status,io_bulk,io_restart,&
&                        read_is_on,write_is_on,io_check_restart,io_header,&
&                        io_fragment,RD_CL,RD_CL_IF_END,io_action,io_mode,io_unit,&
&                        version_is_newer
 implicit none
 type(X_t)   :: X
 type(w_samp):: Xw
 integer     :: ID
!ws
 integer             ::sec_size,i1,iq
 character (schlen)  ::ch,db_desc
 real(SP),allocatable::X_disk(:,:,:),W_disk(:,:)
!
!Integers for alternative PATH search
!
 integer             :: io_unit_save,io_action_save,io_mode_save,io_sec_save(10),&
&                       io_com_save

 if (X%whoami==1) db_desc='Xx'
 if (X%whoami==2) db_desc='em1s'
 if (X%whoami==3) db_desc='em1d'
 if (X%whoami==4) db_desc='pp'
!
 io_unit_save=io_unit(ID)
 io_action_save=io_action(ID)
 io_mode_save=io_mode(ID)
 io_sec_save=io_sec(ID,:)
 io_com_save=io_com(ID)

 ioX=io_connect(desc=trim(db_desc),type=2,ID=ID) 

 if (read_is_on(ID).and.X%whoami==2.and.ioX/=0) then
   io_unit(ID)=io_unit_save 
   io_action(ID)=io_action_save 
   io_mode(ID)=io_mode_save 
   io_sec(ID,:)=io_sec_save 
   io_com(ID)=io_com_save 
   ioX=io_connect(desc=trim(db_desc),type=1,ID=ID) 
 endif
 if (ioX/=0) goto 1
!
! Check the restart point
!
 call io_check_restart(trim(db_desc),ID)

 if (any((/io_sec(ID,:)==1/))) then

   ioX=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,T_EL=.true.)
   if (ioX/=0) goto 1

   call io_elemental(ID,VAR="PARS_1",VAR_SZ=5,MENU=0)
   call io_elemental(ID,DB_I0=X%ng_db,&
&       VAR=" X matrix size          :",I0=X%ng,CHECK=.true.,OP=(/"<="/))
   call io_elemental(ID,&
&       VAR=" X band range           :",I1=X%ib,CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,UNIT=HARTREE,&
&       VAR=" X e/h energy range [ev]:",R1=X%ehe,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)

   call io_elemental(ID,VAR="PARS_2",VAR_SZ=1,CH0="",MENU=0)
   call io_elemental(ID,&
&       VAR=" X Time ordering        :",CH0=X%ordering,CHECK=.true.)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)

   sec_size=6

   if (version_is_newer(ID,(/3,0,1/))) sec_size=8
   call io_elemental(ID,VAR="PARS_3",VAR_SZ=sec_size,MENU=0)
   call io_elemental(ID,&
&       VAR=" X poles           [o/o]:",R0=X%cg_percentual,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR=" Rl vectors in the sum  :",I0=X%ngostnts,WARN=.true.,OP=(/"<="/))
   call io_elemental(ID,&
&       VAR=" [r,Vnl] included       :",L0=X%Vnl_included,WARN=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR=" Field direction        :",R1=X%q0,CHECK=.true.,OP=(/"==","==","=="/))
!
! BZ Energy RIM
!
   if (version_is_newer(ID,(/3,0,1/))) then
     call io_elemental(ID,&
&         VAR=" BZ energy Random IM    :",L0=use_X_RIM,WARN=.true.,OP=(/"=="/))
     call io_elemental(ID,&
&         VAR=" BZ energy RIM points   :",I0=BZ_RIM_tot_nkpts,CHECK=.true.,OP=(/"=="/))
   endif

   if (.not.any((/io_sec(ID,:)==2/))) then
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
     ioX=io_status(ID)
     if (ioX/=0) goto 1
   else
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif

 endif

 do i1=1,size(io_sec(ID,:))
   if (io_sec(ID,i1)==0) cycle
   iq=io_sec(ID,i1)/2
 enddo
 if (iq==0) goto 1
!
! Manage RD_CL_IF_END
!
 if (io_action(ID)==RD_CL_IF_END.and.iq==nqibz) io_action(ID)=RD_CL
!
! Fragmentation
!---------------
!
 call io_fragment(ID,i_pointer=iq)
!
!  type(X_t)    ::X(4) ! x s d p
!
 if (any((/io_sec(ID,:)==2*iq/))) then
   sec_size=6

   if (X%whoami==4) sec_size=1
   write (ch,'(a,i3.3)') "FREQ_PARS_sec_iq",iq
   if (io_sec(ID,1)==1) call io_elemental(ID,VAR=trim(ch),VAR_SZ=sec_size,MENU=0)
   if (io_sec(ID,1)/=1) call io_elemental(ID,VAR=trim(ch),VAR_SZ=sec_size,MENU=1)
   if (X%whoami==4) then
     call io_elemental(ID,UNIT=HARTREE,&
&         VAR=" PPA Im  energy     [ev]:",R0=Xw%dr(2),CHECK=.true.,OP=(/"=="/))
   endif
   if (X%whoami/=4) then
     call io_elemental(ID,&
&         VAR=" :: Current Q-pt index     :",I0=iq)
     call io_elemental(ID,UNIT=HARTREE,&
&         VAR=" :: X energy range     [ev]:",R1=Xw%er,CHECK=.true.,OP=(/"==","=="/))
     call io_elemental(ID,UNIT=HARTREE,&
&         VAR=" :: X damping range    [ev]:",R1=Xw%dr,CHECK=.true.,OP=(/"==","=="/))
     call io_elemental(ID,&
&           VAR=" :: Number of frequencies  :",I0=Xw%n(1),CHECK=.true.,OP=(/"=="/))
   endif

   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   ioX=io_status(ID)
   if (ioX/=0) goto 1

 endif
!
! Sections 1 & 2 are used in VERIFY mode. 
! Now that the menu is closed I can return with ioX/=0 if
! there is a restart point
!
 ioX=io_restart(ID)
 if (ioX/=0) goto 1

 if (any((/io_sec(ID,:)==2*iq+1/))) then
   allocate(X_disk(X%ng_db,X%ng_db,2))

   write (ch,'(a,i3.3)') "FREQ_sec_iq",iq
   call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/Xw%n(1),2/))
   allocate(W_disk(Xw%n(1),2))

   if (write_is_on(ID)) then
     W_disk(:,1)=real(Xw%p(:))
     W_disk(:,2)=aimag(Xw%p(:))
   endif

   call io_bulk(ID,R2=W_disk)

   if (read_is_on(ID)) then
     if (associated(Xw%p)) then
       deallocate(Xw%p)
       call mem_est("W-p")
     endif
     allocate(Xw%p(Xw%n(1)))
     call mem_est("W-p",(/Xw%n(1)/))
     Xw%p(:)=cmplx(W_disk(:,1),W_disk(:,2))
   endif

   deallocate(W_disk)

   write (ch,'(a,i3.3)') "X_Q_",iq

   call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/X%ng_db,X%ng_db,2,Xw%n(1)/))

   do i1=1,Xw%n(1)

     if (write_is_on(ID)) call mat_c2r(X_mat(:,:,i1),X_disk)

     call io_bulk(ID,R3=X_disk,IPOS=(/1,1,1,i1/))

     if (read_is_on(ID)) call mat_r2c(X_disk,X_mat(:,:,i1))

   enddo

   deallocate(X_disk)
 endif

 call io_check_restart(trim(db_desc),ID,now=iq,todo=nqibz)
!
! No I/O errors I update Xio at the first q TODO
!
 ioX=io_restart(ID)

1 call io_disconnect(ID)

 end function

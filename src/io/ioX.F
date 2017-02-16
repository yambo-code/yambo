!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
integer function ioX(X,Xw,ID)
 !
 use pars,          ONLY:SP,schlen,HARTREE
 use memory_m,      ONLY:mem_est
 use R_lattice,     ONLY:nqibz,g_vec
 use matrix_operate,ONLY:mat_c2r,mat_r2c,mat_r2c_ordered
 use X_m,           ONLY:X_t,X_mat,use_X_RIM,X_RLcomp_ig,X_RIM_nkpts
 use frequency,     ONLY:w_samp
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,io_com,&
&                        io_elemental,io_status,io_bulk,io_restart,&
&                        read_is_on,write_is_on,io_check_restart,io_header,&
&                        io_fragment,RD_CL,RD_CL_IF_END,io_action,&
&                        ver_is_gt_or_eq,io_serial_number,serial_number,&
&                        io_save_to_try_again,io_restore_to_try_again,NONE
 implicit none
 type(X_t)   :: X
 type(w_samp):: Xw
 integer     :: ID
 !
 !Work Space
 !
 integer             ::sec_size,i1,iq,i_err,io_com_save
 logical             ::different_db_RL_order
 character (schlen)  ::ch,db_desc
 real(SP)            ::Wd_disk(2)
 real(SP),allocatable::X_disk(:,:,:),W_disk(:,:),RL_vecs_disk(:,:)
 !
 ! I assume that he RL ordering used to build X is not different
 ! from the one in DB1
 !
 different_db_RL_order=.false.
 !
 if (X%whoami==1) db_desc='Xx'
 if (X%whoami==2) db_desc='em1s'
 if (X%whoami==3) db_desc='em1d'
 if (X%whoami==4) db_desc='pp'
 !
 call io_save_to_try_again(ID)
 ioX=io_connect(desc=trim(db_desc),type=2,ID=ID) 
 !
 ! The em1s DB is searched in the SAVE folder when 
 ! it is not found in the JOBNAME folder
 !
 if (read_is_on(ID).and.ioX/=0) then
   call io_restore_to_try_again(ID)
   ioX=io_connect(desc=trim(db_desc),type=1,ID=ID) 
 endif
 if (ioX/=0) goto 1
 !
 ! Check the restart point
 !
 call io_check_restart(trim(db_desc),ID)

 if (any((/io_sec(ID,:)==1/))) then
   !
   ! Header
   !
   if (X%whoami==1) ioX=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,T_EL=.true.,XC_KIND="Xx")
   if (X%whoami==2) ioX=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,T_EL=.true.,XC_KIND="Xs")
   if (X%whoami==3) ioX=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,T_EL=.true.,XC_KIND="Xd")
   if (X%whoami==4) ioX=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,T_EL=.true.,XC_KIND="Xp")
   if (ioX/=0) goto 1
   !
   ! PARS_1
   !
   call io_elemental(ID,VAR="PARS_1",VAR_SZ=5,MENU=0)
   call io_elemental(ID,DB_I0=X%ng_db,&
&       VAR=" X matrix size          :",I0=X%ng,CHECK=.true.,OP=(/"<="/))
   call io_elemental(ID,&
&       VAR=" X band range           :",I1=X%ib,CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,UNIT=HARTREE,&
&       VAR=" X e/h energy range [ev]:",R1=X%ehe,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! PARS_2
   !
   call io_elemental(ID,VAR="PARS_2",VAR_SZ=1,CH0="",MENU=0)
   call io_elemental(ID,&
&       VAR=" X Time ordering        :",CH0=X%ordering,CHECK=.true.)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   if (ver_is_gt_or_eq(ID,(/3,0,5/))) then
     !
     ! TDDFT_KERNEL
     !
     call io_elemental(ID,VAR="TDDFT_KERNEL",VAR_SZ=1,CH0="",MENU=0)
     call io_elemental(ID,&
&       VAR=" X xc-Kernel            :",CH0=X%f_xc,CHECK=.true.)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     ! DRUDE
     !
     call io_elemental(ID,VAR="DRUDE",VAR_SZ=2,MENU=0)
     Wd_disk=(/real(X%Wd),aimag(X%Wd)/)
     call io_elemental(ID,UNIT=HARTREE,&
&       VAR=" X Drude frequency      :",R1=Wd_disk,CHECK=.true.,OP=(/"==","=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     X%Wd=Wd_disk(1)+(0.,1.)*Wd_disk(2)
     !
   endif
   !
   ! PARS_3
   !
   sec_size=6
   if (ver_is_gt_or_eq(ID,(/3,0,1/))) sec_size=8
   if (ver_is_gt_or_eq(ID,(/3,0,9/))) sec_size=9
   call io_elemental(ID,VAR="PARS_3",VAR_SZ=sec_size,MENU=0)
   call io_elemental(ID,&
&       VAR=" X poles           [o/o]:",R0=X%cg_percentual,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR=" Rl vectors in the sum  :",I0=X%ngostnts,WARN=.true.,OP=(/"<="/))
   call io_elemental(ID,&
&       VAR=" [r,Vnl] included       :",L0=X%Vnl_included,WARN=.true.,OP=(/"=="/))
   if (ver_is_gt_or_eq(ID,(/3,0,9/))) then
     call io_elemental(ID,&
&         VAR=" Longitudinal Gauge     :",L0=X%long_gauge,WARN=.true.,OP=(/"=="/))
   endif
   call io_elemental(ID,&
&       VAR=" Field direction        :",R1=X%q0,CHECK=.true.,OP=(/"==","==","=="/))
   !
   if (ver_is_gt_or_eq(ID,(/3,0,1/))) then
     call io_elemental(ID,&
&         VAR=" BZ energy Random IM    :",L0=use_X_RIM,WARN=.true.,OP=(/"=="/))
     call io_elemental(ID,&
&         VAR=" BZ energy RIM points   :",I0=X_RIM_nkpts,CHECK=.true.,OP=(/"=="/))
   endif
   !
   ! When the SN is different and I am importing this DB 
   ! from a different calculation I check the RL vectors
   !
   if (ver_is_gt_or_eq(ID,(/3,0,6/))) then
     !
     allocate(RL_vecs_disk(X%ng_db,3))
     !
     if (write_is_on(ID)) RL_vecs_disk(:,:)=g_vec(:X%ng_db,:)
     !
     call io_bulk(ID,VAR='X_RL_vecs',VAR_SZ=shape(RL_vecs_disk))
     call io_bulk(ID,R2=RL_vecs_disk)
     !
     if (io_serial_number(ID)/=serial_number.and.read_is_on(ID).and.&
&        allocated(g_vec)) then
       if (.not.allocated(X_RLcomp_ig)) allocate(X_RLcomp_ig(X%ng))
       i_err = X_RLcomp_table() 
       if ( i_err < 0 ) io_status(ID) = -1
       if ( i_err > 0 ) different_db_RL_order=.true.
     endif
     deallocate(RL_vecs_disk)
     !
   endif
   !
   ! Close & Menu lower line
   !
   if (.not.any((/io_sec(ID,:)==2/))) then
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
     ioX=io_status(ID)
     if (ioX/=0) goto 1
   else
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
 endif
 !
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
 !
 call io_fragment(ID,i_pointer=iq)
 !
 !  type(X_t)    ::X(4)  x s d p
 !
 if (any((/io_sec(ID,:)==2*iq/))) then
   sec_size=6
   !
   if (X%whoami==4) sec_size=1
   write (ch,'(a,i3.3)') "FREQ_PARS_sec_iq",iq
   !
   io_com_save=io_com(ID)
   if (io_sec(ID,1)/=1) io_com(ID)=NONE
   !
   if (io_sec(ID,1)==1) call io_elemental(ID,VAR=trim(ch),VAR_SZ=sec_size,MENU=0)
   if (io_sec(ID,1)/=1) call io_elemental(ID,VAR=trim(ch),VAR_SZ=sec_size,MENU=1)
   !
   if (X%whoami==4) then
     call io_elemental(ID,UNIT=HARTREE,&
&         VAR=" PPA Im  energy     [ev]:",R0=X%ppaE,CHECK=.true.,OP=(/"=="/))
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
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   ioX=io_status(ID)
   !
   io_com(ID)=io_com_save
   !
   if (ioX/=0) goto 1
   !
 endif
 !
 ! Sections 1 & 2 are used in VERIFY mode. 
 ! Now that the menu is closed I can return with ioX/=0 if
 ! there is a restart point
 !
 ioX=io_restart(ID)
 if (ioX/=0) goto 1
 !
 if (any((/io_sec(ID,:)==2*iq+1/))) then
   allocate(X_disk(X%ng_db,X%ng_db,2))
   !
   write (ch,'(a,i3.3)') "FREQ_sec_iq",iq
   call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/Xw%n(1),2/))
   allocate(W_disk(Xw%n(1),2))
   !
   if (write_is_on(ID)) then
     W_disk(:,1)=real(Xw%p(:))
     W_disk(:,2)=aimag(Xw%p(:))
   endif
   !
   call io_bulk(ID,R2=W_disk)
   !
   if (read_is_on(ID)) then
     if (associated(Xw%p)) then
       deallocate(Xw%p)
       call mem_est("W-p")
     endif
     allocate(Xw%p(Xw%n(1)))
     call mem_est("W-p",(/Xw%n(1)/))
     Xw%p(:)=cmplx(W_disk(:,1),W_disk(:,2))
   endif
   !
   deallocate(W_disk)
   !
   write (ch,'(a,i3.3)') "X_Q_",iq
   !
   call io_bulk(ID,VAR=trim(ch),VAR_SZ=(/X%ng_db,X%ng_db,2,Xw%n(1)/))
   !
   do i1=1,Xw%n(1)
     !
     if (write_is_on(ID)) call mat_c2r(X_mat(:,:,i1),X_disk)
     !
     call io_bulk(ID,R3=X_disk,IPOS=(/1,1,1,i1/))
     !
     if (read_is_on(ID)) then
       if (different_db_RL_order)  then
         call mat_r2c_ordered(X_disk,X_mat(:,:,i1),X_RLcomp_ig)
       else
         call mat_r2c(X_disk,X_mat(:,:,i1))
       endif
     endif
     !
   enddo
   !
   deallocate(X_disk)
 endif

 call io_check_restart(trim(db_desc),ID,now=iq,todo=nqibz)
 !
 ! No I/O errors I update Xio at the first q TODO
 !
 ioX=io_restart(ID)
 !
1 call io_disconnect(ID)
 !
 !
 contains
   !
   integer function X_RLcomp_table()
   !--------------------------------
   !
   ! X has been written using a different RL vectors ordering.
   ! X_RLcomp_ig is defined in such a way that
   !
   ! X_( X_RLcomp_ig(i), X_RLcomp_ig(j) ) = X_on_disk_(i,j)
   !
   use vec_operate,   ONLY:v_is_zero
   use R_lattice,     ONLY:ng_in_shell,n_g_shells
   use zeros,         ONLY:define_zeros
   !
   integer :: is,ig1,ig2,g_limits(2)
   real(SP):: v(3),local_zero(3)
   !
   X_RLcomp_table=0
   X_RLcomp_ig=0
   !
   do is=1,n_g_shells
    if (ng_in_shell(is) > X%ng) then
      if (any(X_RLcomp_ig==0)) X_RLcomp_table=-1
      return
    endif
    !
    g_limits=1
    if (is > 1 ) g_limits = (/ ng_in_shell(is-1), ng_in_shell(is) /)
    !
    call define_zeros(vector_=g_vec,zero_=local_zero)
    !
    g_loop: do ig1=g_limits(1),g_limits(2)
      do ig2=g_limits(1),g_limits(2)
        !
        v=RL_vecs_disk(ig1,:)-g_vec(ig2,:)
        !
        if (v_is_zero(v,zero_=local_zero)) then
          X_RLcomp_ig(ig1)=ig2
          cycle g_loop
        endif
        !
      enddo
    enddo g_loop
   enddo
   !
   end function
   !
end function

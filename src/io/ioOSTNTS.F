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
integer function ioOSTNTS(X,Xen,ID)
 !
 use pars,          ONLY:SP,HARTREE,schlen
 use X_m,           ONLY:X_t,X_q0grad,X_alloc,P_bands_ordered
 use electrons,     ONLY:levels,n_sp_pol
 use R_lattice,     ONLY:nXkibz,q0_def_norm
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,&
&                        io_elemental,io_status,io_bulk,io_save_to_try_again,&
&                        read_is_on,write_is_on,io_header,ver_is_gt_or_eq,&
&                        io_restore_to_try_again
 use global_XC,     ONLY:WF_global_xc_string
 !
 implicit none
 type(X_t)::X
 type(levels)::Xen
 integer     ::ID
 !
 ! Work Space
 !
 integer :: i1,ixyz,sec_size,i_spin
 integer :: db_nbm,db_nbf,db_nb(2)
 character(schlen)    :: VAR_name
 real(SP),allocatable :: X_q0grad_disk(:,:,:)
 !
 call io_save_to_try_again(ID)
 ioOSTNTS=io_connect(desc='ostnts',type=2,ID=ID)
 !
 ! The OSTNTS DB is searched in the SAVE folder when 
 ! it is not found in the JOBNAME folder
 !
 if (read_is_on(ID).and.ioOSTNTS/=0) then
   call io_restore_to_try_again(ID)
   ioOSTNTS=io_connect(desc='ostnts',type=1,ID=ID)
 endif
 if (ioOSTNTS/=0) goto 1
 !
 sec_size=8
 ! 
 ! In V 3.0.8 q0_def_norm is stored in case a longitudinal
 ! gauge grid has already redefined the q0 norm
 !
 if (ver_is_gt_or_eq(ID,(/3,0,9/)))  sec_size=10
 if (ver_is_gt_or_eq(ID,(/3,0,15/))) sec_size=11
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ioOSTNTS=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,T_EL=.true.)
   if (ioOSTNTS/=0) goto 1
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=sec_size,MENU=0)
   call io_elemental(ID,DB_I1=db_nb,&
&       VAR=" X band range           :",I1=X%ib,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(ID,UNIT=HARTREE,&
&       VAR=" X e/h energy range [ev]:",R1=X%ehe,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(ID,DB_I0=db_nbm,&
&       VAR=" Metallic bands         :",I0=Xen%nbm,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,DB_I0=db_nbf,&
&       VAR=" Filled bands           :",I0=Xen%nbf,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR=" RL vectors in the sum  :",I0=X%ngostnts,WARN=.true.,OP=(/"<="/))
   call io_elemental(ID,&
&       VAR=" [r,Vnl] included       :",L0=X%Vnl_included,CHECK=.true.,OP=(/"=="/))
   !
   if (ver_is_gt_or_eq(ID,(/3,0,15/))) then
     call io_elemental(ID,&
&       VAR=" Transitions ordered    :",L0=P_bands_ordered,CHECK=.true.,OP=(/"=="/))
   endif
   !
   if (ver_is_gt_or_eq(ID,(/3,0,9/))) then
     call io_elemental(ID,&
&       VAR=" Longitudinal Gauge     :",L0=X%long_gauge,CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,&
&       VAR=" Field momentum norm    :",R0=q0_def_norm,CHECK=.true.,OP=(/"=="/))
   endif
   !
   if (ver_is_gt_or_eq(ID,(/3,0,15/))) then
     !
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     ! Wavefunctions xc 
     !
     call io_elemental(ID,VAR='WAVE_FUNC_XC',CH0="",VAR_SZ=1,MENU=0)
     call io_elemental(ID,CH0=WF_global_xc_string,&
&         VAR=' Wavefunctions          :',CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0)
   else
     !
     call io_elemental(ID,VAR="",VAR_SZ=0)
     !
   endif
   !
   ioOSTNTS=io_status(ID)
   if (ioOSTNTS/=0) goto 1
 endif
 !
 ! On disk the size is X_q0grad(3,db_nb(2),db_nbm,nXkibz)
 !
 sec_size=3*db_nb(2)*db_nbm
 if (any((/io_sec(ID,:)==2/))) then
   !
   allocate(X_q0grad_disk(db_nb(2),db_nbm,2))
   !
   if (read_is_on(ID)) call X_alloc('OptGrad',(/3,X%ib(2),Xen%nbm,nXkibz/))
   !
   do i1=1,nXkibz
     !
     do ixyz=1,3
       !
       do i_spin=1,n_sp_pol
         !
         if (write_is_on(ID)) call mat_c2r(X_q0grad(ixyz,:,:,i1,i_spin),X_q0grad_disk)
         !
         write (VAR_name,'(3(a,i4.4))') 'OptGrad_k_',i1,'_xyz_',ixyz,'_spin_',i_spin
         call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(X_q0grad_disk))
         call io_bulk(ID,R3=X_q0grad_disk)
         !
         if (read_is_on(ID)) call mat_r2c(X_q0grad_disk,X_q0grad(ixyz,:,:,i1,i_spin))
         !
       enddo
       !
     enddo
     !
   enddo
   !
   deallocate(X_q0grad_disk)
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function

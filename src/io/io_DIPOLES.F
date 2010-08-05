!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
integer function io_DIPOLES(X,Xen,ID)
 !
 use pars,          ONLY:SP,schlen
 use units,         ONLY:HARTREE
 use X_m,           ONLY:X_t,DIP_iR,X_alloc,Dipole_bands_ordered,&
&                        Dipole_uses_shifted_grids
 use electrons,     ONLY:levels,n_sp_pol
 use R_lattice,     ONLY:nXkibz,q0_def_norm
 use matrix_operate,ONLY:mat_c2r,mat_r2c
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,io_control,&
&                        io_elemental,io_status,io_bulk,IO_OUTDATED_DB,&
&                        read_is_on,write_is_on,io_header,io_mode,io_com,io_action
 use global_XC,     ONLY:Dipole_WF_xc_string,loaded_WF_xc_string
 !
 implicit none
 type(X_t)::X
 type(levels)::Xen
 integer     ::ID
 !
 ! Work Space
 !
 integer :: i1,ixyz,sec_size,i_spin,IDP
 integer :: db_nbm,db_nbf,db_nb(2)
 character(schlen)    :: VAR_name
 logical              :: local_long_gauge  
 real(SP),allocatable :: DIP_iR_disk(:,:,:)
 !
 io_DIPOLES=io_connect(desc='dipoles',type=2,ID=ID)
 !
 if (io_DIPOLES/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_DIPOLES=io_header(ID,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,T_EL=.true.)
   if (io_DIPOLES/=0) goto 1
   !
   sec_size=16
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
   call io_elemental(ID,&
&       VAR=" Transitions ordered    :",L0=Dipole_bands_ordered,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR=" Using shifted grids    :",L0=Dipole_uses_shifted_grids,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR=" Field momentum norm    :",R0=q0_def_norm,CHECK=.true.,OP=(/"=="/))
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
   io_DIPOLES=io_status(ID)
   if (io_DIPOLES/=0) goto 1
 endif
 !
 if (.not.Dipole_bands_ordered) db_nbm=db_nb(2)
 !
 ! On disk the size is DIP_iR_or_P(3,db_nb(2),db_nbm,nXkibz)
 !
 sec_size=3*db_nb(2)*db_nbm
 if (any((/io_sec(ID,:)==2/))) then
   !
   allocate(DIP_iR_disk(db_nb(2),db_nbm,2))
   !
   if(read_is_on(ID)) call X_alloc('DIP_iR',(/3,X%ib(2),db_nbm,nXkibz/))
   !
   do i1=1,nXkibz
     !
     do ixyz=1,3
       !
       do i_spin=1,n_sp_pol
         !
         if (write_is_on(ID)) call mat_c2r(DIP_iR(ixyz,:,:,i1,i_spin),DIP_iR_disk)
         !
         write (VAR_name,'(3(a,i4.4))') 'DIP_iR_k_',i1,'_xyz_',ixyz,'_spin_',i_spin
         call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(DIP_iR_disk))
         call io_bulk(ID,R3=DIP_iR_disk)
         !
         if (read_is_on(ID)) call mat_r2c(DIP_iR_disk,DIP_iR(ixyz,:,:,i1,i_spin))
         !
       enddo
       !
     enddo
     !
   enddo
   !
   deallocate(DIP_iR_disk)
   !
 endif
 !
#if defined _RT
 !
 call io_control(ACTION=io_action(ID),COM=io_com(ID),SEC=io_sec(ID,:),MODE=io_mode(ID),ID=IDP) 
 io_DIPOLES=io_connect(desc='P_square',type=2,ID=IDP)
 !
 if (io_DIPOLES/=0) then
   call io_disconnect(ID=IDP)
   goto 1
 endif
 !
 sec_size=db_nb(2)*db_nbm
 if (any((/io_sec(ID,:)==2/))) then
   !
   allocate(DIP_iR_disk(db_nb(2),db_nbm,2))
   !
   if (read_is_on(ID)) call X_alloc('P_square',(/X%ib(2),X%ib(2),nXkibz/))
   !
   do i1=1,nXkibz
     !
     do i_spin=1,n_sp_pol
       !
       if (write_is_on(ID)) call mat_c2r(DIP_iR(4,:,:,i1,i_spin),DIP_iR_disk)
       !
       write (VAR_name,'(2(a,i4.4))') 'P_square_k_',i1,'_spin_',i_spin
       call io_bulk(IDP,VAR=trim(VAR_name),VAR_SZ=shape(DIP_iR_disk))
       call io_bulk(IDP,R3=DIP_iR_disk)
       !
       if (read_is_on(ID)) call mat_r2c(DIP_iR_disk,DIP_iR(4,:,:,i1,i_spin))
       !
     enddo
     !
   enddo
   !
   deallocate(DIP_iR_disk)
   !
 endif
 !
#endif
 !
1 call io_disconnect(ID=ID)
 !
end function

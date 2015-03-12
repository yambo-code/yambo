!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 integer function io_DIPOLES_cut(X,Xen,io_db)
!======================================
 use pars
 use units,            ONLY : HA2EV
 use X_m,              ONLY : X_t
 use electrons,        ONLY : levels, n_spinor, n_spin
 use R_lattice,        ONLY : nXkibz
 use matrix_operate,   ONLY : mat_c2r,mat_r2c
 use IO_m,             ONLY : io_connect,io_disconnect,io_sec,&
&                             io_elemental,io_status,io_bulk,&
&                             read_is_on,write_is_on,io_header
 use optcut,           ONLY : z0f, dcf, DIP_iR_cut
 use memory_m,         ONLY : mem_est
 implicit none
 type(X_t)                 :: X
 type(levels)              :: Xen
 integer                   :: io_db
!ws
 integer                   :: i1,ixyz,sec_size, i_spin
 integer                   :: db_nbm,db_nbf,db_nb(2),db_nspin
 character(schlen)         :: VAR_name, lch, tmp_VAR, tmp_xyz
 real(SP), allocatable     :: DIP_iR_cut_disk(:,:,:)

!io_DIPOLES=io_connect(desc='dipoles',type=2,ID=ID)
!if (io_DIPOLES/=0) goto 1

 io_DIPOLES_cut = io_connect(desc='dipoles_cut',type=2,ID=io_db)
 if (io_DIPOLES_cut/=0) goto 1

 sec_size=11
 if (any((/io_sec(io_db,:)==1/))) then

   io_DIPOLES_cut=io_header(io_db,R_LATT=.true.,WF=.true.)
   if (io_DIPOLES_cut/=0) goto 1

   call io_elemental(io_db,VAR="PARS",VAR_SZ=sec_size,MENU=0)
   call io_elemental(io_db,DB_I1=db_nb,&
&       VAR=" X band range           :",I1=X%ib,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(io_db,UNIT=HA2EV,&
&       VAR=" X e/h energy range [ev]:",R1=X%ehe,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(io_db,DB_I0=db_nspin,&
&       VAR=" Spin multiplicity      :",I0=n_spin,CHECK=.true.,OP=(/"=="/))
   call io_elemental(io_db,DB_I0=db_nbm,&
&       VAR=" Metallic bands         :",I0=Xen%nbm,CHECK=.true.,OP=(/"=="/))
   call io_elemental(io_db,DB_I0=db_nbf,&
&       VAR=" Filled bands           :",I0=Xen%nbf,CHECK=.true.,OP=(/"=="/))
   call io_elemental(io_db,&
&       VAR=" Rl vectors in the sum  :",I0=X%ngostnts,WARN=.true.,OP=(/"<="/))
   call io_elemental(io_db,&
&       VAR=" [r,Vnl] included       :",L0=X%Vnl_included,CHECK=.true.,OP=(/"=="/))
   call io_elemental(io_db,&
&       VAR=" Cutoff zero            :",R0=z0f,CHECK=.true.,OP=(/"=="/))
   call io_elemental(io_db,&
&       VAR=" Cutoff width           :",R0=dcf,CHECK=.true.,OP=(/"=="/))
   call io_elemental(io_db,VAR="",VAR_SZ=0)
   io_DIPOLES_cut=io_status(io_db)
   if (io_DIPOLES_cut/=0) goto 1
 endif
!
! On disk the size is DIP_iR_or_P(3,db_nb(2),db_nbm,nXkibz)
!
 sec_size=3*db_nb(2)*db_nbm
 if (any((/io_sec(io_db,:)==2/))) then

   allocate(DIP_iR_cut_disk(db_nb(2),db_nbm,2))

   if (read_is_on(io_db)) then
     allocate(DIP_iR_cut(3,X%ib(2),Xen%nbm,nXkibz,n_spin))
     call mem_est('DIP_iR_cut',(/ product( (/3,X%ib(2),Xen%nbm,nXkibz,n_spin/) ) /) ) ! fix for complex
   endif

   do i1=1,nXkibz
     do ixyz=1,3
       do i_spin = 1, n_spin

         if (write_is_on(io_db)) call mat_c2r(DIP_iR_cut(ixyz,:,:,i1,i_spin),DIP_iR_cut_disk)

         write (VAR_name,'(3(a,i4.4))') 'DIP_iR_k_',i1,'_xyz_',ixyz,'_spin_',i_spin

         call io_bulk(io_db,VAR=trim(VAR_name),VAR_SZ=shape(DIP_iR_cut_disk))
         call io_bulk(io_db,R3=DIP_iR_cut_disk)

         if (read_is_on(io_db)) call mat_r2c(DIP_iR_cut_disk,DIP_iR_cut(ixyz,:,:,i1,i_spin))

       enddo
     enddo
   enddo

   deallocate(DIP_iR_cut_disk)

 endif

1 call io_disconnect(ID=io_db)

 end function

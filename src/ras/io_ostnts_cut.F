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
 integer function ioOSTNTS_cut(X,Xen,io_db)
!======================================
 use pars
 use units,            ONLY : Hartree
 use X_m,              ONLY : X_t
 use electrons,        ONLY : levels, n_spinor, n_spin
 use R_lattice,        ONLY : nXkibz
 use matrix_operate,   ONLY : mat_c2r,mat_r2c
 use IO_m,             ONLY : io_connect,io_disconnect,io_sec,&
&                             io_elemental,io_status,io_bulk,&
&                             read_is_on,write_is_on,io_header
 use optcut,           ONLY : z0f, dcf, Xcut_q0grad
 use memory_m,         ONLY : mem_est
 implicit none
 type(X_t)                 :: X
 type(levels)              :: Xen
 integer                   :: io_db
!ws
 integer                   :: i1,ixyz,sec_size, i_spin
 integer                   :: db_nbm,db_nbf,db_nb(2),db_nspin
 character(schlen)         :: VAR_name, lch, tmp_VAR, tmp_xyz
 real(SP), allocatable     :: Xcut_q0grad_disk(:,:,:)

 write(lch,'(a,f5.3,a,f5.3)') "ostnts_z",z0f,"_d",dcf
 ioOSTNTS_cut = io_connect(desc=trim(lch),type=1,ID=io_db)
 if (ioOSTNTS_cut/=0) goto 1

 sec_size=11
 if (any((/io_sec(io_db,:)==1/))) then

   ioOSTNTS_cut=io_header(io_db,R_LATT=.true.,WF=.true.)
   if (ioOSTNTS_cut/=0) goto 1

   call io_elemental(io_db,VAR="PARS",VAR_SZ=sec_size,MENU=0)
   call io_elemental(io_db,DB_I1=db_nb,&
&       VAR=" X band range           :",I1=X%ib,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(io_db,UNIT=HARTREE,&
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
   ioOSTNTS_cut=io_status(io_db)
   if (ioOSTNTS_cut/=0) goto 1
 endif
!
! On disk the size is X_q0grad(3,db_nb(2),db_nbm,nXkibz)
!
 sec_size=3*db_nb(2)*db_nbm
 if (any((/io_sec(io_db,:)==2/))) then

   allocate(Xcut_q0grad_disk(db_nb(2),db_nbm,2))

   if (read_is_on(io_db)) then
     allocate(Xcut_q0grad(3,X%ib(2),Xen%nbm,nXkibz,n_spin))
     call mem_est('Xcut_q0grad',(/ product( (/3,X%ib(2),Xen%nbm,nXkibz,n_spin/) ) /) ) ! fix for complex
   endif

   do i1=1,nXkibz
     do ixyz=1,3
       do i_spin = 1, n_spin

         if (write_is_on(io_db)) call mat_c2r(Xcut_q0grad(ixyz,:,:,i1,i_spin),Xcut_q0grad_disk)

         write (VAR_name,'(3(a,i4.4))') 'OptGrad_k_',i1,'_xyz_',ixyz,'_spin_',i_spin

         call io_bulk(io_db,VAR=trim(VAR_name),VAR_SZ=shape(Xcut_q0grad_disk))
         call io_bulk(io_db,R3=Xcut_q0grad_disk)

         if (read_is_on(io_db)) call mat_r2c(Xcut_q0grad_disk,Xcut_q0grad(ixyz,:,:,i1,i_spin))

       enddo
     enddo
   enddo

   deallocate(Xcut_q0grad_disk)

 endif

1 call io_disconnect(ID=io_db)

 end function

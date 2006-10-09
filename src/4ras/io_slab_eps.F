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
 integer function ioslabeps(eps_i_r, X, Xk, Xen, wv, q0v, npol, io_db)
!======================================
 use pars
 use X_m,               ONLY : X_t
 use electrons,         ONLY : levels
 use IO_m,              ONLY : io_connect,io_disconnect,io_sec,&
&                              io_elemental,io_status,io_bulk,&
&                              read_is_on,write_is_on,io_header
 use optcut,            ONLY : z0f, dcf, loptcut
 use frequency,         ONLY : w_samp
 use R_lattice,         ONLY : bz_samp
 implicit none
!complex(SP), intent(inout) :: eps_i(:,:)
 integer,     intent(inout) :: io_db
 type(X_t),   intent(in)    :: X
 type(levels), intent(in)   :: Xen
 type(w_samp),   intent(in) :: wv
 real(SP),    intent(inout) :: eps_i_r(2,wv%n(1),npol) ! Real for NetCDF
 integer,     intent(in)    :: npol
 real(SP),    intent(in)    :: q0v(3,3)
!ws
 integer                    :: ipol
 type(bz_samp),  intent(in) :: Xk  ! why?
!real(SP)                   :: db_dcf, db_z0f
!logical                    :: db_loptcut
 integer                    :: db_nbm,db_nbf,db_nb(2),db_npol
 character(schlen)          :: VAR_name

 ioslabeps = io_connect(desc='slabeps',type=2,ID=io_db)
 if (ioslabeps/=0) goto 1

 if (any((/io_sec(io_db,:)==1/))) then

   ioslabeps=io_header(io_db,R_LATT=.true.,WF=.true.)
   if (ioslabeps/=0) goto 1

   call io_elemental(io_db,VAR="Parameters",VAR_SZ=12,MENU=0)
   call io_elemental(io_db,DB_I1=db_nb,&
&       VAR=" X band range           :",I1=X%ib,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(io_db,UNIT=HARTREE,&
&       VAR=" X e/h energy range [ev]:",R1=X%ehe,CHECK=.true.,OP=(/">=","<="/))
   call io_elemental(io_db,DB_I0=db_nbm,&
&       VAR=" Metallic bands         :",I0=Xen%nbm,CHECK=.true.,OP=(/"=="/))
   call io_elemental(io_db,DB_I0=db_nbf,&
&       VAR=" Filled bands           :",I0=Xen%nbf,CHECK=.true.,OP=(/"=="/))
   call io_elemental(io_db,&
&       VAR=" Rl vectors in the sum  :",I0=X%ngostnts,WARN=.true.,OP=(/"<="/))
   call io_elemental(io_db,&
&       VAR=" [r,Vnl] included       :",L0=X%Vnl_included,CHECK=.true.,OP=(/"=="/))

   call io_elemental(io_db,DB_I0=db_npol,&
&       VAR=" Number of polarizations:",I0=npol,CHECK=.true.,OP=(/"<="/))
   call io_elemental(io_db,&
&       VAR=" Real space cutoff used :",L0=loptcut,CHECK=.true.,OP=(/"=="/))
! These are not t be checked here...
   if(loptcut) then
   call io_elemental(io_db,&
&       VAR=" -> Cutoff zero         :",R0=z0f,CHECK=.true.,OP=(/"=="/))
   call io_elemental(io_db,&
&       VAR=" -> Cutoff width        :",R0=dcf,CHECK=.true.,OP=(/"=="/))
   else
   call io_elemental(io_db,&
&       VAR=" -> Cutoff zero         :",R0=z0f,OP=(/"=="/))
   call io_elemental(io_db,&
&       VAR=" -> Cutoff width        :",R0=dcf,OP=(/"=="/))
   endif
! Also check: q0v

     call io_elemental(io_db,VAR="",VAR_SZ=0)
     ioslabeps=io_status(io_db)
     if (ioslabeps/=0) goto 1
 endif
 
 if (any((/io_sec(io_db,:)==2/))) then

   do ipol = 1, npol

     write (VAR_name,'(a,i1.1)') 'Slab_Eps_Pol_',ipol
     call io_bulk(io_db, VAR=trim(VAR_name), VAR_SZ=shape(eps_i_r) )
     call io_bulk(io_db, R3=eps_i_r )

   enddo
   ioslabeps=io_status(io_db)

 endif

1 call io_disconnect(ID=io_db)

 end function ioslabeps

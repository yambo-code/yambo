!
!        Copyright (C) 2000-2016 the YAMBO team
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
integer function io_BSS_invert(iq,W,ID)
 !
 use pars,          ONLY:SP,cI
 use units,         ONLY:HA2EV
 use stderr,        ONLY:intc
 use frequency,     ONLY:w_samp
 use BS,            ONLY:BS_identifier,BSS_q0,BSS_uses_DbGd,BSS_dr,&
&                        BSS_damp_reference,BSS_inversion_PERT_FULL_ratio,&
&                        BSS_INV_err,BSS_INV_eps,BSS_INV_n_freqs
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,&
&                        io_elemental,io_status,io_bulk,&
&                        io_header,read_is_on,write_is_on
 implicit none
 integer      :: iq
 type(w_samp) :: W
 !
 ! Work Space
 !
 integer     :: ID
 real(SP)    :: EPS(W%n_freqs,4),Wp(W%n_freqs,2)
 !
 io_BSS_invert=io_connect(desc='BS_invert_Q'//trim(intc(iq)),type=2,ID=ID)
 !
 if (io_BSS_invert/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! As this DB must respect the BS ideentifier I only need to
   ! check the K QP corrections
   !
   io_BSS_invert=io_header(ID,XC_KIND="K_E force")
   !
   ! header return
   !
   if (io_BSS_invert/=0) goto 1
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=12,MENU=0)
   call io_elemental(ID,VAR=&
&       " BSK Identifier                  :",I0=BS_identifier,CHECK=.TRUE.,OP=(/"=="/))
   call io_elemental(ID,VAR=&
&       " Field direction                 :",R1=BSS_q0,CHECK=.true.,OP=(/"==","==","=="/))
   call io_elemental(ID,VAR=&
&       " Using energy Double Grid        :",L0=BSS_uses_DbGd,CHECK=.TRUE.,OP=(/"=="/))
   call io_elemental(ID,VAR=&
&       " Number of frequencies           :",DB_I0=BSS_INV_n_freqs,I0=W%n_freqs)
   call io_elemental(ID,VAR=&
&       " Perturbative to full ration     :",R0=BSS_inversion_PERT_FULL_ratio)
   call io_elemental(ID,UNIT=HA2EV,VAR=&
&       " Energy range                [ev]:",R1=W%er)
   call io_elemental(ID,UNIT=HA2EV,VAR=&
&       " Damping range               [ev]:",R1=BSS_dr,CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,UNIT=HA2EV,VAR=&
&       " Damping reference energy    [ev]:",R0=BSS_damp_reference,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   call io_bulk(ID,'FREQs_RANGE',VAR_SZ=(/2/))
   call io_bulk(ID,R1=W%er)
   !
   io_BSS_invert=io_status(ID)
   if (io_BSS_invert/=0) goto 1
   !
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   call io_bulk(ID,'INV_ERRs',VAR_SZ=(/BSS_INV_n_freqs/))
   call io_bulk(ID,I1=BSS_INV_err(:BSS_INV_n_freqs))
   !
   if (read_is_on(ID).and..not.associated(W%p)) then
     allocate(W%p(W%n_freqs))
   endif
   if (write_is_on(ID)) then
     EPS(:BSS_INV_n_freqs,1)=real (BSS_INV_eps(:BSS_INV_n_freqs,1))
     EPS(:BSS_INV_n_freqs,2)=aimag(BSS_INV_eps(:BSS_INV_n_freqs,1))
     EPS(:BSS_INV_n_freqs,3)=real (BSS_INV_eps(:BSS_INV_n_freqs,2))
     EPS(:BSS_INV_n_freqs,4)=aimag(BSS_INV_eps(:BSS_INV_n_freqs,2))
     Wp(:BSS_INV_n_freqs,1) =real (W%p(:BSS_INV_n_freqs))
     Wp(:BSS_INV_n_freqs,2) =aimag(W%p(:BSS_INV_n_freqs))
   endif
   !
   call io_bulk(ID,'FREQs',VAR_SZ=(/BSS_INV_n_freqs,2/))
   call io_bulk(ID,R2=Wp(:BSS_INV_n_freqs,:))
   !call io_bulk(ID,'FREQs',VAR_SZ=(/BSS_INV_n_freqs/))
   !call io_bulk(ID,C1=W%p(:BSS_INV_n_freqs))
   !
   call io_bulk(ID,'EPSILON',VAR_SZ=(/BSS_INV_n_freqs,4/))
   call io_bulk(ID,R2=EPS(:BSS_INV_n_freqs,:))
   !call io_bulk(ID,'EPSILON',VAR_SZ=(/BSS_INV_n_freqs,2/))
   !call io_bulk(ID,C2=BSS_INV_eps_pl(:BSS_INV_n_freqs,:))
   !
   if (read_is_on(ID)) then
     W%p(:BSS_INV_n_freqs)             =Wp(:BSS_INV_n_freqs,1)+cI*Wp(:BSS_INV_n_freqs,2)
     BSS_INV_eps(:BSS_INV_n_freqs,1)=EPS(:BSS_INV_n_freqs,1)+cI*EPS(:BSS_INV_n_freqs,2)
     BSS_INV_eps(:BSS_INV_n_freqs,2)=EPS(:BSS_INV_n_freqs,3)+cI*EPS(:BSS_INV_n_freqs,4)
   endif
   !
 endif
 !
1 call io_disconnect(ID)
 !
end function

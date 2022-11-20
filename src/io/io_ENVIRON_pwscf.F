!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, PDA
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
integer function io_ENVIRON_pwscf(ID)
 !
 use pars,             ONLY:SP,IP
 use stderr,           ONLY:intc
 use IO_m,             ONLY:io_connect,io_disconnect,io_sec, &
&                           io_elemental,io_bulk,io_header,&
&                           read_is_on
 use environ_m,        ONLY:nr1,nr2,nr3,ng_env,eps_env_g,environ_alloc
 !
 implicit none
 !
 integer,intent(in) :: ID
 ! 
 ! Work Space
 !
 
 io_ENVIRON_pwscf=io_connect(desc="environ_pwscf",type=0,ID=ID)
 if (io_ENVIRON_pwscf/=0) goto 1
 !
 ! Dimensions
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_ENVIRON_pwscf=io_header(ID,IMPOSE_SN=.true.)
   if (io_ENVIRON_pwscf/=0) goto 1
   !
   call io_elemental(ID,VAR="NG_ENV",VAR_SZ=1,MENU=0)
   call io_elemental(ID,I0=ng_env,VAR=" Environ number of G vectors")
   call io_elemental(ID,VAR="FFT_DIM_ENV",VAR_SZ=3,MENU=0)
   call io_elemental(ID,I1=(/nr1,nr2,nr3/),VAR=" Environ FFT dim")
   !
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
 endif
 !
 if (.not.any((/io_sec(ID,:)==2/))) goto 1
 !
 if (read_is_on(ID)) call environ_alloc(ng_env)
 !
 call io_bulk(ID,VAR="EPS_ENV_GSPACE",VAR_SZ=(/2,ng_env/))
 call io_bulk(ID,C1=eps_env_g)
 !
1 call io_disconnect(ID=ID)
 !
end function io_ENVIRON_pwscf


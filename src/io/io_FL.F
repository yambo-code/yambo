!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS, IM
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
integer function io_FL(ID,FL_eigenvalues,FL_eigenvectors)
 !
 use pars,          ONLY:SP,schlen
 use electrons,     ONLY:n_sp_pol
 use QP_m,          ONLY:QP_nk
 use IO_m,          ONLY:io_sec,read_is_on,write_is_on,io_extension
 use IO_int,        ONLY:io_connect,io_disconnect,io_bulk,io_header,&
&                        io_fragment,io_elemental,io_variable_bulk,&
&                        def_variable_bulk
 use fl_optics,     ONLY:FL_states
 use timing_m,      ONLY:timing
 !
 implicit none
 !
 integer, intent(in) :: ID
 complex(SP), optional,   intent(inout)    :: FL_eigenvectors(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 real(SP),    optional,   intent(inout)    :: FL_eigenvalues(FL_states(1):FL_states(2),QP_nk,n_sp_pol)
 !
 ! Work space
 !
 integer :: ID_frag,VAR_SZ(4),io_err,ifreq
 character(schlen) :: dim_names(4)
 !
 call timing('io_FL',OPR='stop')
 !
 io_FL=io_connect(desc="FL_database",type=2,ID=ID)
 if (io_FL/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! Header
   !
   io_FL=io_header(ID)
   if (io_FL/=0) goto 1
   !
 endif
 !
 ifreq = maxval(io_sec(ID,:))-1
 if (ifreq==0) goto 1
 !
 call io_fragment(ID,ID_frag,i_fragment=ifreq,ierr=io_err)
 io_FL=io_err !I think this is meant to be io_FL, not io_NL.
 if (io_err/=0) goto 1
 !
 VAR_SZ(1)=2
 dim_names(1)="cmplx"
 !
 ! Variable 1
 !
 VAR_SZ(2:4)=shape(FL_eigenvalues)
 dim_names(2:4)=[character(schlen) :: "FLstates" , "QP_nk" , "n_sp_pol"]
 ! 
 call def_variable_bulk(ID,"FL_eigenvalues", 1,VAR_SZ(2:4),SP,dim_names=dim_names(2:4))
 call io_variable_bulk(ID,1,R3=FL_eigenvalues)
 !
 ! Variable 2
 !
 VAR_SZ(2:3)=shape(FL_eigenvectors)
 dim_names(2:3)=[character(schlen) :: "FLstates" , "LFstates"]
 !
 call def_variable_bulk(ID,"FL_eigenvectors",2,VAR_SZ(1:3),SP,dim_names=dim_names(1:3))
 call io_variable_bulk(ID,4,C2=FL_eigenvectors)
 !
1 call io_fragment_disconnect(ID,ID_frag)
 !
 call io_disconnect(ID)
 !
 call timing('io_FL',OPR='stop')
 !
end function io_FL

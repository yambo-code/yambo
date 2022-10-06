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
integer function io_FL(ID)
 !
 use pars,          ONLY:IP,SP,schlen
 use electrons,     ONLY:n_sp_pol
 use QP_m,          ONLY:QP_nk
 use descriptors,   ONLY:IO_desc_reset
 use IO_m,          ONLY:io_sec,read_is_on,write_is_on,io_extension
 use IO_int,        ONLY:io_connect,io_disconnect,io_bulk,io_header,&
&                        io_fragment,io_elemental,io_variable_bulk,&
&                        def_variable_bulk,io_variable_elemental,&
&                        def_variable_elemental
 use nl_optics,     ONLY:NL_bands
 use fl_optics,     ONLY:FL_states,FL_eigenvectors,FL_eigenvalues,n_tot_fl_modes,&
&                        FL_desc,FL_table
 use timing_m,      ONLY:timing
 !
 implicit none
 !
 integer, intent(in) :: ID
 !
 ! Work space
 !
 integer :: ID_frag,VAR_SZ(5),io_err,ifreq
 character(schlen) :: dim_names(5)
 !
 call timing('io_FL',OPR='start')
 !
 io_FL=io_connect(desc="Floquet",type=2,ID=ID)
 if (io_FL/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! Header
   !
   io_FL=io_header(ID)
   if (io_FL/=0) goto 1
   !
   call IO_desc_reset(FL_desc)
   !
   call def_variable_elemental(ID,VAR="FL_states",VAR_SZ=2,VAR_KIND=IP+100,MENU=0)
   call io_variable_elemental(ID,VAR="[FL] Number of FL states",I1=FL_states,CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=FL_desc)
   !
   call def_variable_elemental(ID,VAR="FL_bands",VAR_SZ=2,VAR_KIND=IP+100,MENU=0)
   call io_variable_elemental(ID,VAR="[FL] Number of FL bands",I1=NL_bands,CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=FL_desc)
   !
   call def_variable_elemental(ID,VAR="FL_modes",VAR_SZ=1,VAR_KIND=IP+100,MENU=0)
   call io_variable_elemental(ID,VAR="[FL] Number of FL modes",I0=n_tot_fl_modes,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=FL_desc)
   !
   call def_variable_elemental(ID,VAR="QP_nk",VAR_SZ=1,VAR_KIND=IP+100,MENU=0)
   call io_variable_elemental(ID,VAR="[FL] Number of kpoints",I0=QP_nk,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=FL_desc)
   !
   call def_variable_bulk(ID,"FL_table",1,VAR_SZ=shape(FL_table),VAR_KIND=IP+100,dim_names=(/"FLstates"/))
   call io_variable_bulk(ID,1,I2=FL_table)
   !
 endif
 !
 ID_frag=0
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
 call def_variable_bulk(ID_frag,"FL_eigenvalues", 1,VAR_SZ(2:4),SP,dim_names=dim_names(2:4))
 call io_variable_bulk(ID_frag,1,R3=FL_eigenvalues)
 !
 ! Variable 2
 !
 VAR_SZ(2:5)=shape(FL_eigenvectors)
 dim_names(2:5)=[character(schlen) :: "FLstates" , "LFstates" , "QP_nk" , "n_sp_pol"]
 !
 call def_variable_bulk(ID_frag,"FL_eigenvectors",2,VAR_SZ(1:3),SP,dim_names=dim_names(1:5))
 call io_variable_bulk(ID_frag,2,C4=FL_eigenvectors)
 !
1 call io_fragment_disconnect(ID,ID_frag)
 !
 call io_disconnect(ID)
 !
 call timing('io_FL',OPR='stop')
 !
end function io_FL

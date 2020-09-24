!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS, AM
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
function io_descriptors(desc,name,ID,QUIET)
 !
 use pars,           ONLY:schlen,SP,DP,IP,LP
 use descriptors,    ONLY:IO_desc
 use IO_m,           ONLY:def_variable_elemental,io_variable_elemental,&
&                         def_variable_bulk,io_variable_bulk,io_status,&
&                         variable_exist,read_is_on,io_com,NONE
 use stderr,         ONLY:intc
 use netcdf,         ONLY:NF90_UNLIMITED
 !
 implicit none
 !
 integer :: io_descriptors
 !
 integer,         intent(in) :: ID
 character(*),    intent(in) :: name
 logical,         intent(in) :: QUIET
 type(IO_desc),intent(inout) :: desc
 !
 ! Work Space
 !
 character(schlen), external :: desc_to_varname
 character(schlen)           :: dim_name(2)
 integer :: n_descs_disk,i1,N,VAR_KIND,io_com_save,desc_size
 !
 if (read_is_on(ID) .and. .not.variable_exist(ID,name//"_N_DESCRIPTORS")) return
 !
 call def_variable_elemental(ID,name//"_N_DESCRIPTORS",1,IP+100,0)
 call  io_variable_elemental(ID,I0=desc%n,DB_I0=n_descs_disk,CHECK=.true.,OP=(/"<="/))
 !
 if (n_descs_disk==0) then
   io_descriptors=io_status(ID)
   return
 endif
 !
 desc_size=n_descs_disk
 if (name=="TIME_STEP") desc_size=NF90_UNLIMITED
 !
 dim_name(1)=trim(name)//"_string_len"
 dim_name(2)=trim(name)//"_desc_size"
 !
 call def_variable_bulk(ID,name//"_DESCRIPTORS_SIZES",2,(/desc_size/),IP+100,dim_names=dim_name(2:2))
 call  io_variable_bulk(ID,2,I1=desc%size(:n_descs_disk))
 !
 call def_variable_bulk(ID,name//"_DESCRIPTORS_NAMES",1,(/schlen,desc_size/),0,dim_names=dim_name)
 call  io_variable_bulk(ID,1,STR1=desc%str(:n_descs_disk))
 !
 call def_variable_bulk(ID,name//"_DESCRIPTORS_KINDS",1,(/schlen,desc_size/),0,dim_names=dim_name)
 call  io_variable_bulk(ID,1,STR1=desc%kind(:n_descs_disk))
 !
 call def_variable_bulk(ID,name//"_DESCRIPTORS_TERMS",1,(/schlen,desc_size/),0,dim_names=dim_name)
 call  io_variable_bulk(ID,1,STR1=desc%term(:n_descs_disk))
 !
 io_com_save=io_com(ID)
 if (QUIET) io_com(ID)=NONE
 !
 do i1=1,n_descs_disk
   !
   if (desc%kind(i1)=='') cycle
   !
   if(desc%kind(i1)=='i')   VAR_KIND=IP+100
   if(desc%kind(i1)=='d')   VAR_KIND=DP
   if(desc%kind(i1)=='r')   VAR_KIND=SP
   if(desc%kind(i1)=='c')   VAR_KIND=SP
   if(desc%kind(i1)=='l')   VAR_KIND=LP+100
   if(desc%kind(i1)=='s')   VAR_KIND=0
   !
   N=desc%size(i1)
   !
   if (len_trim(desc%str(i1))>0) then
     if(desc%kind(i1)/='c') call def_variable_elemental(ID,name//"_"//trim(desc_to_varname(desc%str(i1))),N,VAR_KIND,0)
     if(desc%kind(i1)=='c') call def_variable_elemental(ID,name//"_"//trim(desc_to_varname(desc%str(i1))),2,VAR_KIND,0)
   else
     if(desc%kind(i1)/='c') call def_variable_elemental(ID,name//"_EMPTY_STR_Nr"//trim(intc(i1)),N,VAR_KIND,0)
     if(desc%kind(i1)=='c') call def_variable_elemental(ID,name//"_EMPTY_STR_Nr"//trim(intc(i1)),2,VAR_KIND,0)
   endif
   !
   if(N==1) then
     if(desc%kind(i1)=='i') call io_variable_elemental(ID,I0=desc%ival(1,i1),VAR=desc%str(i1),TERMINATOR=desc%term(i1),&
&                                                         CHECK=desc%check(i1),OP=desc%ops(1:1,i1))
     if(desc%kind(i1)=='r') call io_variable_elemental(ID,R0=desc%rval(1,i1),VAR=desc%str(i1),TERMINATOR=desc%term(i1),&
&                                                         CHECK=desc%check(i1),OP=desc%ops(1:1,i1))
     if(desc%kind(i1)=='d') call io_variable_elemental(ID,D0=desc%dval(i1),VAR=desc%str(i1),TERMINATOR=desc%term(i1),&
&                                                         CHECK=desc%check(i1),OP=desc%ops(1:1,i1))
     if(desc%kind(i1)=='c') call io_variable_elemental(ID,C0=desc%cval(i1),VAR=desc%str(i1),TERMINATOR=desc%term(i1),&
&                                                         CHECK=desc%check(i1),OP=desc%ops(1:1,i1))
     if(desc%kind(i1)=='l') call io_variable_elemental(ID,L0=desc%lval(i1),VAR=desc%str(i1),TERMINATOR=desc%term(i1),&
&                                                         CHECK=desc%check(i1),OP=desc%ops(1:1,i1))
     if(desc%kind(i1)=='s') call io_variable_elemental(ID,CH0=desc%sval(i1),VAR=desc%str(i1),TERMINATOR=desc%term(i1),&
&                                                         CHECK=desc%check(i1),OP=desc%ops(1:1,i1))
   else
     if(desc%kind(i1)=='i') call io_variable_elemental(ID,I1=desc%ival(1:N,i1),VAR=desc%str(i1),TERMINATOR=desc%term(i1),&
&                                                         CHECK=desc%check(i1),OP=desc%ops(1:N,i1))
     if(desc%kind(i1)=='r') call io_variable_elemental(ID,R1=desc%rval(1:N,i1),VAR=desc%str(i1),TERMINATOR=desc%term(i1),&
&                                                         CHECK=desc%check(i1),OP=desc%ops(1:N,i1))
     if(desc%kind(i1)=='c') call error(' Vector of complex not allowed in descriptors ')
     if(desc%kind(i1)=='d') call error(' Vector of doubles not allowed in descriptors ')
     if(desc%kind(i1)=='l') call error(' Vector of logical not allowed in descriptors ')
     if(desc%kind(i1)=='s') call error(' Vector of strings not allowed in descriptors ')
   endif
   !
 enddo
 !
 io_descriptors=io_status(ID)
 !
 io_com(ID)=io_com_save
 !
end function

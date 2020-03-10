!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
integer function variables_X(ID,local_desc_var,local_desc_str,local_desc_size,X)
 !
 use units,         ONLY:HA2EV
 use pars,          ONLY:SP,IP,schlen,lchlen
 use X_m,           ONLY:X_t,use_X_DbGd,X_DbGd_nkpts,n_X_descs,X_DbGd_percentual
 use IO_m,          ONLY:def_variable_elemental,io_variable_elemental,io_status,&
&                        ver_is_gt_or_eq,read_is_on,variable_exist
 !
 implicit none
 !
 integer            ::ID
 integer            ::local_desc_size(100)
 real(SP)           ::local_desc_var(10,100)
 character(lchlen)  ::local_desc_str(100)
 type(X_t)          ::X
 !
 ! Work Space
 !
 integer             ::i_local_desc,sec_size
 logical             ::local_long_gauge
 character(schlen)   ::X_label,VAR_NAME
 !
 i_local_desc=0
 local_desc_var=0._SP
 local_desc_str=' '
 X_label=' '
 if (ver_is_gt_or_eq(ID,revision=955)) X_label="X_"
 !
 ! PARS_1
 !
 call def_variable_elemental(ID,trim(X_label)//"PARS_1",5,SP,0)
 !
 i_local_desc=i_local_desc+1
 local_desc_size(i_local_desc)=1
 call io_variable_elemental(ID,DB_I0=X%ng_db,&
&     VAR=" X matrix size           ",I0=X%ng,CHECK=.true.,OP=(/"<="/),&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
 i_local_desc=i_local_desc+1
 local_desc_size(i_local_desc)=2
 call io_variable_elemental(ID,&
&     VAR=" X band range            ",I1=X%ib,CHECK=.true.,OP=(/"==","=="/),&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
 i_local_desc=i_local_desc+1
 local_desc_size(i_local_desc)=2
 call io_variable_elemental(ID,UNIT=HA2EV,&
&     VAR=" X e/h energy range [ev] ",R1=X%ehe,CHECK=.true.,OP=(/">=","<="/),&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
 !call def_variable_elemental(ID,"",0,0,0)
 !
 ! PARS_2
 !
 VAR_NAME=trim(X_label)//"Time_ordering"
 if (read_is_on(ID) .and. .not. variable_exist(ID,trim(VAR_NAME))) VAR_NAME=trim(X_label)//"PARS_2"
 call def_variable_elemental(ID,trim(VAR_NAME),1,0,0)
 i_local_desc=i_local_desc+1
 local_desc_size(i_local_desc)=1
 call io_variable_elemental(ID,&
&     VAR=" X Time ordering         ",CH0=X%ordering,WARN=.true.,&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
 !call def_variable_elemental(ID,"",0,0,0)
 if (ver_is_gt_or_eq(ID,(/3,0,5/))) then
   !
   ! TDDFT_KERNEL
   !
   call def_variable_elemental(ID,trim(X_label)//"TDDFT_KERNEL",1,0,0)
   i_local_desc=i_local_desc+1
   local_desc_size(i_local_desc)=1
   call io_variable_elemental(ID,&
&     VAR=" X xc-Kernel             ",CH0=X%f_xc,CHECK=.true.,&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
   !call def_variable_elemental(ID,"",0,0,0)
   !
   ! DRUDE
   !
   call def_variable_elemental(ID,trim(X_label)//"DRUDE",2,SP,0)
   i_local_desc=i_local_desc+1
   local_desc_size(i_local_desc)=1
   call io_variable_elemental(ID,UNIT=HA2EV,&
&     VAR=" X Drude frequency       ",C0=X%Wd,CHECK=.true.,OP=(/"=="/),&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
   !call def_variable_elemental(ID,"",0,0,0)
   !
 endif
 !
 ! PARS_3
 !
 sec_size=6
 if (ver_is_gt_or_eq(ID,(/3,0,1/)))      sec_size=8
 if (ver_is_gt_or_eq(ID,(/3,0,9/)))      sec_size=9
 if (ver_is_gt_or_eq(ID,revision=15206)) sec_size=10
 call def_variable_elemental(ID,trim(X_label)//"PARS_3",sec_size,SP,0)
 i_local_desc=i_local_desc+1
 local_desc_size(i_local_desc)=1
 call io_variable_elemental(ID,&
&     VAR=" X poles           [o/o] ",R0=X%cg_percentual,CHECK=.true.,OP=(/"=="/),&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
 i_local_desc=i_local_desc+1
 local_desc_size(i_local_desc)=1
 call io_variable_elemental(ID,&
&     VAR=" RL vectors in the sum   ",I0=X%ngostnts,WARN=.true.,OP=(/"<="/),&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
 i_local_desc=i_local_desc+1
 local_desc_size(i_local_desc)=1
 call io_variable_elemental(ID,&
&     VAR=" [r,Vnl] included        ",L0=X%Vnl_included,WARN=.true.,OP=(/"=="/),&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
 !
 if (.not.ver_is_gt_or_eq(ID,(/3,0,9/)).and.read_is_on(ID)) then
   i_local_desc=i_local_desc+1
   local_desc_size(i_local_desc)=1
   call io_variable_elemental(ID,VAR=" Longitudinal Gauge      ",L0=local_long_gauge,&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
 endif
 !
 i_local_desc=i_local_desc+1
 local_desc_size(i_local_desc)=3
 call io_variable_elemental(ID,&
&     VAR=" Field direction         ",R1=X%q0,CHECK=.true.,OP=(/"==","==","=="/),&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
 !
 if (ver_is_gt_or_eq(ID,(/3,0,1/))) then
   i_local_desc=i_local_desc+1
   local_desc_size(i_local_desc)=1
   call io_variable_elemental(ID,&
&       VAR=" BZ energy Double Grid   ",L0=use_X_DbGd,WARN=.true.,OP=(/"=="/),&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
   i_local_desc=i_local_desc+1
   local_desc_size(i_local_desc)=1
   call io_variable_elemental(ID,&
&       VAR=" BZ energy DbGd points   ",I0=X_DbGd_nkpts,WARN=.true.,OP=(/"=="/),&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
   if (ver_is_gt_or_eq(ID,revision=15206)) then
     i_local_desc=i_local_desc+1
     local_desc_size(i_local_desc)=1
     call io_variable_elemental(ID,&
&         VAR=" BZ Q point size factor  ",R0=X_DbGd_percentual,WARN=.true.,OP=(/"=="/),&
&     DESC_STR=local_desc_str(i_local_desc),DESC_VAR=local_desc_var(:,i_local_desc))
   endif
 endif
 !
 n_X_descs=i_local_desc
 !
 variables_X=io_status(ID)
 !
end function

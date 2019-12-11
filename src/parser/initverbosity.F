!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine initverbosity(var_name,var_id,var_type,defs,verbosity_level)
 !
 use it_tools
 use it_m
 !
 implicit none
 !
 integer        ::var_id,verbosity_level
 character(*)   ::var_name,var_type
 type(initdefs) ::defs
 !
 if (verbosity_level==0) return
 !
 select case (var_type)
   case('i1')
     n_verbose_Ivars=n_verbose_Ivars+1
     if (it_verbose_Itable(1,var_id)>0) then
       n_verbose_Ivars=it_verbose_Itable(1,var_id)
     else
       it_verbose_Ilevel(n_verbose_Ivars)=verbosity_level
       it_verbose_Itable(1,var_id)=n_verbose_Ivars
     endif
     it_verbose_Ivars(n_verbose_Ivars)=var_name
     it_verbose_Idefs(n_verbose_Ivars,1)=defs%i1def(var_id)
   case('i2')
     n_verbose_Ivars=n_verbose_Ivars+1
     if (it_verbose_Itable(2,var_id)>0) then
       n_verbose_Ivars=it_verbose_Itable(2,var_id)
     else
       it_verbose_Ilevel(n_verbose_Ivars)=verbosity_level
       it_verbose_Itable(2,var_id)=n_verbose_Ivars
     endif
     it_verbose_Ivars(n_verbose_Ivars)=var_name
     it_verbose_Idefs(n_verbose_Ivars,:2)=defs%i2def(var_id,:)
   case('i3')
     n_verbose_Ivars=n_verbose_Ivars+1
     if (it_verbose_Itable(3,var_id)>0) then
       n_verbose_Ivars=it_verbose_Itable(3,var_id)
     else
       it_verbose_Ilevel(n_verbose_Ivars)=verbosity_level
       it_verbose_Itable(3,var_id)=n_verbose_Ivars
     endif
     it_verbose_Ivars(n_verbose_Ivars)=var_name
     it_verbose_Idefs(n_verbose_Ivars,:3)=defs%i3def(var_id,:)
   case('r1')
     n_verbose_Rvars=n_verbose_Rvars+1
     if (it_verbose_Rtable(1,var_id)>0) then
       n_verbose_Rvars=it_verbose_Rtable(1,var_id)
     else
       it_verbose_Rlevel(n_verbose_Rvars)=verbosity_level
       it_verbose_Rtable(1,var_id)=n_verbose_Rvars
     endif
     it_verbose_Rvars(n_verbose_Rvars)=var_name
     it_verbose_Rdefs(n_verbose_Rvars,1)=defs%r1def(var_id)
   case('r2')
     n_verbose_Rvars=n_verbose_Rvars+1
     if (it_verbose_Rtable(2,var_id)>0) then
       n_verbose_Rvars=it_verbose_Rtable(2,var_id)
     else
       it_verbose_Rlevel(n_verbose_Rvars)=verbosity_level
       it_verbose_Rtable(2,var_id)=n_verbose_Rvars
     endif
     it_verbose_Rvars(n_verbose_Rvars)=var_name
     it_verbose_Rdefs(n_verbose_Rvars,:2)=defs%r2def(var_id,:)
   case('r3')
     n_verbose_Rvars=n_verbose_Rvars+1
     if (it_verbose_Rtable(3,var_id)>0) then
       n_verbose_Rvars=it_verbose_Rtable(3,var_id)
     else
       it_verbose_Rlevel(n_verbose_Rvars)=verbosity_level
       it_verbose_Rtable(3,var_id)=n_verbose_Rvars
     endif
     it_verbose_Rvars(n_verbose_Rvars)=var_name
     it_verbose_Rdefs(n_verbose_Rvars,:3)=defs%r3def(var_id,:)
   case('r4')
     n_verbose_Rvars=n_verbose_Rvars+1
     if (it_verbose_Rtable(4,var_id)>0) then
       n_verbose_Rvars=it_verbose_Rtable(4,var_id)
     else
       it_verbose_Rlevel(n_verbose_Rvars)=verbosity_level
       it_verbose_Rtable(4,var_id)=n_verbose_Rvars
     endif
     it_verbose_Rvars(n_verbose_Rvars)=var_name
     it_verbose_Rdefs(n_verbose_Rvars,:4)=defs%r4def(var_id,:)
   case('c1')
     n_verbose_Cvars=n_verbose_Cvars+1
     if (it_verbose_Ctable(var_id)>0) then
       n_verbose_Cvars=it_verbose_Ctable(var_id)
     else
       it_verbose_Clevel(n_verbose_Cvars)=verbosity_level
       it_verbose_Ctable(var_id)=n_verbose_Cvars
     endif
     it_verbose_Cvars(n_verbose_Cvars)=var_name
     it_verbose_Cdefs(n_verbose_Cvars)=defs%c1def(var_id)
   case('ch')
     n_verbose_Chvars=n_verbose_Chvars+1
     if (it_verbose_Chtable(var_id)>0) then
       n_verbose_Chvars=it_verbose_Chtable(var_id)
     else
       it_verbose_Chlevel(n_verbose_Chvars)=verbosity_level
       it_verbose_Chtable(var_id)=n_verbose_Chvars
     endif
     it_verbose_Chvars(n_verbose_Chvars)=var_name
     it_verbose_Chdefs(n_verbose_Chvars)=defs%chdef(var_id)
   case('f')
     n_verbose_Fvars=n_verbose_Fvars+1
     if (it_verbose_Ftable(var_id)>0) then
       n_verbose_Fvars=it_verbose_Ftable(var_id)
     else
       it_verbose_Flevel(n_verbose_Fvars)=verbosity_level
       it_verbose_Ftable(var_id)=n_verbose_Fvars
     endif
     it_verbose_Fvars(n_verbose_Fvars)=var_name
 end select
end subroutine

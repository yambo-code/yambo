!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine io_RT_components_OBS(ID,l_JP,l_M,what)
 !
 use pars,        ONLY:SP,schlen
 use IO_m,        ONLY:io_variable_bulk,def_variable_bulk,&
&                      read_is_on,write_is_on
 use electrons,   ONLY:n_spin
 use real_time,   ONLY:l_NE_with_fields,NE_i_time
 use netcdf,         ONLY:NF90_UNLIMITED
 use RT_control,  ONLY:J_cache,P_cache,Ms_cache,Ml_cache,A_cache,&
&                      CACHE_OBS_steps_now,OBS_RT_IO_t
 !
#include<memory.h>
 !
 integer,      intent(in) :: ID
 logical,      intent(in) :: l_JP,l_M
 character(*), intent(in) :: what
 !
 integer           :: VAR_SZ(4),CACHE_SZ,IPOS
 character(schlen) :: dim_names(4)
 !
 select case(WHAT)
   !
 case('DEF')
   !
   if (l_JP) then
     VAR_SZ(1:3)    = (/2,3,NF90_UNLIMITED/)
     dim_names(1:3) = [character(schlen) :: 'complex', 'xyz','time']
     call def_variable_bulk(ID,"Current",     4,VAR_SZ(1:3),SP,dim_names=dim_names(1:3),silent=.true.)
     call def_variable_bulk(ID,"Polarization",5,VAR_SZ(1:3),SP,dim_names=dim_names(1:3),silent=.true.)
   endif
   !
   if(n_spin>1.and.l_M) then
     VAR_SZ(1:3)    = (/2,3,NF90_UNLIMITED/)
     dim_names(1:3) = [character(schlen) :: 'complex', 'xyz','time']
     call def_variable_bulk(ID,"Spin_Magnetization",6,VAR_SZ(1:3),SP,dim_names=dim_names(1:3),silent=.true.)
     !
     VAR_SZ    = (/2,3,2,NF90_UNLIMITED/)
     dim_names = [character(schlen) :: 'complex', 'xyz','orb_itin','time']
     call def_variable_bulk(ID,"Orbital_Magnetization",7,VAR_SZ,SP,dim_names=dim_names,silent=.true.)
   endif
   !
   if(l_NE_with_fields) then
     VAR_SZ    = (/2,3,3,NF90_UNLIMITED/)
     dim_names = [character(schlen) :: 'complex', 'xyz','A_kind','time']
     call def_variable_bulk(ID,"A_tot",8,VAR_SZ,SP,dim_names=dim_names,silent=.true.)
   endif
   !
 case('FILL')
   !
   if(write_is_on(ID)) then
     if(NE_i_time==1) CACHE_SZ=1
     if(NE_i_time> 1) CACHE_SZ=CACHE_OBS_steps_now
     IPOS    =OBS_RT_IO_t%N-CACHE_OBS_steps_now+1
   else if(read_is_on(ID)) then
     CACHE_SZ=CACHE_OBS_steps_now
     IPOS    =1
   endif
   !
   if (l_JP) then
     call io_variable_bulk(ID,4,C2=J_cache(:,1:CACHE_SZ),IPOS=(/1,1,IPOS/))
     call io_variable_bulk(ID,5,C2=P_cache(:,1:CACHE_SZ),IPOS=(/1,1,IPOS/))
   endif
   !
   if(n_spin>1.and.l_M) then
     call io_variable_bulk(ID,6,C2=Ms_cache(:,1:CACHE_SZ),IPOS=(/1,1,IPOS/))
     call io_variable_bulk(ID,7,C3=Ml_cache(:,:,1:CACHE_SZ),IPOS=(/1,1,1,IPOS/))
   endif
   !
   if(l_NE_with_fields) then
     call io_variable_bulk(ID,8,C3=A_cache(:,:,1:CACHE_SZ),IPOS=(/1,1,1,IPOS/))
   endif
   !
 end select
 !
end subroutine

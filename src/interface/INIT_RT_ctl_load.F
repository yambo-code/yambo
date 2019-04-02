!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine INIT_RT_ctl_load(defs,i_type)
 !
 ! Note that this routine is called in 'load' mode BEFORE
 ! the DB1 is read, so before n_sp_pol is set to the correct value.
 ! 
 ! Thus we must call it on the no spin, spin + and spin - cases 
 ! because the code does not know if there is
 ! spin unless the DB1 is read. 
 !
 use it_m,          ONLY:it,initdefs,V_real_time
 use RT_control,    ONLY:RT_ctl_db,RT_ctl_interp_neigh
 !
 implicit none
 integer, intent(in) :: i_type
 type(initdefs)      :: defs
 ! 
 ! Work Space
 ! 
 character(1)      ::prefix(4)
 character(7)      ::description_ch(4)
 !
 prefix=(/'X','K','G','R'/)
 description_ch(1)='Xd'
 description_ch(2)='BSK BSS'
 description_ch(3)='G'
 description_ch(4)='R'
 !
 call it(defs,prefix(i_type)//'fnRTdb',&
&        '[EXTRT '//trim(description_ch(i_type))//'] Database',&
&        RT_ctl_db(i_type),verb_level=V_real_time)
 !
 call it(defs,prefix(i_type)//'fnRT_N',&
&        '[EXTRT '//trim(description_ch(i_type))//'] Interpolation neighbours',&
&        RT_ctl_interp_neigh(i_type),verb_level=V_real_time)
 ! 
end subroutine

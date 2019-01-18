!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine INIT_DephMatrix()
 !
 ! Dephasing Matrix -> input file
 !
 use pars,           ONLY:SP
 use parser_m,       ONLY:PARSER_array
 use real_time,      ONLY:RT_INPUT_Deph_limits
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer :: n_fields
 ! 
 call PARSER_array(mode="REAL dimensions",VAR="DephMatrix",N=n_fields,M=4)
 !
 if (n_fields>0) then
   YAMBO_ALLOC(RT_INPUT_Deph_limits,(n_fields,4))
   call PARSER_array(mode="read",VAR="DephMatrix",N=n_fields,M=4,R_v=RT_INPUT_Deph_limits)
   call PARSER_array(mode="write",VAR="DephMatrix",&
&                    DESC='[RT] Dephasing Matrix (bnd1 | bnd2 | kpt | dephasing(fs) |)',&
&                    N=n_fields,M=4,R_v=RT_INPUT_Deph_limits,Kn=(/"i","i","i","r"/))
 else
   call PARSER_array(mode="write",VAR="DephMatrix",&
&                    DESC='[RT] Dephasing Matrix (bnd1 | bnd2 | kpt | dephasing(fs) |)',&
&                    N=1,M=4,R_v=reshape((/1._SP,1._SP,1._SP,-1._SP/),(/1,4/)),Kn=(/"i","i","i","r"/))
 endif
 !
end subroutine

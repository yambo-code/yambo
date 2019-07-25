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
subroutine init_parse_K_list(k,variable)
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use parser_m,       ONLY:parser
 !
#include<memory.h>
 type(bz_samp)::k
 character(*) ::variable
 !
 ! Work Space
 !
 real(SP)     ::real_ctl
 !
 real_ctl=0.
 k%nibz=1
 K_loop: do while(real_ctl/=999.)
   YAMBO_FREE(k%pt)
   YAMBO_ALLOC(k%pt,(k%nibz,3))
   k%pt(k%nibz,:)=(/0.,0.,999./)
   call parser('QptCoord',k%pt)
   real_ctl=k%pt(k%nibz,3)
   if (real_ctl/=999.) k%nibz=k%nibz+1
 enddo K_loop
 k%nibz=k%nibz-1
 !
end subroutine

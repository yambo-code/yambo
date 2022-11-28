!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine ELPH_sngl_db(k,E,q)
 !
 use units,               ONLY:HA2EV
 use pars,                ONLY:schlen,SP
 use stderr,              ONLY:intc
 use electrons,           ONLY:levels
 use YPP_ELPH,            ONLY:l_gkkp_expand_only_k
 use R_lattice,           ONLY:bz_samp
 use parser_m,            ONLY:parser
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 type(levels)  ::E
 integer       ::qindx_ID,qindx_ID_frag
 !
 call parser('GkkpExpOnlyK',l_gkkp_expand_only_k)
 !
 if(l_gkkp_expand_only_k) call ELPH_load_indexes(k,E,q,qindx_ID,qindx_ID_frag)
 !
end subroutine

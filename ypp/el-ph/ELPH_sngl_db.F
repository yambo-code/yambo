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
 use com,                 ONLY:msg
 use electrons,           ONLY:levels
 use YPP_ELPH,            ONLY:l_gkkp_expand_only_k,elph_dbs_path
 use R_lattice,           ONLY:bz_samp
 use parser_m,            ONLY:parser
 !
#include<memory.h>
 !
 type(bz_samp)      :: k,q
 type(levels)       :: E
 integer            :: qindx_ID,qindx_ID_frag,io_err,iq
 integer, external  :: ELPH_databases_IO_elemental
 integer            :: n_dbs_found
 !
 call parser('GkkpExpOnlyK',l_gkkp_expand_only_k)
 !
 if(l_gkkp_expand_only_k) call ELPH_load_indexes(k,E,q,qindx_ID,qindx_ID_frag)
 !
 call msg('s','Inspecting databases ...')
 n_dbs_found=0
 !
 do iq=1,q%nbz
   io_err=ELPH_databases_IO_elemental("dV EXIST",iq)
   if (io_err==0) n_dbs_found=n_dbs_found+1 
 enddo
 !
 call msg('sr','Found '//intc(n_dbs_found)//' databases in: '//trim(elph_dbs_path))
 if(n_dbs_found==0) return
 !
end subroutine

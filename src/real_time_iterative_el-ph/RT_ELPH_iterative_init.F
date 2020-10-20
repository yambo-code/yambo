!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_ELPH_iterative_init()
 !
 use pars,           ONLY:cZERO
 use RT_lifetimes,   ONLY:l_RT_iterative_ELPH_lifetimes
 use RT_iterative,   ONLY:THETA_matrix,THETA_matrix_index
 use R_lattice,      ONLY:nqbz,nXkibz
 use real_time,      ONLY:RT_bands
 use ELPH,           ONLY:ph_modes
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer             ::iq,i_counter,il,i_s
 !
 if (.not.l_RT_iterative_ELPH_lifetimes) return
 !
 YAMBO_ALLOC(THETA_matrix,(2*nqbz*ph_modes,RT_bands(2)-RT_bands(1)+1,RT_bands(2)-RT_bands(1)+1,nXkibz))
 THETA_matrix=cZERO
 YAMBO_ALLOC(THETA_matrix_index,(nqbz,ph_modes,2))
 !
 i_counter=0
 do iq=1,nqbz
   do il=1,ph_modes
     do i_s=1,2
       i_counter=i_counter+1
       THETA_matrix_index(iq,il,i_s)=i_counter
     enddo
   enddo
 enddo
 !
end subroutine RT_ELPH_iterative_init

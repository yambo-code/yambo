!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_ELPH_zeroing()
 !
 ! The use of the THETA matrix is tricky when the run is parallel.
 ! The point is that every CPU evolves a certain group of q-points.
 ! If I do an all-to-all or a global loading from the DB 
 ! each CPU will have non-zero values for q-points that it will not evolve. During the run,
 ! therefore, values relative to different times will be wrongly merged.
 !
 use drivers,        ONLY:l_elph_scatt
 use pars,           ONLY:cZERO
 use R_lattice,      ONLY:nqbz
 use real_time,      ONLY:THETA_matrix,THETA_matrix_index,l_RT_CCA_Kernel
 use ELPH,           ONLY:ph_modes
 use parallel_m,     ONLY:ncpu
 implicit none
 !
 ! Work Space
 !
 integer :: iq,il,i_s
 !
 if (l_RT_CCA_Kernel) return
 if (.not.l_elph_scatt.or.ncpu==1) return
 !
 do iq=1,nqbz
   do il=1,ph_modes
     do i_s=1,2
       THETA_matrix(THETA_matrix_index(iq,il,i_s),:,:,:) = cZERO
     enddo
   enddo
 enddo
 !
end subroutine RT_ELPH_zeroing

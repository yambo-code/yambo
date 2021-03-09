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
subroutine LEVELS_respect_degenerations(bands,E,IMPOSE)
 !
 use pars,           ONLY:SP
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels,n_bands
 use R_lattice,      ONLY:nkibz
 use stderr,         ONLY:intc
 use vec_operate,    ONLY:degeneration_finder
 !
 implicit none
 !
 integer     :: bands(2)
 type(levels):: E
 logical     :: IMPOSE
 !
 ! Work Space
 !
 integer :: ik,i_n,first_el(n_bands),n_of_el(n_bands),n_deg_grp,i_g,bands_respecting(2),i_c
 !
 bands_respecting=bands
 !
 do ik=1,nkibz
   !
   call degeneration_finder(E%E(:n_bands,ik,1),n_bands,first_el,n_of_el,n_deg_grp,0.0001/HA2EV)
   !
   do i_g=1,n_deg_grp
     do i_c=1,2
       if (bands_respecting(i_c)>=first_el(i_g).and.bands_respecting(i_c)<=first_el(i_g)+n_of_el(i_g)-1) then
         if (i_c==1) bands_respecting(i_c)=first_el(i_g)
         if (i_c==2) bands_respecting(i_c)=first_el(i_g)+n_of_el(i_g)-1
       endif
     enddo
   enddo
   !
 enddo 
 !
 if (all((/bands==bands_respecting/))) return
 !
 call warning("User defined bands ("//trim(intc(bands(1)))//"-"//trim(intc(bands(2)))//") do not respect level degeneracy")
 !
 if (.not.IMPOSE) return
 bands=bands_respecting
 call warning("Bands redefined to be "//trim(intc(bands(1)))//"-"//trim(intc(bands(2))))
 !
end subroutine

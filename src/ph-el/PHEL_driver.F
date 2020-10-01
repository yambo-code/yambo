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
subroutine PHEL_driver(E,k,q)
 !
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp,nqibz
 use ELPH,                ONLY:QP_PH_n_G_bands
 use interfaces,          ONLY:PARALLEL_global_indexes
 !
 implicit none
 !
 type(levels) ,intent(in) :: E
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::i_err,iq
 integer, external   ::ELPH_expanded_databases_check
 !
 call section('*',"Phonon Lifetimes")
 !
 ! Parallel Setup
 !===============
 call PARALLEL_global_indexes(E,k,q,"Phonon_Self_Energy")
 !
 ! El-Ph databases check
 !=======================
 i_err=ELPH_expanded_databases_check(QP_PH_n_G_bands)
 if (i_err<0) then
   call error("Extended GKKP databases absent or damaged")
 endif
 !
 ! Q/K grid remapping
 !====================
 call ELPH_Q_databases_map(k,q)
 !
 ! El-Ph databases I/O
 !=====================
 call ELPH_expanded_databases_load(E,k,q,(/1,QP_PH_n_G_bands/),"IBZ")
 !
 ! Phononic Lifetimes  
 !====================
 do iq=1,nqibz
   call PHEL_lifetimes(iq,E,k,q)
 enddo
 !
end subroutine PHEL_driver

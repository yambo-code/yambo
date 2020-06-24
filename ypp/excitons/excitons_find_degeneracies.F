!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV
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
subroutine excitons_find_degeneracies(BS_E,BS_E_degs)
 !
 use pars,          ONLY:SP
 use com,           ONLY:msg
 use vec_operate,   ONLY:sort
 use BS_solvers,    ONLY:BSS_n_eig
 use YPP,           ONLY:deg_energy
 !
 implicit none
 !
 complex(SP)   :: BS_E(BSS_n_eig)
 integer       :: BS_E_degs(BSS_n_eig)
 !
 ! Work space
 !
 integer :: j1,j2
 integer :: S_indx(BSS_n_eig)
 real(SP):: v2sort(BSS_n_eig)
 !
 call msg('s',':: Sorting energies')
 !==================================
 !
 v2sort=real(BS_E)
 call sort(arrin=v2sort,indx=S_indx)
 !
 BS_E_degs=0
 !
 do j1=1,BSS_n_eig
   !
   if (BS_E_degs(S_indx(j1))>0) cycle
   !
   BS_E_degs(S_indx(j1))=S_indx(j1)
   !
   do j2=j1+1,BSS_n_eig
     if ( abs( real(BS_E(S_indx(j1)))-real(BS_E(S_indx(j2))) )>deg_energy) exit
     BS_E_degs(S_indx(j2))=S_indx(j1)
   enddo
   !
 enddo
 !
end subroutine excitons_find_degeneracies

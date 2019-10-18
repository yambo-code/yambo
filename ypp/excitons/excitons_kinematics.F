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
subroutine excitons_kinematics(Xk,eh,i_s)
 !
 use electrons,     ONLY:spin
 use R_lattice,     ONLY:bz_samp
 use YPPm,          ONLY:EXCITON_kin
 use BS_solvers,    ONLY:BSS_eh_table
 !
 implicit none
 !
 type(bz_samp)          :: Xk
 type(EXCITON_kin)      :: eh
 integer                :: i_s
 ! 
 eh%ik_bz   = BSS_eh_table(i_s,1)
 eh%ik_ibz  = Xk%sstar(eh%ik_bz,1)
 eh%ik_sym  = Xk%sstar(eh%ik_bz,2)
 eh%iv      = BSS_eh_table(i_s,2)
 eh%ic      = BSS_eh_table(i_s,3)
 eh%spin    =spin(BSS_eh_table(i_s,:))
 !
end subroutine

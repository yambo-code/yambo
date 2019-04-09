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
subroutine excitons_collinear(Singlet,Triplet,S_eh)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:spin
 use BS,            ONLY:BS_H_dim
 use BS_solvers,    ONLY:BS_mat,BSS_eh_table,BSS_n_eig
 use YPP,           ONLY:EXCITONS_user_indexes,EXCITONS_n_user_states
 !
 implicit none
 !
 real(SP)    :: Singlet(BSS_n_eig),Triplet(BSS_n_eig,3),S_eh(BSS_n_eig)
 ! 
 ! Work Space
 !
 real(SP) :: spin_factor(2)
 integer  :: i_l,i_s,i_c,i_spin
 !
 spin_factor=(/1.,-1./)
 !
 do i_c=1,EXCITONS_n_user_states
   !
   i_l=EXCITONS_user_indexes(i_c)
   !
   !NEW 
   do i_s=1,BS_H_dim
     !
     i_spin=spin(BSS_eh_table(i_s,:))
     !
     Triplet(i_l,2)=Triplet(i_l,2)+1./sqrt(2.)*BS_mat(i_s,i_l)*conjg(BS_mat(i_s,i_l))*spin_factor(i_spin)
     Singlet(i_l)  =Singlet(i_l)  +1./sqrt(2.)*BS_mat(i_s,i_l)*conjg(BS_mat(i_s,i_l))
     !
   enddo
   !
   S_eh(i_l)=sqrt(2.)*abs(Triplet(i_l,2))
   !
   !OLD 
!   do i_s=1,BS_H_dim,2
!     S_sq(i_l)=S_sq(i_l)+BS_mat(i_s,i_l)*conjg(BS_mat(i_s+1,i_l))+&
!&                                 (BS_mat(i_s+1,i_l)*conjg(BS_mat(i_s,i_l)))
!   enddo
   !
 enddo
 !
end subroutine

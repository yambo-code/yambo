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
subroutine excitons_collinear(lambda,n_lambda,deg_list,S_z,S_sq)
 !
 use pars,          ONLY:SP
 use BS_solvers,    ONLY:BS_mat,BSS_eh_table,BSS_n_eig
 !
 implicit none
 !
 real(SP)    :: S_sq(BSS_n_eig),S_z(BSS_n_eig)
 integer     :: deg_list(BSS_n_eig),n_lambda,lambda(n_lambda)
 ! 
 ! Work Space
 !
 integer :: i_l,i_s,i_deg,n_deg
 !
 S_z(:)=0
 S_sq(:)=0
 !
 ! First calculates S_sq and S_z for all states
 !
 do i_l=1,n_lambda
   !
   do i_s=1,BSS_n_eig
     write (*,*) BSS_eh_table(i_s,:)
   enddo
   !
 enddo
 stop


 do i_l=1,BSS_n_eig 
   !
   !
!   do i1=1,BSS_n_eig,2
!     S_sq(i_l)=S_sq(i_l)+BS_mat(i1,i_l)*conjg(BS_mat(i1+1,i_l))+&
!&                              (BS_mat(i1+1,i_l)*conjg(BS_mat(i1,i_l)))
!   enddo
   !
 enddo
 !
 ! then sum over degenerate states
 !
 do i_l=1,BSS_n_eig
   !
   if (deg_list(i_l)/=i_l) cycle
   !
   n_deg=count(deg_list==i_l)
   !
   do i_deg=i_l+1,i_l+n_deg-1
     S_sq(i_l)=S_sq(i_l)+S_sq(i_deg)
   enddo
   !
   S_sq(i_l)=1._SP-S_sq(i_l)
   !
   do i_deg=deg_list(i_l)+1,deg_list(i_l)+n_deg-1
     S_sq(i_deg)=S_sq(i_l)
     S_z(i_deg)=S_z(i_l)
   enddo
   !
 enddo
 !
end subroutine

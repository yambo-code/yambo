!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine excitons_collinear(deg_list,S_z,S_sq)
 !
 use pars,          ONLY:SP
 use BS_solvers,    ONLY:BS_mat
 use BS,            ONLY:BS_H_dim
 !
 implicit none
 !
 real(SP)    :: S_sq(BS_H_dim),S_z(BS_H_dim)
 integer     :: deg_list(BS_H_dim)
 ! 
 ! Work Space
 !
 integer :: lambda,i1,lambda_deg,n_lambda_deg
 !
 S_z(:)=0
 S_sq(:)=0
 !
 ! First calculates S_sq and S_z for all states
 !
 do lambda=1,BS_H_dim
   !
   do i1=1,BS_H_dim,2
     S_sq(lambda)=S_sq(lambda)+BS_mat(i1,lambda)*conjg(BS_mat(i1+1,lambda))+&
&                                 (BS_mat(i1+1,lambda)*conjg(BS_mat(i1,lambda)))
   enddo
   !
 enddo
 !
 ! then sum over degenerate states
 !
 do lambda=1,BS_H_dim
   !
   if (deg_list(lambda)/=lambda) cycle
   !
   n_lambda_deg=count(deg_list==lambda)
   !
   do lambda_deg=lambda+1,lambda+n_lambda_deg-1
     S_sq(lambda)=S_sq(lambda)+S_sq(lambda_deg)
   enddo
   !
   S_sq(lambda)=1._SP-S_sq(lambda)
   !
   do lambda_deg=deg_list(lambda)+1,deg_list(lambda)+n_lambda_deg-1
     S_sq(lambda_deg)=S_sq(lambda)
     S_z(lambda_deg)=S_z(lambda)
   enddo
   !
 enddo
 !
end subroutine

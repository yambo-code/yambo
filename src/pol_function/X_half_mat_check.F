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
subroutine X_half_mat_check(Xen,Xk,Xw)
 !
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:i_space_inv
 use X_m,           ONLY:X_FILL_UP_matrix_only
 !
 implicit none
 !
 type(levels) ,intent(in)   :: Xen
 type(bz_samp),intent(in)   :: Xk
 type(w_samp) ,intent(in)   :: Xw
 !
 call WF_spatial_inversion(Xen,Xk)
 !
 if (i_space_inv==1) then
   X_FILL_UP_matrix_only= .TRUE.
 else
   X_FILL_UP_matrix_only= all( aimag(Xw%p(:))<1.E-4 ).and. all( real(Xw%p(:))<1.E-4 )
 endif
 !
end subroutine

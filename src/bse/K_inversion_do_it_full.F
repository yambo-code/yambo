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
subroutine K_inversion_do_it_full(Lo,KERNEL,what)
 !
 ! 1./(Lo^{-1}-M)
 !
 use pars,           ONLY:SP,cZERO
 use BS_solvers,     ONLY:BS_mat
 use BS,             ONLY:BS_H_dim
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver
 use linear_algebra, ONLY:INV
 !
 implicit none
 !
 character  ,intent(in) ::what ! "r","a"
 complex(SP),intent(in) ::Lo(BS_H_dim)
 complex(SP),intent(out)::KERNEL(BS_H_dim,BS_H_dim) 
 !
 ! Work Space
 !
 integer     ::i1,i2
 !
 KERNEL=cZERO
 !
 do i2=1,BS_H_dim
   KERNEL(i2,i2)=1._SP/Lo(i2)
   do i1=1,BS_H_dim 
     if (what=="r") KERNEL(i1,i2)=KERNEL(i1,i2)-      BS_mat(i1,i2)
     if (what=="a") KERNEL(i1,i2)=KERNEL(i1,i2)-conjg(BS_mat(i1,i2))
   enddo
 enddo
 !
 call LINEAR_ALGEBRA_driver(INV,M=KERNEL)
 !
end subroutine

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
subroutine crystal_lattice()
 !
 use pars,           ONLY:SP
 use D_lattice,      ONLY:a,alat,lattice
 use vec_operate   , ONLY:v_norm
 use matrix_operate, ONLY:m3inv
 use com,            ONLY:warning
 implicit none
 !
 ! Work Space
 !
 integer, parameter :: n_lattices=4,n_rep=3
 real(SP)           :: test_a(3,3,n_lattices),a_test_m1(3,3),&
&                      a_m1(3,3),coeff(3),v(3)
 character(7)       :: lattice_name(n_lattices)
 integer            :: i_l,i,j,k
 logical            :: lattice_test_failed(n_lattices)
 !
 ! Load standard lattice vectors 
 ! (taken from http://cst-www.nrl.navy.mil/bind/kpts/index.html)
 !
 lattice_name(1)='HCP'
 test_a(1,:,1) = (/ 1./2. , -sqrt(3.)/2., 0./)*alat(1)
 test_a(2,:,1) = (/ 1./2. ,  sqrt(3.)/2., 0./)*alat(1)
 test_a(3,:,1) = (/ 0.    ,  0.         , 1./)*alat(3)
 !
 lattice_name(2)='BCC'
 test_a(1,:,2) = (/-1./2. , 1./2.   , 1./2.  /)*alat(1)
 test_a(2,:,2) = (/ 1./2. ,-1./2.   , 1./2.  /)*alat(1)
 test_a(3,:,2) = (/ 1./2. , 1./2.   ,-1./2.  /)*alat(1)
 !
 lattice_name(3)='FCC'
 test_a(1,:,3) = (/ 0.    , 1./2.   , 1./2.  /)*alat(1)
 test_a(2,:,3) = (/ 1./2. , 0.      , 1./2.  /)*alat(1)
 test_a(3,:,3) = (/ 1./2. , 1./2.   , 0.     /)*alat(1)
 !
 lattice_name(4)='CUB'
 test_a(1,:,4) = (/ 1.    , 0.      , 0.     /)*alat(1)
 test_a(2,:,4) = (/ 0.    , 1.      , 0.     /)*alat(1)
 test_a(3,:,4) = (/ 0.    , 0.      , 1.     /)*alat(1)
 !
 call m3inv(a,a_m1)
 !
 lattice_test_failed=.FALSE.
 !
 do i_l=1,n_lattices
   call m3inv(test_a(:,:,i_l),a_test_m1)
   do i=0,n_rep
     do j=0,n_rep
       do k=0,n_rep
         !
         v=i*test_a(1,:,i_l)+j*test_a(2,:,i_l)+k*test_a(3,:,i_l)
         coeff=matmul(v,a_m1)
         if (.not.lattice_test_failed(i_l)) lattice_test_failed(i_l) = &
&           v_norm( real(nint(coeff))-coeff  )  > 1.E-5
         !
         v=i*a(1,:)+j*a(2,:)+k*a(3,:)
         coeff=matmul(v,a_test_m1)
         if (.not.lattice_test_failed(i_l)) lattice_test_failed(i_l) = &
&           v_norm( real(nint(coeff))-coeff  )  > 1.E-5
         !
       enddo
     enddo
   enddo
 enddo
 !
 if (  count (.not.lattice_test_failed)  > 1 ) then
   call warning('Two or more crystal strcutures fit the given cell')
 else if (  count (.not.lattice_test_failed) == 1 ) then
   do i_l=1,n_lattices
     if (.not.lattice_test_failed(i_l)) lattice=lattice_name(i_l)
   enddo
 endif
 !
end subroutine

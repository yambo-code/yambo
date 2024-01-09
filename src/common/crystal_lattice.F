!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine crystal_lattice()
 !
 use pars,           ONLY:SP,schlen
 use D_lattice,      ONLY:a,alat,lattice,dl_sop,nsym
 use vec_operate   , ONLY:v_norm
 use matrix_operate, ONLY:m3inv,m3det
 !
 implicit none
 !
 ! Work Space
 !
 integer, parameter :: n_lattices=5,n_rep=3,n_pre=3
 real(SP)           :: latt_a(3,3,n_lattices),latt_a_m1(3,3),&
&                      a_m1(3,3),coeff(3),v(3),latt_a_rot(3,3),pre_factor(n_pre)
 character(schlen)  :: lattice_name(n_lattices)
 integer            :: i_l,i,j,k,i_s,i_p
 logical            :: lattice_test_failed(n_lattices,n_pre)
 !
 ! Load standard lattice vectors 
 !
 ! Refs.  http://cst-www.nrl.navy.mil/bind/kpts/index.html (out of date)
 !        http://web.mit.edu/espresso_v6.1/i386_linux26/qe-6.1/Doc/brillouin_zones.pdf 
 !
 lattice_name(1)='HCP'
 latt_a(1,:,1) = (/ 1._SP/2._SP , -sqrt(3._SP)/2._SP, 0._SP /)*alat(1)
 latt_a(2,:,1) = (/ 1._SP/2._SP ,  sqrt(3._SP)/2._SP, 0._SP /)*alat(1)
 latt_a(3,:,1) = (/ 0._SP       ,  0._SP            , 1._SP /)*alat(3)
 !
 lattice_name(2)='BCC'
 latt_a(1,:,2) = (/-1._SP/2._SP , 1._SP/2._SP, 1._SP/2._SP /)*alat(1)
 latt_a(2,:,2) = (/ 1._SP/2._SP ,-1._SP/2._SP, 1._SP/2._SP /)*alat(1)
 latt_a(3,:,2) = (/ 1._SP/2._SP , 1._SP/2._SP,-1._SP/2._SP /)*alat(1)
 !
 lattice_name(3)='FCC'
 latt_a(1,:,3) = (/ 0._SP       , 1._SP/2._SP, 1._SP/2._SP /)*alat(1)
 latt_a(2,:,3) = (/ 1._SP/2._SP , 0._SP      , 1._SP/2._SP /)*alat(1)
 latt_a(3,:,3) = (/ 1._SP/2._SP , 1._SP/2._SP, 0._SP       /)*alat(1)
 !
 lattice_name(4)='CUB'
 latt_a(1,:,4) = (/ 1._SP       , 0._SP      , 0._SP /)*alat(1)
 latt_a(2,:,4) = (/ 0._SP       , 1._SP      , 0._SP /)*alat(1)
 latt_a(3,:,4) = (/ 0._SP       , 0._SP      , 1._SP /)*alat(1)
 !
 if (a(1,2)<a(1,1)) lattice_name(5)='FC-ORTHOROMBIC (b<a)'
 if (a(1,2)>a(1,1)) lattice_name(5)='FC-ORTHOROMBIC (b>a)'
 latt_a(1,:,5) = (/ a(1,1) , a(1,2) , 0._SP   /)
 latt_a(2,:,5) = (/-a(1,1) , a(1,2) , 0._SP   /)
 latt_a(3,:,5) = (/ 0._SP  , 0._SP  , alat(3) /)
 !
 call m3inv(a,a_m1)
 !
 pre_factor=(/.5_SP,1._SP,2._SP/)
 !
 lattice_test_failed=.FALSE.
 !
 do i_l=1,n_lattices
   !
   do i_p=1,n_pre
     !
     do i_s=1,nsym
       !
       do i=1,3
         latt_a_rot(i,:)=matmul(dl_sop(:,:,i_s),latt_a(i,:,i_l)*pre_factor(i_p))
       enddo
       !
       if (abs(m3det(latt_a_rot))<=1.E-7) then
         lattice_test_failed(i_l,i_p)=.TRUE.
         cycle
       endif
       !
       call m3inv(latt_a_rot,latt_a_m1)
       !
       do i=0,n_rep
         do j=0,n_rep
           do k=0,n_rep
             !
             v=i*latt_a_rot(1,:)+j*latt_a_rot(2,:)+k*latt_a_rot(3,:)
             coeff=matmul(v,a_m1)
             if (.not.lattice_test_failed(i_l,i_p)) lattice_test_failed(i_l,i_p) = v_norm( real(nint(coeff))-coeff  )  > 1.E-5
             !
             v=i*a(1,:)+j*a(2,:)+k*a(3,:)
             coeff=matmul(v,latt_a_m1)
             if (.not.lattice_test_failed(i_l,i_p)) lattice_test_failed(i_l,i_p) = v_norm( real(nint(coeff))-coeff  )  > 1.E-5
             !
           enddo
         enddo
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
 if (  count (.not.lattice_test_failed)  > 1 ) then
   call warning('Two or more crystal structures fit the given cell')
 else if (  count (.not.lattice_test_failed) == 1 ) then
   do i_l=1,n_lattices
     if (any(.not.lattice_test_failed(i_l,:))) lattice=lattice_name(i_l)
   enddo
 endif
 !
end subroutine

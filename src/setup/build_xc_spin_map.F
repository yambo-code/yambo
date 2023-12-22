!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine build_xc_spin_map()
 !
 ! see http://www.tddft.org/programs/libxc/manual/
 !
 ! 1 for fxc and gradients
 ! 2 for kxc = the derivative of fxc, at present not used in yambo
 !
 use electrons,            ONLY:n_spin
 use xc_functionals,       ONLY:xc_spin,xc_spin_map1,xc_spin_map2
 !
 implicit none
 !
 if (n_spin==1) then
   xc_spin=1
   allocate(xc_spin_map1(1,1))
   allocate(xc_spin_map2(1,1,1))
   xc_spin_map1=1
   xc_spin_map2=1
   return
 endif
 !
 ! fxc and grad
 !
 xc_spin(1)=3
 !
 allocate(xc_spin_map1(n_spin,n_spin))
 !
 xc_spin_map1(1,1)=1
 xc_spin_map1(1,2)=2
 xc_spin_map1(2,1)=2
 xc_spin_map1(2,2)=3
 !
 ! kxc
 !
 xc_spin(2)=4
 !
 allocate(xc_spin_map2(n_spin,n_spin,n_spin))
 !
 xc_spin_map2(1,1,1)=1
 xc_spin_map2(2,1,1)=2
 xc_spin_map2(1,2,1)=2
 xc_spin_map2(1,1,2)=2
 xc_spin_map2(1,2,2)=3
 xc_spin_map2(2,1,2)=3
 xc_spin_map2(2,2,1)=3
 xc_spin_map2(2,2,2)=4
 !
end subroutine build_xc_spin_map

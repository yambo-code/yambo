!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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

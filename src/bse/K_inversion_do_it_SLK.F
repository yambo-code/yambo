!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine K_inversion_do_it_SLK(Lo,what)
 !
 ! 1./(Lo^{-1}-M) (using SLK procedures)
 !
 use pars,           ONLY:SP
 use BS,             ONLY:BS_H_dim
 use BS_solvers,     ONLY:K_slk,K_inv_slk
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver
 use linear_algebra, ONLY:INV
 !
 implicit none
 !
 character  ,intent(in) ::what ! "r","a"
 complex(SP),intent(in) ::Lo(BS_H_dim)
 !
 ! Work Space
 !
 integer     ::i_r,borders(2)
 !
 if (what=="r") K_inv_slk%blc=-K_slk%blc
 if (what=="a") K_inv_slk%blc=-conjg(K_slk%blc)
 ! 
 borders=(/max(K_slk%rows(1),K_slk%cols(1)),min(K_slk%rows(2),K_slk%cols(2))/)
 !
 if (borders(1)<borders(2)) then
   do i_r=borders(1),borders(2)
     !
     K_inv_slk%blc(i_r,i_r,1)=K_inv_slk%blc(i_r,i_r,1)+1._SP/Lo(i_r)
     !
   enddo
 endif
 !
 call LINEAR_ALGEBRA_driver(INV,M_slk=K_inv_slk)
 !
end subroutine

!
!        Copyright (C) 2000-2016 the YAMBO team
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
 use BS,             ONLY:BS_H_dim,BS_blk
#if defined _SCALAPACK
 use BS_solvers,     ONLY:BSS_inv_blk,K_diago_in_BS_mat
 use BS,             ONLY:n_BS_blks
#endif
 use parallel_m,     ONLY:ncpu
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
#if defined _SCALAPACK
 integer        :: i_B,i_r
#endif
 !
 KERNEL=cZERO
 !
#if defined _SCALAPACK
 if (ncpu==1) then
#endif
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
#if defined _SCALAPACK
   !
 else
   ! 
   do i_B=1,n_BS_blks
     !
     if (what=="r") BSS_inv_blk(i_B)%mat=-      BS_blk(i_B)%mat
     if (what=="a") BSS_inv_blk(i_B)%mat=-conjg(BS_blk(i_B)%mat)
     !
     if (BSS_inv_blk(i_B)%mode=="A"                  ) cycle
     if (BSS_inv_blk(i_B)%iT_p/=BSS_inv_blk(i_B)%iT_k) cycle
     !
     do i_r=1,min(BSS_inv_blk(i_B)%size(1),BSS_inv_blk(i_B)%size(2))
       !
       if (.not.K_diago_in_BS_mat) BSS_inv_blk(i_B)%mat(i_r,i_r)=cZERO
       !
       BSS_inv_blk(i_B)%mat(i_r,i_r)=real(BSS_inv_blk(i_B)%mat(i_r,i_r))+1._SP/Lo( BSS_inv_blk(i_B)%coordinate(1)+i_r-1 )
       !
     enddo
     !
   enddo
   !
   call LINEAR_ALGEBRA_driver(INV,M=KERNEL,M_bse=BSS_inv_blk)
   !
 endif
 !
#endif
 !
end subroutine

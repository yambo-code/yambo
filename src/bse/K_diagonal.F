!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine K_diagonal(iq,what)
 !
 use pars,           ONLY:SP,cZERO,cI
 use parser_m,       ONLY:parser
 use BS_solvers,     ONLY:BS_mat,BSS_eh_f,BSS_eh_E,K_diago_in_BS_mat,BSS_eh_W,BS_diagonal,K_slk
 use BS,             ONLY:BS_K_dim,BSE_mode,BS_H_dim
 use SLK_m,          ONLY:SLK_POOL
 use parallel_int,   ONLY:PP_redux_wait
 !
#include<memory.h>
 !
 integer,       intent(in) :: iq
 character(*)              :: what
 !
 ! Work Space
 !
 integer          :: i_r,i_c,rows(2),cols(2)
 real(SP)         :: f_r,f_c
 complex(SP)      :: E_plus_W,pre_factor
 !
 ! Diagonal (Energies) 
 !====================
 if ( (K_slk%kind=="SLK".and.index(what,"add_E")>0) .or.&
&     (.not.K_slk%kind=="SLK".and.index(what,"rm_E")>0) ) then
   !
   rows=(/1,BS_H_dim/)
   if (K_slk%kind=="SLK") then
     rows=(/max(K_slk%rows(1),K_slk%cols(1)),min(K_slk%rows(2),K_slk%cols(2))/)
   endif
   !
   do i_r=rows(1),rows(2)
     !
     if (.not.allocated(BSS_eh_W)) E_plus_W=BSS_eh_E(i_r)
     if (     allocated(BSS_eh_W)) E_plus_W=BSS_eh_E(i_r)-cI*BSS_eh_W(i_r)
     !
     if (K_slk%kind=="SLK".and.index(what,"add_E")>0)then
       !
       if (i_r<=BS_K_dim(1)) then
         K_slk%blc(i_r,i_r,1)=K_slk%blc(i_r,i_r,1)+E_plus_W
       else if (cpl(i_r)) then
         K_slk%blc(i_r,i_r,1)=K_slk%blc(i_r,i_r,1)-conjg(E_plus_W)
       endif
       !
     else if (.not.K_slk%kind=="SLK".and.index(what,"rm_E")>0)then
       !
       if (i_r<=BS_K_dim(1)) then
         BS_mat(i_r,i_r)=BS_mat(i_r,i_r)-E_plus_W
       else if (cpl(i_r)) then
         BS_mat(i_r,i_r)= BS_mat(i_r,i_r)+conjg(E_plus_W)
       endif
       !
     endif
     !
   enddo
   !
 endif
 !
 ! Occupations 
 !=============
 if ( index(what,"rm_OCC")>0) then
   rows=(/1,BS_H_dim/)
   cols=(/1,BS_H_dim/)
   if (K_slk%kind=="SLK") then
     rows=K_slk%rows
     cols=K_slk%cols
   endif
   do i_r=rows(1),rows(2)
     !
     if (.not.cpl(i_r)) f_r=sqrt(BSS_eh_f(i_r))
     if (     cpl(i_r)) f_r=sqrt(BSS_eh_f(i_r-BS_K_dim(1)))
     !
     do i_c=cols(1),cols(2)
       !
       if (.not.cpl(i_c)) f_c=sqrt(BSS_eh_f(i_c))
       if (     cpl(i_c)) f_c=sqrt(BSS_eh_f(i_c-BS_K_dim(1)))
       !
       pre_factor=1/cI
       if (.not.cpl(i_r).and..not.cpl(i_c)) pre_factor= 1
       if (     cpl(i_r).and.     cpl(i_c)) pre_factor=-1
       !
       if (K_slk%kind=="SLK") then
         !
         K_slk%blc(i_r,i_c,1)=K_slk%blc(i_r,i_c,1)/f_r/f_c*pre_factor
         !
       else
         !
         BS_mat(i_r,i_c)=BS_mat(i_r,i_c)/f_r/f_c*pre_factor
         !
       endif
     enddo
   enddo
   !
 endif
 !
 ! Diagonal 
 !=========
 !
 if (index(what,"DIAG")/=0) then
   !
   rows=(/1,BS_H_dim/)
   if (K_slk%kind=="SLK") then
     rows=(/max(K_slk%rows(1),K_slk%cols(1)),min(K_slk%rows(2),K_slk%cols(2))/)
   endif
   !
   call parser('BSSInvKdiag',K_diago_in_BS_mat)
   !
   if (.not.K_diago_in_BS_mat) then
     !
     YAMBO_ALLOC(BS_diagonal,(BS_K_dim(1)))
     BS_diagonal=cZERO
     !
     if (rows(1)<=rows(2)) then
       do i_r=rows(1),rows(2)
         !
         if (K_slk%kind=="SLK") then
           if (.not.cpl(i_r).and.SLK_POOL%ID==0) BS_diagonal(i_r)=K_slk%blc(i_r,i_r,1)
           K_slk%blc(i_r,i_r,1)=cZERO
         else
           if (.not.cpl(i_r)) BS_diagonal(i_r)=BS_mat(i_r,i_r)
           BS_mat(i_r,i_r)=cZERO
         endif
       enddo
     endif
     !
     if (K_slk%kind=="SLK") call PP_redux_wait(BS_diagonal)
     !
   endif
   !
 endif
 !
 contains
   !
   logical function cpl(I)
     integer :: I
     cpl=index(BSE_mode,'coupling')>0.and.I>BS_K_dim(1)
   end function
   !
end subroutine K_diagonal

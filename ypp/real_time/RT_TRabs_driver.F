!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DV DS
!
subroutine RT_transient_absorption_driver(k,Xk,en,Xen,q)
 !
 use pars,          ONLY:SP,pi,schlen
 use R_lattice,     ONLY:bz_samp
 use stderr,        ONLY:intc
 use electrons,     ONLY:levels
 !
#include<y_memory.h>
 !
 type(bz_samp) ::Xk,k,q
 type(levels)  ::Xen,en
 !
 ! Work Space
 !
 logical              :: l_flag
 integer ,allocatable :: BS_E_degs(:)
 !... I/0
 integer              :: iq,io_err
 !
 call section('*','Transient absorption')
 !
 call                excitons_read(k,Xk,en,Xen,BSiq,"check",io_err)
 !
 if (io_err==0) call excitons_read(k,Xk,en,Xen,BSiq,"eigenvalues",io_err)
 !
 if (io_err/=0) then
   call warning("BSE databases not found. IP transient absorption")
   call RT_transient_absorption(Xen,Xk,q)
   return
 endif
 !
 call excitons_read(k,Xk,en,Xen,BSiq,"eigenvectors",io_err)
 ! 
 ! Transient absorption 
 !======================
 call RT_transient_absorption(Xen,Xk,q)
 !
 ! CLEAN
 !
 call excitons_read(k,Xk,en,Xen,BSiq,"clean",io_err)
 !
end subroutine

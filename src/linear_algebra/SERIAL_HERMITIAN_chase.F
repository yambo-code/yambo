!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AF
!
! #CHASE STAMP      
!
subroutine SERIAL_HERMITIAN_chase(n,M,E,neig)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_nl_optics,l_real_time
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,&
&                         LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,       ONLY:timing
 !
#include<y_memory.h>
 !
 integer     :: n,neig
 complex(SP) :: M(n,n)
 real(SP)    :: E(neig)

 integer     :: nev, nex, deg, ierr
 real(SP)    :: tol
 character   :: mode, opt, qr
 !
 character(64)     :: subname="SERIAL_HERMITIAN_chase"
 complex(SP), allocatable :: Vtemp(:,:)
 real(SP),    allocatable :: Etemp(:)

 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_HERMITIAN_chase',OPR='start')
 endif
 ! 
 !
 E=0.0
 !
 nev = min(neig,n)
 nex = min(n-nev,nev)
 !
 deg = 20
 tol = 1e-8
 mode = 'R'
 opt = 'S'
 qr = 'C'
 !
 allocate(Vtemp(n,nev+nex))
 allocate(Etemp(nev+nex))
 Vtemp = 0.0
 Etemp = 0.0
 ! 
 call timing('ChASE_INIT',OPR='start')
 !
#if defined _DOUBLE
 call zchase_init(n, nev, nex, M, n, Vtemp, Etemp, ierr)
#else
 call cchase_init(n, nev, nex, M, n, Vtemp, Etemp, ierr)
#endif
 !
 call timing('ChASE_INIT',OPR='stop')
 !
#if defined _DOUBLE
 call zchase(deg,tol,mode,opt,qr)
#else
 call cchase(deg,tol,mode,opt,qr)
#endif
 ! 
#if defined _DOUBLE
 call zchase_finalize(ierr)
#else
 call cchase_finalize(ierr)
#endif
 !
 if (ierr/=0) call warning("[WARNING] ChASE did not finalize properly")
 !
 E(1:nev) = Etemp(1:nev)
 M(:,1:nev) = Vtemp(:,1:nev)
 M(:,nev+1:) = 0.0_SP
 !
 deallocate(Etemp)
 deallocate(Vtemp)
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_HERMITIAN_chase',OPR='stop')
 endif
 !
end subroutine

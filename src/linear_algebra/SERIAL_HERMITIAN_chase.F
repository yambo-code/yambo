!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AF
!
! #CHASE STAMP      

subroutine SERIAL_HERMITIAN_chase(n,M,E,neig)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_nl_optics,l_real_time
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,&
&                         LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,       ONLY:timing
#include<y_memory.h>
 !
 integer     :: n,neig,ii
 complex(SP) :: M(n,n)
 real(SP)    :: E(neig),abstol

 integer :: nev, nex, init, deg, flag
 real(SP) :: tol
 character :: mode, opt, qr
 !
 character(64)     :: subname="SERIAL_HERMITIAN_chase"
 complex(SP), allocatable :: Vtemp(:,:)
 real(SP), allocatable :: Etemp(:)

 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_HERMITIAN_chase',OPR='start')
 endif
 
allocate(Vtemp(n,n))
 Vtemp = 0.0
!
 nev = min(neig,n)
 nex = max(0,min(10,n-nev))
 deg = 20
 tol = 1e-10
 mode = 'R'
 opt = 'S'
 qr = 'C'

 print *, "SERIAL ChASE, Matrix sample :"
 print *, M(1:5,1:5)

allocate(Etemp(nev+nex))
 Etemp = 0.0
 
call timing('ChASE_INIT',OPR='start')
#if defined _DOUBLE
 print *, "double go!"
 call zchase_init(n, nev, nex, M, n, Vtemp, Etemp, init)
#else
 print *, "simple go!"
 call cchase_init(n, nev, nex, M, n, Vtemp, Etemp, init)
#endif

   call timing('ChASE_INIT',OPR='stop')

 print *, "Initialized"

#if defined _DOUBLE
 call zchase(deg,tol,mode,opt,qr)
#else
 call cchase(deg,tol,mode,opt,qr)
#endif
 
print *, "Solved"

#if defined _DOUBLE
 call zchase_finalize(flag)
#else
 call cchase_finalize(flag)
#endif

 if(flag/=0) print *, "[WARNING] ChASE did not finalize properly"
 !
 do ii = 1,nev
        E(ii) = Etemp(ii)
 end do
 M = Vtemp
 print *, E(:)
 deallocate(Etemp)
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_HERMITIAN_chase',OPR='stop')
 endif
 !
end subroutine

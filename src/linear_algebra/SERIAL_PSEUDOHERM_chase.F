!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AF
!
! #CHASE STAMP      
!
subroutine SERIAL_PSEUDOHERM_chase(n,M,E,V_left,V_right,neig)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_nl_optics,l_real_time
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,&
&                         LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,       ONLY:timing
 use chase_diag
 !
#include<y_memory.h>
 !
 integer     :: n,neig
 complex(SP) :: M(n,n)
 real(SP)    :: E(neig)
 complex(SP) :: V_left(n,*),V_right(n,*)

 integer     :: nev,nex,ierr,deg,is,ie
 real(SP)    :: tol
 character   :: mode,opt,qr
 !
 character(64)     :: subname="SERIAL_PSEUDOHERM_chase"
 complex(SP), allocatable :: Vtemp(:,:)
 real(SP),    allocatable :: Etemp(:)

 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_PSEUDOHERM_chase',OPR='start')
 endif
 ! 
 E=0.0
 !
 ! AF: Warning if neig too large (eg > 25-30% of n)
 ! AF: neig is here assumed to be the number of positive eigs
 !
 nev = min(neig,n)
 nex = min(100,nev)
 !
#ifdef _DOUBLE
 deg = 20
 tol = 1e-8
#else
 deg = 10
 tol = 1e-4
#endif
 !
 mode = 'R'
 opt = 'S'
 qr = 'C'
 !
 YAMBO_ALLOC(Vtemp,(n,2*(nev+nex)))
 YAMBO_ALLOC(Etemp,(2*(nev+nex)))
 Vtemp = 0.0
 Etemp = 0.0
 ! 
 call timing('ChASE_PS_INIT',OPR='start')
 !
#if defined _DOUBLE
 call zchase_init_pseudo(n, nev, nex, M, n, Vtemp, Etemp, init=ierr)
#else
 call cchase_init_pseudo(n, nev, nex, M, n, Vtemp, Etemp, init=ierr)
#endif
 !
 call timing('ChASE_PS_INIT',OPR='stop')
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
 if (ierr/=0) call warning("[WARNING] ChASE_PS did not finalize properly")
 !
 E(1:nev) = Etemp(1:nev)
 V_right(:,1:nev) = Vtemp(:,1:nev)
 !
 ! Left eigenvectors: L = S R with S=diag(I,-I) for BSE pseudo-Hermitian.
 ! If R=(X,Y), then L=(X,-Y). Must copy the upper half as well.
 !
 is=n/2+1
 ie=n
 V_left(1:n/2,1:nev) =  Vtemp(1:n/2,1:nev)
 V_left(is:ie,1:nev) = -Vtemp(is:ie,1:nev)
 !
 YAMBO_FREE(Etemp)
 YAMBO_FREE(Vtemp)
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_PSEUDOHERM_chase',OPR='stop')
 endif
 !
end subroutine SERIAL_PSEUDOHERM_chase

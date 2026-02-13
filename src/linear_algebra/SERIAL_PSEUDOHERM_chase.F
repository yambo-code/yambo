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
 call timing('ChASE_PS_INIT',OPR='start')
 !
#if defined _DOUBLE
 call zchase_init_pseudo(n, nev, nex, M, n, Vtemp, Etemp, ierr)
#else
 call cchase_init_pseudo(n, nev, nex, M, n, Vtemp, Etemp, ierr)
#endif
 !
 call timing('ChASE_PS_INIT',OPR='stop')
 !
#if defined _DOUBLE
 call zchase_pseudo(deg,tol,mode,opt,qr)
#else
 call cchase_pseudo(deg,tol,mode,opt,qr)
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
 is=n/2+1
 ie=n
 V_left(is:ie,1:nev) = -Vtemp(is:ie,1:nev)
 !
 deallocate(Etemp)
 deallocate(Vtemp)
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_PSEUDOHERM_chase',OPR='stop')
 endif
 !
end subroutine SERIAL_PSEUDOHERM_chase

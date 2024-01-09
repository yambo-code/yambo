!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine CLOSE_the_run(MEM,TIME,INIT_stop)
 !
 use timing_m,       ONLY:timing_deallocate,global_list
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use electrons,      ONLY:levels,E_reset
 use frequency,      ONLY:w_samp,W_reset
 use X_m,            ONLY:X_t,X_reset
 use DIPOLES,        ONLY:DIPOLE_t
 !
 implicit none
 !
 logical :: MEM,TIME,INIT_stop
 ! 
 ! Work Space
 !
 type(levels)      ::en
 type(bz_samp)     ::q,k
 type(X_t)         ::X(4)
 type(w_samp)      ::Xw(4)
 type(DIPOLE_t)    :: Dip
 integer           ::ierr,i_c
 integer, external ::INIT
 !
 ! Timing Overview
 !=================
 !
 if (TIME) then
   !
   call section('*','Timing Overview')
   !
   call TIMING_overview(global_list,"yambo")
   !
 endif
 !
#if defined _MEM_CHECK 
 !
 ! Memory Overview
 !=================
 !
 if (MEM) then
   !
   call section('*','Memory Overview')
   !
   call MEM_report("final")
   !
 endif
 !
#endif
 !
 ! GAME OVER :(
 !==============
 call section('X','Game Over & Game summary')
 !
 if (INIT_stop) then
   !
   ! Update INIT defs
   !==================
   !
   ! ... temporary allocations
   !
   call E_reset(en)
   call bz_samp_reset(q)
   call bz_samp_reset(k)
   do i_c=1,4
     call W_reset(Xw(i_c))
     call X_reset(X(i_c),type=i_c)
   enddo
   !
   ierr=INIT(en,q,k,X,Xw,Dip," ",1,.false.,.true.)
   !
 endif
 !
 ! Time Finalize
 !===============
 !
 call timing_deallocate( )
 !
end subroutine

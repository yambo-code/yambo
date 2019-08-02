!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine CLOSE_the_run(MEM,TIME,INIT_stop)
 !
 use timing_m,       ONLY:timing_overview,timing_deallocate,global_list
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
   call timing_overview(LIST=global_list,MAIN_NAME="yambo")
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

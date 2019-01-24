!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine K_observables(WHAT,W)
 !
 use pars,          ONLY:cZERO
 use X_m,           ONLY:X_alloc,Epsilon_ij,Epsilon_ii,Joint_DOS
 use frequency,     ONLY:w_samp
#if defined _PL
 use PHOTOLUM,      ONLY:PL,PL_init
#endif
 use BS_solvers,    ONLY:BSS_n_freqs
 !
#include<memory.h>
 !
 character(*) :: WHAT
 type(w_samp) :: W
 !
 if (WHAT=="ALLOC") then
   !
#if defined _KERR
   YAMBO_ALLOC(Epsilon_ij,(BSS_n_freqs,4))
   Epsilon_ij=cZERO
   Epsilon_ij(:,1)=W%p(:)
#endif
#if defined _PL
   call PL_init( )
   PL(:,1) = W%p(:)  
#endif
   !
   call X_alloc("EPS",(/BSS_n_freqs,4/))
   Epsilon_ii(:,1)   =W%p(:)
   Joint_DOS(:,1)    =W%p(:)
   !
 else if (WHAT=="FREE") then
   !
#if defined _KERR
   YAMBO_FREE(Epsilon_ij)
#endif
   !
#if defined _PL
   YAMBO_FREE(PL)
#endif
   !
   call X_alloc("EPS")
   !
 endif
 !
end subroutine

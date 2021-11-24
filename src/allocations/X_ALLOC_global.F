!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine X_ALLOC_global(WHAT,W)
 !
 use pars,          ONLY:cZERO
 use X_m,           ONLY:X_ALLOC_elemental,Epsilon_ii,Joint_DOS,Epsilon_ij,X_magnons,X_dichroism,X_obs,EEL
 use frequency,     ONLY:w_samp
 use drivers,       ONLY:l_chi,l_optics
 use BS_solvers,    ONLY:B_Hall
 use PHOTOLUM,      ONLY:PL,PL_init
 use BS,            ONLY:l_BS_anomalous_Hall,l_BS_esort
 !
#include<memory.h>
 !
 character(*)   :: WHAT
 type(w_samp)   :: W
 !
 if (WHAT=="ALLOC") then
   !
   if (X_obs(3)%active) then
     call X_ALLOC_elemental("EPS",(/W%n_freqs,4/))
     if (l_optics.and.l_chi) call X_ALLOC_elemental("ALPHA",(/W%n_freqs,2/))
     Epsilon_ii(:,1)=W%p(:)
   endif
   !
   if (X_obs(9)%active) then
     call X_ALLOC_elemental("OFF",(/W%n_freqs,4/))
     Epsilon_ij(:,1)=W%p(:)
   endif
   if(l_BS_anomalous_Hall) B_Hall=cZERO
   !
   if (X_obs(13)%active) then
     call X_ALLOC_elemental("MAGNONS",(/W%n_freqs,2,4/))
     X_magnons=cZERO
     X_magnons(:,1,1)=W%p(:)
     X_magnons(:,2,1)=W%p(:)
   endif
   !
   if (X_obs(15)%active) then
     call X_ALLOC_elemental("DICH",(/W%n_freqs,4/))
     X_dichroism(:,1)=W%p(:)
   endif
   !
   if (X_obs(2)%active) then
     call PL_init( )
     call X_ALLOC_elemental("PL",(/W%n_freqs,3/))
     PL(:,1) = W%p(:)  
   endif
   !
   if (X_obs(5)%active) then
     call X_ALLOC_elemental("EEL",(/W%n_freqs,2/))
   endif
   !
   if (X_obs(6)%active) then
     call X_ALLOC_elemental("ALPHA",(/W%n_freqs,2/))
   endif
   !
   if (X_obs(12)%active) then
     call X_ALLOC_elemental("JDOS",(/W%n_freqs,4/))
     Joint_DOS(:,1)=W%p(:)
   endif
   !
   if (X_obs(8)%active) then
     call X_ALLOC_elemental("BS_E_SORTED",(/1000,9,2/))
   endif
   !
   if (X_obs(1)%active) then
     X_obs(1)%N_columns=2
     X_obs(1)%COL_header(1)   = 'E/ev[1]'
     X_obs(1)%COL_header(2:3) = (/'Re(q^2Fxc)[2]','Re(q^2Fxc)[3]'/)
   endif
   !
 else if (WHAT=="FREE") then
   call X_ALLOC_elemental("FREE ALL")
 endif
 !
end subroutine

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
subroutine K_observables(WHAT,W)
 !
 use pars,          ONLY:rZERO,cZERO
 use X_m,           ONLY:X_alloc,Epsilon_ii,Joint_DOS,BS_E_sorted
 use frequency,     ONLY:w_samp
 use BS_solvers,    ONLY:BSS_n_freqs
 use BS,            ONLY:l_BS_abs,l_BS_jdos,l_BS_esort
 use BS_solvers,    ONLY:B_Hall
 use PHOTOLUM,      ONLY:PL,PL_init
 use BS,            ONLY:l_BS_kerr,l_BS_magnons,l_BS_anomalous_Hall,l_BS_dichroism,l_BS_photolum
 use X_m,           ONLY:Epsilon_ij,X_magnons,X_dichroism
 !
#include<memory.h>
 !
 character(*) :: WHAT
 type(w_samp) :: W
 !
 if (WHAT=="ALLOC") then
   !
   if(l_BS_kerr) then
     YAMBO_ALLOC(Epsilon_ij,(BSS_n_freqs,4))
     Epsilon_ij=cZERO
     Epsilon_ij(:,1)=W%p(:)
   endif
   if(l_BS_anomalous_Hall) B_Hall=cZERO
   !
   if(l_BS_magnons) then
     YAMBO_ALLOC(X_magnons,(BSS_n_freqs,2,4))
     X_magnons=cZERO
     X_magnons(:,1,1)=W%p(:)
     X_magnons(:,2,1)=W%p(:)
   endif
   !
   if(l_BS_dichroism) then
     YAMBO_ALLOC(X_dichroism,(BSS_n_freqs,4))
     X_dichroism=cZERO
     X_dichroism(:,1)=W%p(:)
   endif
   !
   if(l_BS_photolum) then
     call PL_init( )
     PL=rZERO
     PL(:,1) = W%p(:)  
   endif
   !
   if(l_BS_abs.or.l_BS_kerr) then
     call X_alloc("EPS",(/BSS_n_freqs,4/))
     Epsilon_ii(:,1)=W%p(:)
   endif
   !
   if(l_BS_jdos) then
     YAMBO_ALLOC(Joint_DOS,(BSS_n_freqs,4))
     Joint_DOS=cZERO
     Joint_DOS(:,1)=W%p(:)
   endif
   !
   if(l_BS_esort) then
     YAMBO_ALLOC(BS_E_sorted,(1000,9,2))
     BS_E_sorted=cZERO
   endif
   !
 else if (WHAT=="FREE") then
   !
   YAMBO_FREE(Epsilon_ij)
   YAMBO_FREE(X_magnons)
   YAMBO_FREE(X_dichroism)
   YAMBO_FREE(PL)
   !
   call X_alloc("EPS")
   !
   YAMBO_FREE(Joint_DOS)
   YAMBO_FREE(BS_E_sorted)
   !
 endif
 !
end subroutine

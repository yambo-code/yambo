!
!        Copyright (C) 2000-2017 the YAMBO team
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
function FREQUENCIES_damping(W,E)
 !
 use pars,      ONLY:SP
 use frequency, ONLY:w_samp
 implicit none
 !
 real(SP)      :: FREQUENCIES_damping,E,beta
 type(w_samp)  :: W
 ! 
 ! Work Space
 !
 real(SP):: e1,e2,d1,d2,E_
 !
 e1=W%er(1)
 e2=W%er(2)
 d1=W%dr(1)
 d2=W%dr(2)
 E_=E
 !
 if (W%er(1)<0._SP .and. W%er(2)<=0._SP) then
   e1=-W%er(2)
   e2=-W%er(1)
   E_=-E
 endif
 !
 if (W%er(1)<0._SP .and. W%er(2)>0._SP ) then
   !
   ! Cannot use Fermi damping is the E range is not everywhere positve
   !
   W%damp_reference=0._SP
   if (E>=0._SP) then
     e1=0._SP
     e2=W%er(2)
   else
     e1=0._SP
     e2=-W%er(1)
     E_=-E
   endif
 endif
 !
 ! To use Fermi we need that d2/2 > d1 
 !
 if (d2/2._SP<= d1) then
   W%damp_reference=0._SP
 endif
 !
 if (W%damp_reference==0._SP) then
   !
   !           /(E2,D2)
   !          /
   !         /
   !   (E1,D1) 
   !
   FREQUENCIES_damping=d1+(d2-d1)/(e2-e1)*(E_-e1)
 else
   beta=1._SP/(W%damp_reference-e1)*log( (d2-d1)/d1 )
   FREQUENCIES_damping=d2/(1._SP+exp(-beta*(E_-W%damp_reference)))
 endif
 !
end function

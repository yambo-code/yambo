!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,DS
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
function X_simple_GreenF(W,pole,ordering)
 !
 use pars, ONLY:SP
 !
 implicit none
 !
 complex(SP)  ::pole,W,X_simple_GreenF
 character(*) ::ordering
 !              
 logical      ::ord_t_ordered,ord_retarded_,ord_resonant_,ord_ares_tord,ord_ares_reta
 !
 ord_t_ordered= trim(ordering)=="t"      ! T-ordered
 ord_retarded_= trim(ordering)=="c"      ! Retarded   (in yambo it is called "c"=causal)
 ord_resonant_= trim(ordering)=="r"      ! Resonant      part of the T-ordered
 ord_ares_tord= trim(ordering)=="a"      ! Anti-resonant part of the T-ordered
 ord_ares_reta= trim(ordering)=="ca"     ! Anti-resonant part of the Retarded
 !
 if ( ord_t_ordered ) X_simple_GreenF=1._SP/(W-pole)-1._SP/(conjg(W)+      pole )
 if ( ord_retarded_ ) X_simple_GreenF=1._SP/(W-pole)-1._SP/(      W +conjg(pole))       
 if ( ord_resonant_ ) X_simple_GreenF=1._SP/(W-pole)
 if ( ord_ares_tord ) X_simple_GreenF=              -1._SP/(conjg(W)+      pole )
 if ( ord_ares_reta ) X_simple_GreenF=              -1._SP/(      W +conjg(pole))
 !
end function

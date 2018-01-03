!
!        Copyright (C) 2000-2018 the YAMBO team
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
function Lorentzian_FT(W,pole,ordering)
 !
 ! This subrutine returns the frequency dependent Green function
 ! which is the imaginary part of the
 ! Fourier transform of the Lorentzian function
 ! theta(t)*e^{-i*pole*t} [this is the retarded version, other ordering are computed]
 !
 ! AM, 1/8/16. Note that aimag(pole) is always negative while aimag(W)>0
 !
 use pars, ONLY:SP
 !
 implicit none
 !
 complex(SP)  ::pole,W,Lorentzian_FT
 character(*) ::ordering
 !              
 logical      ::ord_t_ordered,ord_retarded_,ord_resonant_,ord_ares_tord,ord_ares_reta
 !
 ord_t_ordered= trim(ordering)=="T"      ! T-ordered
 ord_retarded_= trim(ordering)=="R"      ! Retarded 
 ord_resonant_= trim(ordering)=="r"      ! Resonant      part of the T-ordered/Retarded
 ord_ares_tord= trim(ordering)=="Ta"     ! Anti-resonant part of the T-ordered
 ord_ares_reta= trim(ordering)=="Ra"     ! Anti-resonant part of the Retarded
 !
 !                                  RESONANT       ANTIRESONANT
 !
 if ( ord_t_ordered ) Lorentzian_FT=1._SP/(W-pole)-1._SP/(conjg(W)+      pole )
 if ( ord_retarded_ ) Lorentzian_FT=1._SP/(W-pole)-1._SP/(      W +conjg(pole))       
 if ( ord_resonant_ ) Lorentzian_FT=1._SP/(W-pole)
 if ( ord_ares_tord ) Lorentzian_FT=              -1._SP/(conjg(W)+      pole )
 if ( ord_ares_reta ) Lorentzian_FT=              -1._SP/(      W +conjg(pole))
 !
end function

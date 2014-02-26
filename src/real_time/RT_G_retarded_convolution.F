!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
real(SP) function RT_G_retarded_convolution(EmEp,Ebose,WmWp,Wbose,Egap,Skind)
 !
 use pars,       ONLY:SP
 use real_time,  ONLY:Gr_kind,two_alpha
 !
 implicit none
 !
 real(SP)     :: EmEp,WmWp,Egap
 real(SP)     :: Ebose,Wbose
 character(2) :: Skind
 !
 ! Work Space
 !
 integer            :: local_alpha
 real(SP)           :: E_center,W_center
 real(SP), external :: HyperbolicSecant_FT
 !
 RT_G_retarded_convolution=0._SP
 !
 E_center=EmEp+Ebose
 if (.not.Energy_Condition(EmEp,Ebose,E_center,Egap)) return
 !
 W_center=WmWp+Wbose
 !
 if (skind=='EP') local_alpha=  two_alpha  ! El-Ph, 2 GFs
 if (skind=='EE') local_alpha=2*two_alpha  ! El-El, 4 GFs
 !
 if (trim(Gr_kind)=='QP') RT_G_retarded_convolution=W_center/(E_center**2+W_center**2)
 !
 if (trim(Gr_kind)=='HS') RT_G_retarded_convolution=HyperbolicSecant_FT(E_center,W_center,local_alpha)
 !
 contains
   !
   logical function Energy_Condition(E1,E2,Etot,Egap_)
     real(SP) :: E1,E2,Etot,Egap_
     real(SP), parameter :: trash=1.E-7
     Energy_Condition=.true.
     if ( abs(E1)  <1.E-7    ) Energy_Condition=.false.      ! C1a
     if ( abs(E2)  <1.E-7    ) Energy_Condition=.false.      ! C1b
     if ( abs(Etot)<1.E-7    ) Energy_Condition=.false.      ! C2
     if ( abs(Etot)>2.*Egap_ ) Energy_Condition=.false.      ! C3
     if (    E1/E2 >0._SP    ) Energy_Condition=.false.      ! C4a+C4b
   end function
   !
end function RT_G_retarded_convolution

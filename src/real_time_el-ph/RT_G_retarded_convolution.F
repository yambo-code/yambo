!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
real(SP) function RT_G_retarded_convolution(EmEp,Ebose,WmWp,Wbose,Etresh,Skind)
 !
 use pars,       ONLY:SP
 use real_time,  ONLY:Gr_kind,two_alpha
 !
 implicit none
 !
 real(SP)     :: EmEp,WmWp,Etresh
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
 if (.not.Energy_Condition(EmEp,Ebose,E_center,Etresh)) return
 !
 W_center=WmWp+Wbose
 !
 select case(trim(Gr_kind))
   case('SQ')
     RT_G_retarded_convolution=Squared_function(E_center,W_center)
   case('QP')
     RT_G_retarded_convolution=W_center/(E_center**2+W_center**2)
   case('HS')
     if (Skind=='EP') local_alpha=  two_alpha  ! El-Ph, 2 GFs
     if (Skind=='EE') local_alpha=2*two_alpha  ! El-El, 4 GFs
     RT_G_retarded_convolution=HyperbolicSecant_FT(E_center,W_center,local_alpha)
 end select
 !
 contains
   !
   ! Davide:
   !
   logical function Energy_Condition(E1,E2,Etot,Etresh_)
     real(SP) :: E1,E2,Etot,Etresh_
     Energy_Condition=.true.
     if ( abs(Etot)>Etresh_ ) Energy_Condition=.false.
     if (  E1==0._SP        ) return
     if (  E2==0._SP        ) return
     if (  E1/E2   >0._SP   ) Energy_Condition=.false.
   end function
   !
   real(SP) function Squared_function(E_,W_)
     real(SP) :: E_,W_
     if(E_<=W_) Squared_function=1._SP/W_
     if(E_> W_) Squared_function=0._SP
   end function Squared_function
   !
end function RT_G_retarded_convolution

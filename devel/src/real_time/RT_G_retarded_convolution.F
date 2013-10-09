!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
real(SP) function RT_G_retarded_convolution(Approx,EmEp,Wph,W,Egap,is)
 !
 use pars,       ONLY:SP
 implicit none
 !
 character(*) :: Approx
 real(SP)     :: EmEp,Wph,W,Egap
 integer      :: is
 !
 ! Work Space
 !
 real(SP) :: E_center
 !
 RT_G_retarded_convolution=0.
 !
 E_center=EmEp+is*Wph
 !
 if (.not.Energy_Condition(EmEp,E_center)) return
 !
 RT_G_retarded_convolution=W/(E_center**2+W**2)
 !
 contains
   !
   logical function Energy_Condition(Ennp,EnnppmW)
     real(SP) :: Ennp,EnnppmW
     Energy_Condition=.TRUE.
     if (abs(Ennp)<1.E-10) Energy_Condition=.FALSE.
     if (abs(EnnppmW)>2.*Egap) Energy_Condition=.FALSE.
     if (is== 1.and.Ennp>1.E-10) Energy_Condition=.FALSE.
     if (is==-1.and.Ennp<1.E-10) Energy_Condition=.FALSE.
   end function
   !
end function RT_G_retarded_convolution

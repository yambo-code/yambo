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
function CIntegrate(F,W,N)
 !
 use pars,    ONLY:SP
 implicit none
 integer,     intent(in)  :: N
 real(SP),    intent(in)  :: W(N)
 complex(SP), intent(in)  :: F(N)
 complex(SP)              :: CIntegrate
 !
 ! Work Space
 !
 integer      :: i_w
 complex(SP)  :: M,Q
 !
 CIntegrate=(0._SP,0._SP)
 !
 do i_w=1,N-1
   M=(F(i_w)-F(i_w+1))/(W(i_w)-W(i_w+1))
   Q=F(i_w)-M*W(i_w)
   CIntegrate=CIntegrate+1./2.*M*(W(i_w+1)**2.-W(i_w)**2.)+Q*(W(i_w+1)-W(i_w))
 enddo
 !
end function

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
subroutine QPartilize(N,G,W,E,Z,dG_step)
 !
 use pars,    ONLY:SP
 implicit none
 integer,     intent(in)   :: N
 real(SP),    intent(inout):: dG_step
 real(SP),    intent(in)   :: W(N)
 complex(SP), intent(in)   :: G(N)
 complex(SP), intent(out)  :: E,Z
 !
 ! Work Space
 !
 integer          ::i_w,n_poles,dG_steps,ip_larger_R,ip
 integer,parameter::n_max_poles=100
 real(SP)         ::R(n_max_poles),Eqp(n_max_poles),Width(n_max_poles)
 complex(SP)      ::beta(n_max_poles),G_m1(N),A(N)
 !
 G_m1(:)=1./G(:)
 A(:)   =W(:)-G_m1(:)
 !
 if (dG_step>0.) dG_steps=int(dG_step/real(W(2)-W(1))) 
 !
 n_poles=0
 do i_w=1,N-1
   if (real(G_m1(i_w))*real(G_m1(i_w+1))<0.) then
     n_poles=n_poles+1
     Eqp(n_poles)   =real(W(i_w))
     Width(n_poles) =aimag(A(i_w))
     R(n_poles)     =abs(aimag(G(i_w)))
     beta(n_poles)=(A(min(i_w+dG_steps,N))-A(i_w))/real(W(min(i_w+dG_steps,N))-W(i_w))
   endif
   if (n_poles==n_max_poles) exit
 enddo
 ip_larger_R=1
 if (n_poles>1) then
   do ip=2,n_poles
     if (R(ip)>R(ip_larger_R)) ip_larger_R=ip
   enddo
 endif
 !
 Z=1./(1.-beta(ip_larger_R))
 E=Eqp(ip_larger_R)+Z*CMPLX(0.,Width(ip_larger_R))
 !
end subroutine

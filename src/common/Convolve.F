!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine Convolve(f,g,f_times_g,N,step,Eo)
 !
 ! f(x) = \sum_i f_i delta(x-i * step-Eo)
 ! g(x) = \sum_i g_i delta(x-i * step-Eo)
 !
 ! Eo=No*step
 !
 ! fxg(x) =  \sum_{k,j} f_(k-j-No) g_j delta(x-k * step-Eo)
 !
 use pars,    ONLY:SP
 implicit none
 integer,     intent(in)  :: N
 real(SP),    intent(in)  :: f(N),g(N),step,Eo
 complex(SP), intent(out) :: f_times_g(N)
 !
 ! Work Space
 !
 integer      :: K,J,L,Mo
 !
 Mo=nint(Eo/step)
 !
 f_times_G=(0.,0.)
 !
 do K=1,N
   do J=1,N
     L=K-J-Mo
     if (L<1.or.L>N) cycle
     f_times_g(K)=f_times_G(K)+f(L)*g(J)*step
   enddo
 enddo
 !
end subroutine

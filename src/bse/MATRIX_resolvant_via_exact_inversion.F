!
!        Copyright (C) 2000-2016 the YAMBO team
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
integer function MATRIX_resolvant_via_exact_inversion(n,m,n_loop,W,C,f,V,K)
 !
 ! C<W|1./(V^{-1}-M)|W>
 !
 use pars,           ONLY:SP,cZERO
 use wrapper,        ONLY:Vstar_dot_V,M_by_V
 !
 implicit none
 !
 integer,    intent(in) ::n,m,n_loop
 complex(SP),intent(in) ::C,W(n),V(n,m),K(n,n)
 complex(SP),intent(out)::f 
 !
 ! Work Space
 !
 complex(SP) ::wk(n,n),A_n(n)
 integer     ::i_L
 !
 MATRIX_resolvant_via_exact_inversion=0
 !
 do i_L=1,n_loop
   !
   if (i_L==1) call K_invertion_do_it_full(V(:,i_L),wk,"r")
   if (i_L==2) call K_invertion_do_it_full(V(:,i_L),wk,"a")
   !
   A_n=cZERO
   !
   if(i_L==1) then
     call M_by_V('n',n,wk,W,A_n)
     f=  C*Vstar_dot_V(n,W,A_n)
   else if(i_L==2) then
     call M_by_V('n',n,wk,conjg(W),A_n)
     f=f+C*Vstar_dot_V(n,conjg(W),A_n)
   endif
   !
 enddo
 !
end function

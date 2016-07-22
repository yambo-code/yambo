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
integer function MATRIX_resolvant_via_exact_inversion(n,m,W,C,f,V,K)
 !
 ! C<W|1./(V^{-1}-M)|W>
 !
 use pars,           ONLY:SP,cZERO
 use interfaces,     ONLY:MATRIX_driver
 use matrix_operate, ONLY:INV
 use wrapper,        ONLY:Vstar_dot_V,M_by_V
 !
 implicit none
 !
 integer,    intent(in) ::n,m
 complex(SP),intent(in) ::C,W(n),V(n,m),K(n,n)
 complex(SP),intent(out)::f 
 !
 ! Work Space
 !
 complex(SP) ::wk(n,n),A_n(n)
 integer     ::i1,i2,i3
 !
 MATRIX_resolvant_via_exact_inversion=0
 !
 do i3=1,m
   A_n=cZERO
   wk=cZERO
   do i2=1,n
     wk(i2,i2)=1._SP/V(i2,i3)
     do i1=1,n 
       if(i3==1) wk(i1,i2)=wk(i1,i2)-      K(i1,i2)
       if(i3==2) wk(i1,i2)=wk(i1,i2)-conjg(K(i1,i2))
     enddo
   enddo
   call MATRIX_driver(INV,wk)
   if(i3==1) then
     call M_by_V('n',n,wk,W,A_n)
     f=  C*Vstar_dot_V(n,W,A_n)
   else if(i3==2) then
     call M_by_V('n',n,wk,conjg(W),A_n)
     f=f+C*Vstar_dot_V(n,conjg(W),A_n)
   endif
 enddo
 !
end function

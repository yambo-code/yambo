!
!        Copyright (C) 2000-2015 the YAMBO team
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
integer function MATRIX_resolvant_via_exact_inversion(N,W,C,f,V,M)
 !
 ! C<W|1./(V^{-1}-M)|W>
 !
 use pars,           ONLY:SP
 use interfaces,     ONLY:MATRIX_driver
 use matrix_operate, ONLY:INV
 use wrapper,        ONLY:Vstar_dot_V,M_by_V
 !
 implicit none
 !
 integer      ::N
 complex(SP)  ::W(N),C,f
 complex(SP)  ::V(N),M(N,N)
 !
 ! Work Space
 !
 complex(SP), allocatable ::wk(:,:)
 integer     ::i1,i2
 complex(SP) ::A_n(N)
 !
 MATRIX_resolvant_via_exact_inversion=0
 !
 allocate(wk(N,N))
 wk=(0.,0.)
 !
 do i1=1,N 
   wk(i1,i1)=1./V(i1)
   do i2=1,N 
     wk(i1,i2)=wk(i1,i2)-M(i1,i2)
   enddo
 enddo
 call MATRIX_driver(INV,wk)
 call M_by_V('n',N,wk,W,A_n)
 f=C*Vstar_dot_V(N,W,A_n)
 !
 deallocate(wk)
 !
end function

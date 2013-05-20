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
integer function pert_inversion_new(N,W,C,f,V,use_full_inv)
 !
 ! C<W|1./(V^{-1}-M)|W> = \sum_n C<W| ( V M )^n V |W>
 !
 use pars,           ONLY:SP
 use matrix_operate, ONLY:mat_dia_inv,USE_LK,INV
 use wrapper,        ONLY:Vstar_dot_V,M_by_V
 use BS,             ONLY:K_BSmat_by_V
 use com,            ONLY:error
 !
 implicit none
 !
 integer      ::N
 complex(SP)  ::W(N),C,f
 complex(SP)  ::V(N)
 logical      ::use_full_inv
 !
 ! Work Space
 !
 integer, parameter       :: max_it=30
 real(SP),parameter       :: perc_treshold=0.01
 integer     ::it,i1,i2
 complex(SP) ::A_nm1(N),A_n(N),delta_f,fo
 !
 pert_inversion_new=0
 !
 A_nm1(:)=V(:)*W(:)
 fo=C*Vstar_dot_V(N,W,A_nm1)
 !
 if (abs(f)==0.) f=fo
 !
 it=0
 !
 if (.not.use_full_inv) then
   !
   delta_f=0.
   !
   do while (it<=max_it) 
     !
     call K_BSmat_by_V(1,1,A_nm1,A_n,inversion=.true.)
     A_n(:)=V(:)*A_n(:)
     delta_f=C*Vstar_dot_V(N,W,A_n)
     !
     f=f+delta_f
     !
     if (abs(real(delta_f))<perc_treshold*abs(real(fo)).and.&
&        abs(aimag(delta_f))<perc_treshold*abs(aimag(fo))) return
     !
     A_nm1(:)=A_n(:)
     !
     it=it+1
     !
   enddo
   !
   pert_inversion_new=1
   f=(0.,0.)
   !
 else
   !
   call error('Full inversion not yet implemented')
   !
 endif
 !
end function

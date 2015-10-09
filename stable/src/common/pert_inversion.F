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
integer function pert_inversion(iw,N,W,C,f,V)
 !
 ! C<W|1./(V^{-1}-M)|W> = \sum_n C<W| (1-VM)^{-1} V |W> = \sum_n C<W| ( VM )^n V |W>
 !
 use pars,           ONLY:SP
 use matrix_operate, ONLY:mat_dia_inv,USE_LK,INV
 use wrapper,        ONLY:Vstar_dot_V,M_by_V
 use BS,             ONLY:BSS_inv_iter_max
 use com,            ONLY:error
 !
 implicit none
 !
 integer      ::iw,N
 complex(SP)  ::W(N),C,f
 complex(SP)  ::V(N)
 !
 ! Work Space
 !
 real(SP),parameter       :: perc_treshold=0.01
 integer     ::it,i1,i2
 complex(SP) ::A_nm1(N),A_n(N),delta_f,fo
 !
 pert_inversion=0
 !
 A_nm1(:)=V(:)*W(:)
 fo=C*Vstar_dot_V(N,W,A_nm1)
 !
 if (abs(f)==0.) f=fo
 !
 it=1
 !
 delta_f=0.
 !
 do while (it<=BSS_inv_iter_max) 
   !
   call K_BSmat_by_V(1,it*iw,N,A_nm1,A_n,1,'Inv')
   A_n(:)=V(:)*A_n(:)
   delta_f=C*Vstar_dot_V(N,W,A_n)
   !
   f=f+delta_f
   !
   if (abs(real(delta_f))<perc_treshold*abs(real(fo)).and.&
&      abs(aimag(delta_f))<perc_treshold*abs(aimag(fo))) return
   !
   if( abs(real(delta_f))>40._SP .or. abs(aimag(delta_f))>40._SP ) exit
   !
   A_nm1(:)=A_n(:)
   !
   it=it+1
   !
 enddo
 !
 pert_inversion=1
 f=(0.,0.)
 !
end function

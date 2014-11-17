!
!        Copyright (C) 2000-2014 the YAMBO team
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
integer function MATRIX_resolvant_via_perturbative_inversion(N,W,C,f,V)
 !
 ! C<W|1./(V^{-1}-M)|W> = \sum_n C<W| (1-VM)^{-1} V |W> = \sum_n C<W| ( VM )^n V |W>
 !
 use pars,           ONLY:SP
 use wrapper,        ONLY:Vstar_dot_V,M_by_V
 use BS,             ONLY:BS_mat
 use parallel_m
#if defined _OPENMP
 use wrapper,        ONLY:Vstar_dot_V_omp,M_by_V_omp
#else
 use wrapper,        ONLY:Vstar_dot_V,M_by_V
#endif
 !
 implicit none
 !
 integer      ::N
 complex(SP)  ::W(N),C,f
 complex(SP)  ::V(N)
 !
 ! Work Space
 !
 real(SP),parameter       :: perc_treshold=0.01
 integer     ::BSS_inv_iter_max=51
 integer     ::it,i1
 complex(SP) ::A_nm1(N),A_n(N),delta_f,fo
 !
 MATRIX_resolvant_via_perturbative_inversion=0
 !
 A_nm1(:)=V(:)*W(:)
 !
#if defined _OPENMP
 fo=C*Vstar_dot_V_omp(N,W,A_nm1)
#else
 fo=C*Vstar_dot_V(N,W,A_nm1)
#endif
 !
 if (abs(f)==0.) f=fo
 !
 it=1
 !
 delta_f=0.
 !
 do while (it<=BSS_inv_iter_max) 
   !
#if defined _OPENMP
   call M_by_V_omp('n',N,BS_mat,A_nm1,A_n)
#else
   call M_by_V('n',N,BS_mat,A_nm1,A_n)
#endif
   !
   A_n(:)=V(:)*A_n(:)
#if defined _OPENMP
   delta_f=C*Vstar_dot_V_omp(N,W,A_n)
#else
   delta_f=C*Vstar_dot_V(N,W,A_n)
#endif
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
 MATRIX_resolvant_via_perturbative_inversion=1
 f=(0.,0.)
 !
end function

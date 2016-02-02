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
integer function MATRIX_resolvant_via_perturbative_inversion(n,m,W,C,f,V,K)
 !
 ! C<W|1./(V^{-1}-K)|W> = \sum_n C<W| (1-VK)^{-1} V |W> = \sum_n C<W| ( VK )^n V |W>
 !
 use BS,             ONLY:Inversion_treshold
 use pars,           ONLY:SP,cZERO,cONE
 use wrapper_omp,    ONLY:Vstar_dot_V_omp,M_by_V_omp
 !
 implicit none
 !
 integer,    intent(in)  ::n,m
 complex(SP),intent(in)  ::C,W(n),V(n,m),K(n,n)
 complex(SP),intent(out) ::f
 !
 ! Work Space
 !
 integer     ::BSS_inv_iter_max=51
 integer     ::it
 complex(SP) ::A_nm1(n,m),A_n(n,m),delta_f,fo
 !
 MATRIX_resolvant_via_perturbative_inversion=0
 !
 A_nm1(:,1)=V(:,1)*W(:)
 fo=C*Vstar_dot_V_omp(N,W,A_nm1(:,1))
 if(m==2) then
   A_nm1(:,2)=V(:,2)*conjg(W(:))
   fo=fo+C*Vstar_dot_V_omp(N,conjg(W),A_nm1(:,2))
 endif
 !
 it=1
 !
 f=fo
 delta_f=0._SP
 !
 if(Inversion_treshold<0) fo=cONE
 !
 do while (it<=BSS_inv_iter_max) 
   !
   call M_by_V_omp('n',N,K,A_nm1(:,1),A_n(:,1))
   if(m==2) call M_by_V_omp('n',N,conjg(K),A_nm1(:,2),A_n(:,2))
   !
   A_n(:,1)=V(:,1)*A_n(:,1)
   delta_f=C*Vstar_dot_V_omp(N,W,A_n(:,1))
   if(m==2) then 
     A_n(:,2)=V(:,2)*A_n(:,2)
     delta_f=delta_f+C*Vstar_dot_V_omp(N,conjg(W),A_n(:,2))
   endif
   !
   f=f+delta_f
   !
   if (abs(delta_f)/abs(fo)<abs(Inversion_treshold)) return
   !
   if( abs(delta_f)>40._SP) exit
   !
   A_nm1=A_n
   !
   it=it+1
   !
 enddo
 !
 MATRIX_resolvant_via_perturbative_inversion=1
 f=cZERO
 !
end function

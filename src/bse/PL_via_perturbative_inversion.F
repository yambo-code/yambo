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
integer function PL_via_perturbative_inversion(n,Lo_dim,n_loop,D,C,f,Lo)
 !
 use pars,           ONLY:SP,rZERO
 use wrapper_omp,    ONLY:Vstar_dot_V_omp,M_by_V_omp
 use BS_solvers,     ONLY:K_INV_PL,BS_mat,K_slk
 !
 implicit none
 !
 integer,    intent(in)  ::n,Lo_dim,n_loop
 real(SP),   intent(in)  ::C
 complex(SP),intent(in)  ::D(n,3),Lo(n,Lo_dim)
 real(SP)   ,intent(out) ::f
 !
 ! Work Space
 !
 integer     ::BSS_inv_iter_max=51
 integer     ::it,ic
 complex(SP) ::R_x_D_nm1(n,n_loop,3),R_x_D_n(n,n_loop,3),WK(n),CUMULATIVE(n,n_loop,3)
 real(SP)    ::f_previous,fo,delta_f
 !
 PL_via_perturbative_inversion=0
 !
 ! IP part
 !
 fo=rZERO
 !
 do ic=1,3
   R_x_D_nm1(:,1,ic)=conjg(Lo(:,1))*D(:,ic)
   WK=Lo(:,1+n_loop)*R_x_D_nm1(:,1,ic)
   fo=fo+C*Vstar_dot_V_omp(n,R_x_D_nm1(:,1,ic),WK)
   if(n_loop==2) then
     R_x_D_nm1(:,2,ic)=Lo(:,2)*conjg(D(:,ic))
     WK=Lo(:,2+n_loop)*R_x_D_nm1(:,2,ic)
     fo=fo+C*Vstar_dot_V_omp(n,R_x_D_nm1(:,2,ic),WK)
   endif
 enddo
 !
 ! Iterative Cycle 
 !
 it=1
 f_previous=fo
 CUMULATIVE=R_x_D_nm1
 !
 do while (it<=BSS_inv_iter_max) 
   !
   f=0._SP
   !
   do ic=1,3
     !
     if (K_slk%kind=="SLK") then
#if defined _SCALAPACK
       call PARALLEL_M_by_V('n',n,K_slk,R_x_D_nm1(:,1,ic),R_x_D_n(:,1,ic))
#endif
     else
       call M_by_V_omp('n',n,BS_mat,R_x_D_nm1(:,1,ic),R_x_D_n(:,1,ic))
     endif
     CUMULATIVE(:,1,ic)=CUMULATIVE(:,1,ic)+conjg(Lo(:,1))*R_x_D_n(:,1,ic)
     WK=Lo(:,1+n_loop)*CUMULATIVE(:,1,ic)
     f=f+C*Vstar_dot_V_omp(n,CUMULATIVE(:,1,ic),WK)
     !
     if (n_loop==2) then
       if (K_slk%kind=="SLK") then
#if defined _SCALAPACK
         call PARALLEL_M_by_V('n',n,K_slk,R_x_D_nm1(:,2,ic),R_x_D_n(:,2,ic))
#endif
       else
         call M_by_V_omp('n',n,BS_mat,R_x_D_nm1(:,2,ic),R_x_D_n(:,2,ic))
       endif
       CUMULATIVE(:,2,ic)=CUMULATIVE(:,2,ic)+Lo(:,2)*R_x_D_n(:,2,ic)
       WK=Lo(:,2+n_loop)*CUMULATIVE(:,2,ic)
       f=f+C*Vstar_dot_V_omp(n,CUMULATIVE(:,2,ic),WK)
       !
     endif
     !
   enddo
   !
   delta_f=f-f_previous
   !
   if (abs(delta_f)/abs(fo)<0.001) return
   !
   if (abs(delta_f)/abs(fo)>abs(K_INV_PL%treshold)) then
     PL_via_perturbative_inversion=1
     f=rZERO
     return
   endif
   !
   R_x_D_nm1=R_x_D_n
   f_previous=f
   !
   it=it+1
   !
 enddo
 !
end function

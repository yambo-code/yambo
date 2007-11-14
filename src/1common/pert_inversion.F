!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
integer function pert_inversion(N,W,C,f,V,M,use_full_inv)
 !
 ! C<W|1./(V^{-1}-M)|W> = \sum_n C<W| ( V M )^n V |W>
 !
 use pars,           ONLY:SP
 use matrix_operate, ONLY:mat_dia_inv,USE_LK,INV
 !
 implicit none
 integer      ::N
 complex(SP)  ::W(N),C,f
 complex(SP)  ::V(N),M(N,N)
 logical      ::use_full_inv
 !
 ! Work Space
 !
 integer, parameter       :: max_it=30
 real(SP),parameter       :: perc_treshold=0.01
 complex(SP), allocatable ::wk(:,:)
 integer     ::it,i1,i2
 complex(SP) ::A_nm1(N),A_n(N),delta_f,fo,V_diago_summed(N)
#if defined _DOUBLE
 complex(SP):: zdotc
#else
 complex(SP):: cdotc
#endif
 !
 pert_inversion=0
 !
 A_nm1(:)=V(:)*W(:)
#if defined _DOUBLE
 fo=C*zdotc(N,W,1,A_nm1,1)
#else
 fo=C*cdotc(N,W,1,A_nm1,1)
#endif
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
#if defined _DOUBLE
     call zgemv('n',N,N,(1._SP,0._SP),M,N,A_nm1,1,(0._SP,0._SP),A_n,1)
#else
     call cgemv('n',N,N,(1._SP,0._SP),M,N,A_nm1,1,(0._SP,0._SP),A_n,1)
#endif
     A_n(:)=V(:)*A_n(:)
#if defined _DOUBLE
     delta_f=C*zdotc(N,W,1,A_n,1)
#else
     delta_f=C*cdotc(N,W,1,A_n,1)
#endif
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
   pert_inversion=1
   f=(0.,0.)
   !
 else
   !
   allocate(wk(N,N))
   wk=(0.,0.)
   do i1=1,N 
     wk(i1,i1)=1./V(i1)
     do i2=1,N 
       wk(i1,i2)=wk(i1,i2)-M(i1,i2)
     enddo
   enddo
   call mat_dia_inv(INV,USE_LK,wk)
#if defined _DOUBLE
   call zgemv('n',N,N,(1._SP,0._SP),wk,N,W,1,(0._SP,0._SP),A_n,1)
#else
   call cgemv('n',N,N,(1._SP,0._SP),wk,N,W,1,(0._SP,0._SP),A_n,1)
#endif
#if defined _DOUBLE
   f=C*zdotc(N,W,1,A_n,1)
#else
   f=C*cdotc(N,W,1,A_n,1)
#endif
   deallocate(wk)
   !
 endif
 !
end function

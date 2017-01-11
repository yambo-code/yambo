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
subroutine PARALLEL_X_distribute( X , NW )
 !
 ! I distribute here the upper part of the X matrix.
 !
 use pars,          ONLY:SP
 use parallel_m,    ONLY:PAR_COM_RL_INDEX
 use X_m,           ONLY:X_t,Xo_rows,Xo_cols,X_mat,Xo_my_rows,Xo_my_cols,&
&                        UPPER_X_mat_only,Xo_lower_triangle
#include <memory.h>
 !
 type(X_t)    :: X
 integer      :: NW
 !
 ! Work Space
 !
 integer :: M,rows(PAR_COM_RL_INDEX%n_CPU,2),cols(PAR_COM_RL_INDEX%n_CPU,2),i_c,N_rows
 !
 if (PAR_COM_RL_INDEX%n_CPU==1) then
   rows(1,:)=(/1,X%ng/)
   cols(1,:)=(/1,X%ng/)
 else 
   M= ( (X%ng**2-X%ng)/2 + X%ng )/PAR_COM_RL_INDEX%n_CPU
   rows(1,:)=(/1,X%ng/)
   cols(1,:)=(/1,X_sol(X%ng)/)
   do i_c=2,PAR_COM_RL_INDEX%n_CPU
     rows(i_c,:)=(/cols(i_c-1,2)+1,X%ng/)
     N_rows=X_sol( rows(i_c,2)- rows(i_c,1)+1 )
     cols(i_c,:)=cols(i_c-1,2)+(/1,N_rows/)
     if (N_rows==0) cols(i_c,2)=X%ng
   enddo
 endif
 !do i_c=1,PAR_COM_RL_INDEX%n_CPU
 ! if (PAR_COM_RL_INDEX%CPU_id+1==i_c) then
 !   write (*,*) i_c,rows(i_c,:),cols(i_c,:)
 !   N_rows=(cols(i_c,2)-cols(i_c,1)+1)
 !   write (*,*) (rows(i_c,2)-rows(i_c,1)+1)*N_rows-(N_rows**2-N_rows)/2,M
 ! endif
 !enddo
 !
 Y_ALLOCATE(Xo_rows(PAR_COM_RL_INDEX%n_CPU,2))
 Y_MEM(Xo_rows)
 Y_ALLOCATE(Xo_cols(PAR_COM_RL_INDEX%n_CPU,2))
 Y_MEM(Xo_cols)
 !
 Xo_my_rows=rows(PAR_COM_RL_INDEX%CPU_id+1,:)
 Xo_my_cols=cols(PAR_COM_RL_INDEX%CPU_id+1,:)
 Xo_rows   =rows
 Xo_cols   =cols
 !
 Y_ALLOCATE(X_mat(Xo_my_rows(1):Xo_my_rows(2),Xo_my_cols(1):Xo_my_cols(2),NW))
 Y_MEM(X_mat)
 !
 if (UPPER_X_mat_only) return
 !
 if (PAR_COM_RL_INDEX%n_CPU==1) then
   !
   Xo_lower_triangle => X_mat
   !
 else
   !
   Y_ALLOCATE(Xo_lower_triangle(Xo_my_cols(1):Xo_my_cols(2),Xo_my_rows(1):Xo_my_rows(2),NW))
   Y_MEM(Xo_lower_triangle)
   !
 endif
 !
 contains
   !
   integer function X_sol(N)
     integer :: N
     X_sol=nint( (real(N)+.5_SP)-sqrt( (real(N)+0.5_SP)**2 - 2.*real(M) ) )+1
     if (  (real(N)+.5_SP)**2 - 2.*real(M) < 0 ) X_sol=0
   end function
end subroutine

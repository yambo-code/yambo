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
subroutine MATRIX_init( what, M_par, N )
 !
 ! I distribute here the upper part of the X matrix.
 !
 use pars,           ONLY:SP
 use linear_algebra, ONLY:PAR_matrix
 use parallel_m,     ONLY:COMM_copy,PAR_COM_SLK
#if defined _SCALAPACK
 use SLK_m,          ONLY:BLACS_structure_in_use
#endif
#include <memory.h>
 !
 character(*)           :: what
 type(PAR_matrix)       :: M_par
 integer                :: N
 !
 ! Work Space
 !
 integer       :: M,rows(M_par%INTER_comm%n_CPU,2),cols(M_par%INTER_comm%n_CPU,2),i_c,N_rows
 !
 if (what=="SLK") then
   call COMM_copy(PAR_COM_SLK, M_par%INTER_comm)
#if defined _SCALAPACK
   BLACS_structure_in_use=.FALSE.
#endif
   M_par%kind="SLK"
 endif
 !
 if (M_par%INTER_comm%n_CPU==1) then
   M_par%rows  = (/1,N/)
   M_par%cols  = (/1,N/)
   M_par%N     = N
   M_par%nrows = N
   M_par%ncols = N
   M_par%kind="SER"
   return
 endif
 !
 if (what=="SLK") then
   !
   ! SLK structure.
   !
#if defined _SCALAPACK
   call SLK_matrix_init(M_par,N)
   BLACS_structure_in_use=.TRUE.
#endif
 endif
 !
 if (what=="Xo") then
   !
   ! Response function. Only upper triangle is distributed.
   !
   M_par%kind="XUP"
   M= ( (N**2-N)/2 + N )/M_par%INTER_comm%n_CPU
   cols(1,:)=(/1,N/)
   rows(1,:)=(/1,UPPER_solver(N)/)
   do i_c=2,M_par%INTER_comm%n_CPU
     cols(i_c,:)=(/rows(i_c-1,2)+1,N/)
     N_rows=UPPER_solver( cols(i_c,2)- cols(i_c,1)+1 )
     rows(i_c,:)=rows(i_c-1,2)+(/1,N_rows/)
     if (N_rows==0) rows(i_c,2)=N
   enddo
   M_par%N     = N
   M_par%rows  = rows(M_par%INTER_comm%CPU_id+1,:)
   M_par%cols  = cols(M_par%INTER_comm%CPU_id+1,:)
   M_par%nrows = M_par%rows(2)-M_par%rows(1)+1 
   M_par%ncols = M_par%cols(2)-M_par%cols(1)+1 
   return
 endif
 !
 if (what=="PAR") then
   !
   ! Response function. Entire Matrix.
   !
   M_par%N     = N
   M_par%kind  ="PAR"
   M           = N/M_par%INTER_comm%n_CPU
   M_par%cols  = (/1,N/)
   M_par%rows  = (/M*M_par%INTER_comm%CPU_id+1,M*(M_par%INTER_comm%CPU_id+1)/)
   if (M_par%INTER_comm%CPU_id+1==M_par%INTER_comm%n_CPU) M_par%rows(2) = N
   M_par%nrows = M_par%rows(2)-M_par%rows(1)+1 
   M_par%ncols = M_par%cols(2)-M_par%cols(1)+1 
   return
 endif
 !
 contains
   !
   integer function UPPER_solver(N)
     integer :: N
     UPPER_solver=nint( (real(N)+.5_SP)-sqrt( (real(N)+0.5_SP)**2 - 2.*real(M) ) )+1
     if (  (real(N)+.5_SP)**2 - 2.*real(M) < 0 ) UPPER_solver=0
   end function
   !
end subroutine

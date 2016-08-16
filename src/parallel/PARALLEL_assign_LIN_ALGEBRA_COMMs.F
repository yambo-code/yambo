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
subroutine PARALLEL_assign_LIN_ALGEBRA_COMMs(ENVIRONMENT,idriver)
 !
 use parallel_m,    ONLY:PAR_COM_WORLD,PAR_COM_NULL,&
&                        PARALLEL_cpu_mat_inversion,PARALLEL_cpu_mat_diagonalization,&
&                        PAR_COM_Q_A2A,PAR_COM_Q_INDEX
 use SLK_m,         ONLY:SLK_COM_A2A,SLK_COM_INDEX
 use linear_algebra,ONLY:INV,DIAGO
 !
 implicit none
 !
 character(*)            :: ENVIRONMENT
 integer                 :: idriver
 !
 ! Work Space
 !
 integer :: N_cpu_lin_algebra
 !
 if (idriver==INV)   N_cpu_lin_algebra=PARALLEL_cpu_mat_inversion
 if (idriver==DIAGO) N_cpu_lin_algebra=PARALLEL_cpu_mat_diagonalization
 !
 if (N_cpu_lin_algebra==1) then         ! This means LK (serial)
   !
   call SLK_communicators_init( idriver, PAR_COM_NULL, PAR_COM_WORLD, 0)
   !
   return
   !
 endif
 !
#if defined _SCALAPACK
 !
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum".or.ENVIRONMENT=="Response_G_space") then
   !
   call SLK_communicators_init( idriver, PAR_COM_Q_A2A , PAR_COM_Q_INDEX, N_cpu_lin_algebra)
   !
 else if (ENVIRONMENT=="ScaLapacK") then
   !
   call SLK_communicators_init( idriver,SLK_COM_A2A(1),SLK_COM_INDEX(1),N_cpu_lin_algebra)
   !
 else
   !
   call SLK_communicators_init( idriver, PAR_COM_WORLD , PAR_COM_NULL, N_cpu_lin_algebra)
   !
 endif
 !
#endif
 !
end subroutine

!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine PARALLEL_global_ScaLapacK( )
 !
 use linear_algebra,ONLY:INV
 use parallel_m,    ONLY:CPU_structure
 use parallel_int,  ONLY:PARALLEL_assign_chains_and_COMMs
 use SLK_m,         ONLY:SLK_COM_INDEX,SLK_COM_A2A
 !
 implicit none
 !
 CALL PARALLEL_structure(2,(/"p","d"/))
 !
 call PARALLEL_assign_chains_and_COMMs(2,COMM_index_1=SLK_COM_INDEX(1),&
&                                        COMM_index_2=SLK_COM_INDEX(2),&
&                                        COMM_A2A_1=SLK_COM_A2A(1),&
&                                        COMM_A2A_2=SLK_COM_A2A(2))
 !
 ! Linear Algebra (all drivers)
 !
 call PARALLEL_assign_LA_COMMs("ScaLapacK",INV,CPU_structure(7)%nCPU_lin_algebra_INV)
 !
end subroutine PARALLEL_global_ScaLapacK

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
subroutine PARALLEL_assign_LIN_ALGEBRA_COMMs(ENVIRONMENT,WHAT,COMM_index,COMM_operation)
 !
 use parallel_m,    ONLY:MPI_comm,PARALLEL_n_structures_active,PARALLEL_CPU_role,&
&                        PARALLEL_cpu_mat_inversion,PARALLEL_cpu_mat_diagonalization,&
&                        INTER_CHAIN,INTRA_CHAIN,CHILD_CHAIN,ncpu,myid,mpi_comm_world,&
&                        COMM_copy,n_chains,PAR_COM_Q_A2A,PAR_COM_Q_INDEX,comm_default_value
 !
 implicit none
 !
 character(*)            :: ENVIRONMENT
 character(*)            :: WHAT
 type(MPI_comm)          :: COMM_index
 type(MPI_comm)          :: COMM_operation
 !
 ! Work Space
 !
 integer :: i_lin_algebra_chain,i_father_chain,N_cpu_lin_algebra
 !
 if (WHAT=="INV")   N_cpu_lin_algebra=PARALLEL_cpu_mat_inversion
 if (WHAT=="DIAGO") N_cpu_lin_algebra=PARALLEL_cpu_mat_diagonalization
 !
 i_father_chain=0
 i_lin_algebra_chain=0
 !
 i_lin_algebra_chain=log(real(ncpu)/real(N_cpu_lin_algebra))/log(2.)
 !
 if (i_lin_algebra_chain==n_chains) then ! This means LK (serial). N_cpu_lin_algebra=1
   !
   COMM_operation%COMM  =comm_default_value
   COMM_operation%CPU_id=0
   COMM_operation%n_CPU =1
   !
   COMM_index%COMM  =mpi_comm_world
   COMM_index%CPU_id=myid
   COMM_index%n_CPU =ncpu
   !
 else if (i_lin_algebra_chain==0) then ! This means SLK using all WORLD. N_cpu_lin_algebra=ncpu
   !
   COMM_operation%COMM  =mpi_comm_world
   COMM_operation%CPU_id=myid
   COMM_operation%n_CPU =ncpu
   !
   COMM_index%COMM  =comm_default_value
   COMM_index%CPU_id=0
   COMM_index%n_CPU =1
   !
 else
   !
   call COMM_copy(INTER_CHAIN(i_lin_algebra_chain),COMM_index)
   call COMM_copy(INTRA_CHAIN(i_lin_algebra_chain),COMM_operation)
   !
 endif
 !
 ! Up to today I could not still use SLK with subgroups of CPU. So I switch SLK off
 ! when Q's are distributed.
 !
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum".and.PAR_COM_Q_INDEX%n_CPU/=1) then
   !
   COMM_operation%COMM  =comm_default_value
   COMM_operation%CPU_id=0
   COMM_operation%n_CPU =1
   !
   call COMM_copy(PAR_COM_Q_A2A,COMM_index)
   !
 endif
 !
end subroutine

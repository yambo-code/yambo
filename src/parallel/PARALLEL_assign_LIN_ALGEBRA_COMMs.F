!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,AF
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
 use parallel_m,    ONLY:PAR_COM_WORLD,PAR_COM_NULL,MPI_comm,COMM_copy,&
&                        PAR_COM_INV,PAR_COM_DIAGO,PAR_COM_INV_INDEX,PAR_COM_DIAGO_INDEX,&
&                        PARALLEL_cpu_mat_inversion,PARALLEL_cpu_mat_diagonalization,&
&                        linear_algebra_is_parallel,PAR_COM_Q_A2A,PAR_COM_Q_INDEX
#if defined _SCALAPACK
 use SLK_m,         ONLY:SLK_COM_A2A,SLK_COM_INDEX,SLK_ORTHO_INV,SLK_POOL_INV,SLK_ORTHO_DIAGO,SLK_POOL_DIAGO
#endif
 use linear_algebra,ONLY:INV,DIAGO,MAT_MUL,LIN_SYS
 use com,           ONLY:warning
 !
 implicit none
 !
 character(*)            :: ENVIRONMENT
 integer                 :: idriver
 !
 ! Work Space
 !
 integer :: Ncpu_lin_algebra, slk_error
 !
 !  LK -> Lapack,     serial linear algebra
 ! SLK -> Scalapack, parallel linear algebra
 !
#if defined _SCALAPACK 
 if (idriver==INV.or.idriver==LIN_SYS.or.idriver==MAT_MUL) then
   Ncpu_lin_algebra=PAR_COM_INV%n_CPU
   if (PARALLEL_cpu_mat_inversion>0)  Ncpu_lin_algebra=PARALLEL_cpu_mat_inversion
   if (PARALLEL_cpu_mat_inversion==0) Ncpu_lin_algebra=1         ! safe default
 endif
 if (idriver==DIAGO) then
   Ncpu_lin_algebra=PAR_COM_DIAGO%n_CPU
   if (PARALLEL_cpu_mat_diagonalization>0)  Ncpu_lin_algebra=PARALLEL_cpu_mat_diagonalization
   if (PARALLEL_cpu_mat_diagonalization==0) Ncpu_lin_algebra=1   ! safe default
 endif
#else
 Ncpu_lin_algebra=1
#endif
 !
 ! smallest SLK grid is 2x2
 if (Ncpu_lin_algebra<=3) Ncpu_lin_algebra=1
 !
 ! Serial (LK) case
 if (Ncpu_lin_algebra==1) then  
   !
   call SLK_communicators_init( PAR_COM_NULL, PAR_COM_WORLD)
   !
   linear_algebra_is_parallel=.FALSE.
   return
   !
 endif
 !
#if defined _SCALAPACK
 !
 ! environment-dependent definitions
 !
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum".or.ENVIRONMENT=="Response_G_space") then
   !
   call SLK_communicators_init(  PAR_COM_Q_A2A , PAR_COM_Q_INDEX )
   !
 else if (ENVIRONMENT=="ScaLapacK") then
   !
   call SLK_communicators_init( SLK_COM_A2A(1),SLK_COM_INDEX(1))
   !
 else
   !
   call SLK_communicators_init( PAR_COM_WORLD , PAR_COM_NULL )
   !
 endif
 !
 ! redef of Ncpu and SLK init
 !
 linear_algebra_is_parallel=.TRUE.
 !
 if (idriver==INV.or.idriver==LIN_SYS.or.idriver==MAT_MUL) then
   Ncpu_lin_algebra=min(Ncpu_lin_algebra,PAR_COM_INV%n_CPU)
 endif
 if (idriver==DIAGO) then
   Ncpu_lin_algebra=min(Ncpu_lin_algebra,PAR_COM_DIAGO%n_CPU)
 endif
 ! 
 if (Ncpu_lin_algebra<=3) linear_algebra_is_parallel=.FALSE.
 if (.not.linear_algebra_is_parallel) return
 !
 if (idriver==INV.or.idriver==LIN_SYS.or.idriver==MAT_MUL) then
   call SLK_setup( idriver, Ncpu_lin_algebra, SLK_ORTHO_INV, SLK_POOL_INV, PAR_COM_INV, PAR_COM_INV_INDEX, slk_error)
 endif
 if (idriver==DIAGO) then
   call SLK_setup( idriver, Ncpu_lin_algebra, SLK_ORTHO_DIAGO, SLK_POOL_DIAGO, PAR_COM_DIAGO, PAR_COM_DIAGO_INDEX, slk_error)
 endif
 if (slk_error/=0) then
    linear_algebra_is_parallel=.FALSE.
    call warning("SLK setup failed. Cannot build a proper BLACS grid. Switching to standard LK")
 endif
 !
#endif
 !
 contains
   !
   subroutine SLK_communicators_init(COMM, INDEX_comm)
   !
   IMPLICIT NONE
   !
   type(MPI_comm),    intent(in)    :: COMM , INDEX_comm
   !
   if (idriver==INV) then
     call COMM_copy(INDEX_comm,PAR_COM_INV_INDEX)
     call COMM_copy(COMM,      PAR_COM_INV)
   else if (idriver==DIAGO) then
     call COMM_copy(INDEX_comm,PAR_COM_DIAGO_INDEX)
     call COMM_copy(COMM      ,PAR_COM_DIAGO)
   endif
   !
  end subroutine SLK_communicators_init
  !
end subroutine

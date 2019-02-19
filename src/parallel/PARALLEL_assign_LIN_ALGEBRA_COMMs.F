!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine PARALLEL_assign_LIN_ALGEBRA_COMMs(ENVIRONMENT,idriver,Ncpu_in)
 !
 use parallel_m,    ONLY:PAR_COM_WORLD,PAR_COM_NULL,MPI_comm,COMM_copy,&
&                        PAR_COM_SLK,PAR_COM_SLK_INDEX,PAR_COM_Q_A2A,PAR_COM_Q_INDEX,&
&                        linear_algebra_is_parallel
#if defined _SCALAPACK
 use SLK_m,         ONLY:SLK_COM_A2A,SLK_COM_INDEX,SLK_ORTHO,SLK_POOL
#endif
 use com,           ONLY:warning
 !
 implicit none
 !
 character(*)            :: ENVIRONMENT
 integer                 :: idriver
 integer                 :: Ncpu_in
 !
 ! Work Space
 !
 integer :: Ncpu_lin_algebra, slk_error
 !
 !  LK -> Lapack,     serial linear algebra
 ! SLK -> Scalapack, parallel linear algebra
 !
 Ncpu_lin_algebra=1
#if defined _SCALAPACK 
 if (Ncpu_in==0) Ncpu_lin_algebra=1         ! safe default
 if (Ncpu_in>0)  Ncpu_lin_algebra=Ncpu_in
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
   call SLK_communicators_init( PAR_COM_Q_A2A, PAR_COM_Q_INDEX ) 
   !
 else if (ENVIRONMENT=="ScaLapacK") then
   !
   call SLK_communicators_init( SLK_COM_A2A(1), SLK_COM_INDEX(1) )
   !
 else
   !
   call SLK_communicators_init( PAR_COM_WORLD, PAR_COM_NULL )
   !
 endif
 !
 ! redef of Ncpu and SLK init
 !
 linear_algebra_is_parallel=.TRUE.
 !
 Ncpu_lin_algebra=min(Ncpu_lin_algebra,PAR_COM_SLK%n_CPU)
 ! 
 if (Ncpu_lin_algebra<=3) linear_algebra_is_parallel=.FALSE.
 if (.not.linear_algebra_is_parallel) return
 !
 ! init SLK
 !
 call SLK_setup( idriver, Ncpu_lin_algebra, SLK_ORTHO, SLK_POOL, PAR_COM_SLK, PAR_COM_SLK_INDEX, slk_error)
 !
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
   implicit none
   !
   type(MPI_comm), intent(in) :: COMM, INDEX_comm
   !
   call COMM_copy(INDEX_comm,PAR_COM_SLK_INDEX)
   call COMM_copy(COMM,      PAR_COM_SLK)
   !
  end subroutine SLK_communicators_init
  !
end subroutine

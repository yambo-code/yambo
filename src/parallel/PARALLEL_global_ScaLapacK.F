!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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

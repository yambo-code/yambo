!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): NM DS
!
! Fill a Yambo SLK (block-block) matrix from BS_blk via MATRIX_transfer(M_bse).
! Block size is ceil(N/nprow) from SLK_matrix_init — the layout ChASE expects.
!
subroutine K_stored_in_a_blacs_matrix(M_blacs,ncpu_blacs,i_BS_mat,BS_mat_dim,iq)
 !
 use pars,           ONLY:cZERO,schlen
 use matrix,         ONLY:PAR_matrix
 use interfaces,     ONLY:MATRIX_transfer
 use SLK_m,          ONLY:SLK_ORTHO
 use timing_m,       ONLY:timing
 use parallel_m,     ONLY:ncpu
 use BS,             ONLY:BS_blk,BS_Blocks_symmetrize_K
 use BS_solvers,     ONLY:run_inversion,run_Haydock
 !
#include <y_memory.h>
 !
 integer,          intent(in)  :: ncpu_blacs,i_BS_mat,BS_mat_dim,iq
 type(PAR_matrix), intent(out) :: M_blacs
 !
 character(schlen) :: timing_msg
 integer :: rows(2),cols(2)
 !
 timing_msg='BSE kernel redistr'
 if(ncpu_blacs>1) timing_msg='Yambo to blacs redistr'
 call timing(trim(timing_msg),OPR='start')
 !
 call BS_Blocks_symmetrize_K()
 !
 call MATRIX_init( "SLK", M_blacs, BS_mat_dim, 1 )
 M_blacs%kind="SLK"
 if (SLK_ORTHO%coordinate(1)==-1) then
   rows=(/1,1/)
   cols=(/1,1/)
 else
   rows=M_blacs%BLCrows
   cols=M_blacs%BLCcols
 endif
 YAMBO_ALLOC(M_blacs%blc,(rows(1):rows(2),cols(1):cols(2),1))
 M_blacs%blc=cZERO
 !
 if (ncpu_blacs==ncpu) then
   call MATRIX_transfer(M_out=M_blacs, M_bse= BS_blk )
   if (SLK_ORTHO%coordinate(1)/=-1) call K_add_E_diagonal_slk(M_blacs)
 else if(ncpu_blacs==1) then
   call K_stored_in_a_big_matrix(i_BS_mat,BS_mat_dim,M_blacs%blc(:,:,1),iq,"DIAGO")
 else
   call error("Filling of blacs matrix not coded with ncpu_blacs/=ncpu")
 endif
 !
 if ( (.not.(run_Haydock.or.run_inversion)) ) call BS_blks_free(i_BS_mat)
 !
 call timing(trim(timing_msg),OPR='stop')
 !
end subroutine K_stored_in_a_blacs_matrix

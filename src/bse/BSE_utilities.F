!
! License-Identifier: GPL
!
! Copyright (C) 2023 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS
!
subroutine build_inverse_eh_table(nk)
 !
 use electrons,       ONLY:n_sp_pol
 use BS,              ONLY:BS_H_dim,BS_bands
 use BS_solvers,      ONLY:BSS_eh_table,BSS_eh_table_m1
 !
 implicit none
 !
 integer, intent(in) :: nk
 !
 integer :: iv,ic,i_H,ik_bz
 !
 if (.not.allocated(BSS_eh_table_m1)) then
   allocate(BSS_eh_table_m1(nk,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2),n_sp_pol,n_sp_pol))
   BSS_eh_table_m1=0
 endif 
 !call live_timing('BSE inverse eh-table',BS_H_dim)
 !
 do i_H=1,BS_H_dim
   !
   ik_bz = BSS_eh_table(i_H,1)
   iv    = BSS_eh_table(i_H,2)
   ic    = BSS_eh_table(i_H,3)
   !
   BSS_eh_table_m1(ik_bz,iv,ic,1,1)=i_H
   !call live_timing(steps=1)
   !
 enddo
 !call live_timing( )
 !
end subroutine build_inverse_eh_table

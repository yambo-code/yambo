!
! License-Identifier: GPL
!
! Copyright (C) 2026 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS AF
!
subroutine K_block_tables(i_block,i_Tgrp_k,i_Tgrp_p,H_pos_start,mode_now)
 !
 ! Table of the transitions and energies of the kernel block i_block.
 !
 ! They are filled at the elements on the diagonal of the whole matrix
 ! (H_pos(1)==H_pos(2)), where H_pos defines the position inside the full bse matrix
 ! (or inside the R/C block). Elements already computed in a restarted run
 ! are not touched.
 !
 ! Used by the batched evaluation of the kernel.
 !
 use BS,            ONLY:BS_blk,BS_T_grp,l_BSE_restart
 !
 implicit none
 !
 integer,      intent(in) :: i_block,i_Tgrp_k,i_Tgrp_p,H_pos_start(2)
 character(1), intent(in) :: mode_now
 !
 ! Work Space
 !
 integer :: i_Tk,i_Tp
 !
 ! H_pos_start(1)+i_Tk == H_pos_start(2)+i_Tp
 !
 do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
   !
   i_Tk=i_Tp+H_pos_start(2)-H_pos_start(1)
   if (i_Tk<1.or.i_Tk>BS_T_grp(i_Tgrp_k)%size) cycle
   !
   if (BS_blk(i_block)%done(i_Tk,i_Tp)=="t".and.l_BSE_restart) cycle
   !
   BS_blk(i_block)%table(:,i_Tk)=BS_T_grp(i_Tgrp_k)%table(i_Tk,:)
   BS_blk(i_block)%E(i_Tk)      =BS_T_grp(i_Tgrp_k)%E(i_Tk,1)
   if (mode_now=="C") then
     ! Swap c and v
     BS_blk(i_block)%table(2,i_Tk)=BS_T_grp(i_Tgrp_k)%table(i_Tk,3)
     BS_blk(i_block)%table(3,i_Tk)=BS_T_grp(i_Tgrp_k)%table(i_Tk,2)
   endif
   !
 enddo
 !
end subroutine K_block_tables

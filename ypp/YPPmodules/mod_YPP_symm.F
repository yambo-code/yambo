!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
module YPP_symm
 !
 implicit none
 !
 logical               :: l_wf_nc_k_red
 !
 integer               :: ng_vec_save,ng_vec_new
 integer               :: wf_ng_save,wf_ng_new,wf_ng_cut
 integer               :: wf_ncx_save,wf_ncx_new
 !
 integer,allocatable   :: wf_nc_k_save(:),wf_nc_k_reduced(:),wf_nc_k_new(:)
 integer,allocatable   :: wf_igk_save(:,:),wf_igk_reduced(:,:),wf_igk_new(:,:)
 !
end module YPP_symm

!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA DS
!
! headers
!
#include<dev_defs.h>
#include<y_memory.h>
!
!
subroutine WF_shift_kpoint(b_to_shift,nb_to_shift,ikbz,wf_shift,Xk,WF_k_in,WF_k_out)
 !
 use pars,          ONLY: SP,cZERO
 use electrons,     ONLY: n_spinor
 use R_lattice,     ONLY: k_map,bz_samp
 use R_lattice,     ONLY: DEV_VAR(G_m_G),G_m_G,minus_G
 use wave_func,     ONLY: wf_ng_1st_BZ,wf_ng_overlaps
 use devxlib,       ONLY: devxlib_memcpy_d2d, devxlib_memset_d
 use gpu_m,         ONLY: have_gpu
 use y_memory_alloc
 !
 implicit none
 !
 integer,       intent(in) :: wf_shift(3),ikbz,b_to_shift(2),nb_to_shift
 type(bz_samp), intent(in) :: Xk
 complex(SP), intent(in)  DEV_ATTR :: WF_k_in (wf_ng_1st_BZ, n_spinor,nb_to_shift)
 complex(SP), intent(out) DEV_ATTR :: WF_k_out(wf_ng_overlaps,n_spinor,nb_to_shift)
 !
 ! Work space
 !
 integer :: id,id_loc,ik,is
 integer :: ig,igp,i_b1,i_b2
 integer :: g0_idx(3,2),g0_idx_val
 integer :: i_spinor,i_b
 !
 ik = Xk%sstar(ikbz,1)
 is = Xk%sstar(ikbz,2)
 !
 i_b1= 1
 i_b2= b_to_shift(2)-b_to_shift(1)+1
 !
 if (i_b2 /= nb_to_shift) call error("[WF_shift_kpoint] Unexpected error")
 !
 if (all(WF_shift==0)) then
   call devxlib_memset_d(WF_k_out,cZERO)
   call devxlib_memcpy_d2d(WF_k_out,WF_k_in, range1=(/1,wf_ng_1st_BZ/))
   return
 endif
 !
 g0_idx_val=1
 do id=1,3
   if(WF_shift(id)==0) cycle
   ig=k_map%g0_idx(id,WF_shift(id))
   igp=minus_G(g0_idx_val)
   g0_idx_val=G_m_G(ig,igp)
 enddo 
 !
 ! dev2dev
 call devxlib_memset_d(WF_k_out,cZERO)

 !
 ! main loop
 !
     !
     !DEV_ACC_DEBUG data present(WF_k_in,G_m_G,WF_k_out)
     !DEV_ACC parallel loop collapse(3)
     !DEV_CUF kernel do(3) <<<*,*>>>
     !DEV_OMPGPU target map(present,alloc:WF_k_in,G_m_G,WF_k_out)
     !DEV_OMPGPU teams loop collapse(3)
     !DEV_OMP parallel do default(shared), private(i_b,i_spinor,ig,igp),collapse(3)
     !
     do i_b=i_b1,i_b2
       do i_spinor=1,n_spinor
         do ig=1,wf_ng_1st_BZ
           !
           if(WF_k_in(ig,1,i_b1)==cZERO) cycle
           !
           igp=DEV_VAR(G_m_G)(ig,g0_idx_val)
           WF_k_out(igp,i_spinor,i_b)=WF_k_in(ig,i_spinor,i_b)
           !
         enddo
       enddo
     enddo
     !
     !DEV_OMP end parallel do
     !DEV_OMPGPU end target
     !DEV_ACC_DEBUG end data
     !
 !
end subroutine WF_shift_kpoint


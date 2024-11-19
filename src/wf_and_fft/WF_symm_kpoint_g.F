!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA DS AF
!
#include<dev_defs.h>                                                    
!
subroutine WF_symm_kpoint_g(b_to_load,ik,i_sp_pol,isymm,ig0,WF)
 !
 use pars,          ONLY:SP,cZERO
 use electrons,     ONLY:n_spinor
 use wave_func,     ONLY:WAVEs,wf_nc_k,DEV_VAR(wf_igk)
 use wave_func,     ONLY:wf_ng,wf_ng_1st_BZ
 use vec_operate,   ONLY:c2a
 use R_lattice,     ONLY:DEV_VAR(g_rot),DEV_VAR(G_m_G),&
 &                       k_pt,rl_sop,g_vec
 use D_lattice,     ONLY:idt_index,DEV_VAR(spin_sop),sop_inv, &
 &                       dl_sop,i_time_rev,nsym
 !
 implicit none
 !
 integer,       intent(in) :: b_to_load(2)
 integer,       intent(in) :: ik,i_sp_pol,isymm,ig0
 type (WAVEs), target, intent(inout) :: WF
 !
 ! Work space
 !
 complex(SP)              DEV_ATTR :: WF_k_tmp(wf_ng_1st_BZ,n_spinor)
 complex(SP), pointer     DEV_ATTR :: WF_p(:,:)
 !
 real(SP)    :: krot(3),G_cc(3)
 !
 integer :: ib,i_wf,isymm_m1
 integer :: ic,ig,igp,igr,igs,i_spinor
 !
#if defined _GPU
 call error("subroutine WF_symm_kpoint_g is not gpu ported")
#endif
 !
 if(isymm==idt_index) return
 !
 isymm_m1=sop_inv(isymm)
 !  
 do ib=b_to_load(1),b_to_load(2)
   !
   WF_k_tmp=cZERO
   !
   i_wf=WF%index(ib,ik,i_sp_pol)
   WF_p => DEV_VAR(WF%c)(:,:,i_wf)
   !
   if (n_spinor==1) then
     !
     !DEV_ACC_DEBUG data present(WF_k_tmp,g_rot,WF_p)
     !DEV_ACC parallel loop private(ig,igr,igs)
     !DEV_CUF kernel do(1) <<<*,*>>>
     !DEV_OMPGPU target map(present,alloc:WF_k_tmp,g_rot,WF_p)
     !DEV_OMPGPU teams loop
     do ic=1,wf_nc_k(ik)
       ig=DEV_VAR(wf_igk)(ic,ik)
       igr=DEV_VAR(g_rot)(ig,isymm)
       igs=DEV_VAR(G_m_G)(igr,ig0)
       WF_k_tmp(igs,1)=WF_p(ig,1)
     enddo
     !
   else
     !
     !DEV_ACC_DEBUG data present(WF_k_tmp,g_rot,WF_p)
     !DEV_ACC parallel loop private(ig,igr,igs)
     !DEV_CUF kernel do(1) <<<*,*>>>
     !DEV_OMPGPU target map(present,alloc:WF_k_tmp,g_rot,WF_p)
     !DEV_OMPGPU teams loop
     do ic=1,wf_nc_k(ik)
       ig=DEV_VAR(wf_igk)(ic,ik)
       igr=DEV_VAR(g_rot)(ig,isymm)
       igs=DEV_VAR(G_m_G)(igr,ig0)
       WF_k_tmp(igs,1)=DEV_VAR(spin_sop)(1,1,isymm)*WF_p(ig,1)+ &
&                      DEV_VAR(spin_sop)(1,2,isymm)*WF_p(ig,2)
       WF_k_tmp(igs,2)=DEV_VAR(spin_sop)(2,1,isymm)*WF_p(ig,1)+ &
&                      DEV_VAR(spin_sop)(2,2,isymm)*WF_p(ig,2)
     enddo
   endif
   !
   ! take care of time reversal if needed
   !if(isymm>nsym/(1+i_time_rev)) call dev_conjg(WF_k_tmp)
   if(isymm>nsym/(1+i_time_rev)) WF_k_tmp=conjg(WF_k_tmp)
   !
   !call dev_memcpy(WF_p,WF_k_tmp)
   WF_p=WF_k_tmp
   !
   nullify(WF_p)
   !
 enddo
 !
end subroutine WF_symm_kpoint_g
!
!
subroutine WF_symm_kpoint_g_BZ(b_to_load,ik,i_sp_pol,isymm,ig0,isref,igref,WF)
  !
  use pars,          ONLY:SP,cZERO
  use electrons,     ONLY:n_spinor
  use wave_func,     ONLY:WAVEs,wf_nc_k,DEV_VAR(wf_igk)
  use wave_func,     ONLY:wf_ng,wf_ng_1st_BZ
  use vec_operate,   ONLY:c2a
  use R_lattice,     ONLY:DEV_VAR(g_rot),DEV_VAR(G_m_G),&
  &                       k_pt,rl_sop,g_vec
  use D_lattice,     ONLY:idt_index,DEV_VAR(spin_sop),sop_inv, &
  &                       dl_sop,i_time_rev,nsym
  !
  implicit none
  !
  integer,       intent(in) :: b_to_load(2)
  integer,       intent(in) :: ik,i_sp_pol,isymm,ig0,isref,igref
  type (WAVEs), target, intent(inout) :: WF
  !
  ! Work space
  !
  complex(SP)              DEV_ATTR :: WF_k_tmp(wf_ng_1st_BZ,n_spinor)
  complex(SP), pointer     DEV_ATTR :: WF_p(:,:)
  !
  real(SP)    :: krot(3),G_cc(3)
  !
  integer :: ib,i_wf,isymm_m1
  integer :: ic,ig,igp,igr,igs,i_spinor
  !
  if(isymm==idt_index) return
  !
#if defined _GPU
  call error("subroutine WF_symm_kpoint_g_BZ is not gpu ported")
#endif
  !
  isymm_m1=sop_inv(isymm)
  !  
  do ib=b_to_load(1),b_to_load(2)
    !
    WF_k_tmp=cZERO
    !
    i_wf=WF%index(ib,ik,i_sp_pol)
    WF_p => DEV_VAR(WF%c)(:,:,i_wf)
    !
    if (n_spinor==1) then
      !
      do ic=1,wf_nc_k(ik)
        ig=DEV_VAR(G_m_G)(DEV_VAR(g_rot)(DEV_VAR(wf_igk)(ic,ik),isref),igref)
        igr=DEV_VAR(g_rot)(ig,isymm)
        igs=DEV_VAR(G_m_G)(igr,ig0)
        WF_k_tmp(igs,1)=WF_p(ig,1)
      enddo
      !
    else
      !
      do ic=1,wf_nc_k(ik)
        ig=DEV_VAR(G_m_G)(DEV_VAR(g_rot)(DEV_VAR(wf_igk)(ic,ik),isref),igref)
        igr=DEV_VAR(g_rot)(ig,isymm)
        igs=DEV_VAR(G_m_G)(igr,ig0)
        WF_k_tmp(igs,1)=DEV_VAR(spin_sop)(1,1,isymm)*WF_p(ig,1)+ &
 &                      DEV_VAR(spin_sop)(1,2,isymm)*WF_p(ig,2)
        WF_k_tmp(igs,2)=DEV_VAR(spin_sop)(2,1,isymm)*WF_p(ig,1)+ &
 &                      DEV_VAR(spin_sop)(2,2,isymm)*WF_p(ig,2)
      enddo
    endif
    !
    ! take care of time reversal if needed
    !if(isymm>nsym/(1+i_time_rev)) call dev_conjg(WF_k_tmp)
    if(isymm>nsym/(1+i_time_rev)) WF_k_tmp=conjg(WF_k_tmp)
    !
    !call dev_memcpy(WF_p,WF_k_tmp)
    WF_p=WF_k_tmp
    !
    nullify(WF_p)
    !
  enddo
  !
end subroutine WF_symm_kpoint_g_BZ
!
!
subroutine WF_symm_kpoint_g_ssop(b_to_load,ik,i_sp_pol,isymm,ig0,ssop,WF)
  !
  use pars,          ONLY:SP,cZERO
  use electrons,     ONLY:n_spinor
  use wave_func,     ONLY:WAVEs,wf_nc_k,DEV_VAR(wf_igk)
  use wave_func,     ONLY:wf_ng,wf_ng_1st_BZ
  use vec_operate,   ONLY:c2a
  use R_lattice,     ONLY:DEV_VAR(g_rot),DEV_VAR(G_m_G),&
  &                       k_pt,rl_sop,g_vec
  use D_lattice,     ONLY:idt_index,DEV_VAR(spin_sop),sop_inv, &
  &                       dl_sop,i_time_rev,nsym
  !
  implicit none
  !
  integer,       intent(in) :: b_to_load(2)
  integer,       intent(in) :: ik,i_sp_pol,isymm,ig0
  complex(SP) DEV_ATTR, intent(in)    :: ssop(2,2)
  type (WAVEs), target, intent(inout) :: WF
  !
  ! Work space
  !
  complex(SP)              DEV_ATTR :: WF_k_tmp(wf_ng_1st_BZ,n_spinor)
  complex(SP), pointer     DEV_ATTR :: WF_p(:,:)
  !
  real(SP)    :: krot(3),G_cc(3)
  !
  integer :: ib,i_wf,isymm_m1
  integer :: ic,ig,igp,igr,igs,i_spinor
  !
  if(isymm==idt_index) return
  !
#if defined _GPU
  call error("subroutine WF_symm_kpoint_g_ssop is not gpu ported")
#endif
  !
  isymm_m1=sop_inv(isymm)
  !  
  do ib=b_to_load(1),b_to_load(2)
    !
    WF_k_tmp=cZERO
    !
    i_wf=WF%index(ib,ik,i_sp_pol)
    WF_p => DEV_VAR(WF%c)(:,:,i_wf)
    !
    if (n_spinor==1) then
      !
      do ic=1,wf_nc_k(ik)
        ig=DEV_VAR(wf_igk)(ic,ik)
        igr=DEV_VAR(g_rot)(ig,isymm)
        igs=DEV_VAR(G_m_G)(igr,ig0)
        WF_k_tmp(igs,1)=WF_p(ig,1)
      enddo
      !
    else
      !
      do ic=1,wf_nc_k(ik)
        ig=DEV_VAR(wf_igk)(ic,ik)
        igr=DEV_VAR(g_rot)(ig,isymm)
        igs=DEV_VAR(G_m_G)(igr,ig0)
        WF_k_tmp(igs,1)=ssop(1,1)*WF_p(ig,1)+ &
 &                      ssop(1,2)*WF_p(ig,2)
        WF_k_tmp(igs,2)=ssop(2,1)*WF_p(ig,1)+ &
 &                      ssop(2,2)*WF_p(ig,2)
      enddo
    endif
    !
    !
    ! take care of time reversal if needed
    !if(isymm>nsym/(1+i_time_rev)) call dev_conjg(WF_k_tmp)
    if(isymm>nsym/(1+i_time_rev)) WF_k_tmp=conjg(WF_k_tmp)
    !
    !call dev_memcpy(WF_p,WF_k_tmp)
    WF_p=WF_k_tmp
    !
    nullify(WF_p)
    !
  enddo
  !
 end subroutine WF_symm_kpoint_g_ssop

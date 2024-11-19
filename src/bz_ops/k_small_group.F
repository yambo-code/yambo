!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine k_small_group(k)
 !
 ! Outputs:
 ! 
 !   k%grp_table  k%grp_nsym
 !
 !Given the kpoints and the simmetry operations
 !this sub. computes which symmetries belong to the small group of k
 !
 use pars,           ONLY:SP
 use vec_operate,    ONLY:rlu_v_is_zero,c2a,k2bz
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:rl_sop,bz_samp
 use zeros,          ONLY:k_rlu_zero
 !
#include<y_memory.h>
 !
 type(bz_samp)::k
 !
 ! Work Space
 !
 integer         :: ik,is,istark,ikbz,isk
 real(SP)        :: kref(3),krot(3)
 !
 if (allocated(k%grp_table)) return
 !
 YAMBO_ALLOC(k%grp_nsym,(k%nbz))
 YAMBO_ALLOC(k%grp_table,(nsym,k%nbz))
 YAMBO_ALLOC(k%grp_table_m1,(nsym,k%nbz))
 !
 k%grp_nsym=0
 k%grp_table=-1
 k%grp_table_m1=-1
 do ik=1,k%nibz
   do istark=1,k%nstar(ik)
     isk =k%star(ik,istark)
     ikbz=k%k_table(ik,isk)
     do is=1,nsym
       kref=k%ptbz(ikbz,:)
       call c2a(v_in=kref,mode='ki2a')
       krot=matmul(rl_sop(:,:,is),k%ptbz(ikbz,:))
       call c2a(v_in=krot,mode='ki2a')
       if (.not.rlu_v_is_zero(krot-kref,zero_=k_rlu_zero)) cycle
       k%grp_nsym(ikbz)=k%grp_nsym(ikbz)+1
       k%grp_table(k%grp_nsym(ikbz),ikbz)=is
       k%grp_table_m1(is,ikbz)=k%grp_nsym(ikbz)
     enddo
   enddo
 enddo
 !
end subroutine

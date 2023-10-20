!
! License-Identifier: GPL
!
! Copyright (C) 2011 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
logical function k_map_fine_to_coarse_reduce(FineGd_k)
 !
 use zeros,               ONLY:k_rlu_zero
 use pars,                ONLY:SP
 use vec_operate,         ONLY:c2a,rlu_v_is_zero
 use R_lattice,           ONLY:bz_samp
 use LIVE_t,              ONLY:live_timing
 !
#include<memory.h>
 !
 type(bz_samp), intent(inout)      :: FineGd_k
 !
 ! Work Space
 !
 integer :: ik,ip,unique(FineGd_k%nbz)
 real(SP):: v(3)
 !
 unique=1
 !
 call live_timing('Map Fine Grid to Reduced Coarse one',FineGd_k%nbz)
 !
 do ik=1,FineGd_k%nbz
   call live_timing(steps=1)
   if (unique(ik)==0) cycle
   do ip=ik+1,FineGd_k%nbz
     if (unique(ip)==0) cycle
     v=FineGd_k%ptbz(ik,:)-FineGd_k%ptbz(ip,:)
     call c2a(v_in=v,mode='ki2a')
     if (rlu_v_is_zero(v,zero_=k_rlu_zero)) unique(ip)=0
   enddo
 enddo
 ! 
 call live_timing( )
 !
 k_map_fine_to_coarse_reduce=count(unique==1)==FineGd_k%nbz
 ! 
end function

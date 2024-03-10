!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine k_build_up_BZ_tables(k)
 !
 ! Output:
 !
 !  s_table such that
 !
 !  R k_ibz = S k_ibz with S=k%s_table(k,R)
 !  R k_ibz = k_bz    with k_bz=k%k_table(k,R)
 !
 use pars,           ONLY:SP
 use vec_operate,    ONLY:c2a,rlu_v_is_zero
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:rl_sop,bz_samp
 use zeros,          ONLY:k_rlu_zero
#include<memory.h>
 !
 type(bz_samp)::k
 !
 ! Work Space
 !
 integer      ::i1,i2,is1
 logical      ::s_table_is_ok,k_table_is_ok,g_table_is_ok
 real(SP)     ::v(3)
 !
 integer, external :: G_index
 !
 s_table_is_ok=.FALSE.
 k_table_is_ok=.FALSE.
 g_table_is_ok=.FALSE.
 if (allocated(k%s_table)) then
   if (size(k%s_table)==k%nibz*nsym) s_table_is_ok=.TRUE.
 endif
 if (allocated(k%k_table)) then
   if (size(k%k_table)==k%nibz*nsym) k_table_is_ok=.TRUE.
 endif
 if (allocated(k%g_table)) then
   if (size(k%g_table)==k%nibz*nsym) g_table_is_ok=.TRUE.
 endif
 if (.not.k_table_is_ok) then
   YAMBO_FREE(k%k_table)
   YAMBO_ALLOC(k%k_table,(k%nibz,nsym))
 endif
 if (.not.s_table_is_ok) then
   YAMBO_FREE(k%s_table)
   YAMBO_ALLOC(k%s_table,(k%nibz,nsym))
 endif
 if (.not.g_table_is_ok) then
   YAMBO_FREE(k%g_table)
   YAMBO_ALLOC(k%g_table,(k%nibz,nsym))
   k%g_table=1
 endif
 !
 call k_ibz2bz(k,'a',.false.)
 k%s_table=0
 k%k_table=0
 do i1=1,k%nibz
   do is1=1,nsym
     call c2a(v_in=matmul(rl_sop(:,:,is1),k%pt(i1,:)),v_out=v,mode='ki2a')
     do i2=1,k%nbz
       if ( .not.rlu_v_is_zero(v-k%ptbz(i2,:),zero_=k_rlu_zero) ) cycle
       k%s_table(i1,is1)=k%sstar(i2,2)
       k%k_table(i1,is1)=i2
       call c2a(v_in=(v-k%ptbz(i2,:)),v_out=v,mode='ka2i')
       k%g_table(i1,is1)=G_index(v)
       exit
     enddo
   enddo
 enddo
 !
 ! CLEAN
 !
 call k_ibz2bz(k,'d',.false.)
 !
end subroutine k_build_up_BZ_tables

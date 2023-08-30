!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine k_ibz2bz(k,units,FORCE_BZ)
 !
 ! Output
 !
 !  k%ptbz
 !
 use pars,           ONLY:SP,schlen
 use vec_operate,    ONLY:c2a,k2bz
 use R_lattice,      ONLY:rl_sop,bz_samp
 !
#include<memory.h>
 !
 type(bz_samp)::k
 character(1) ::units
 logical      ::FORCE_BZ
 !
 ! Work Space  
 !
 real(SP):: k_bz(3)
 integer :: i1
 character(schlen)::ch
 !
 write (ch,'(2a)') trim(k%description),"-ptbz"
 !
 ! Clean
 !
 if (units=="d") then
   YAMBO_FREE(k%ptbz)
   k%units=' '
   return
 endif
 !
 if (allocated(k%ptbz).and.k%units==units) return
 !
 ! First the stars...
 !
 if (.not.allocated(k%sstar)) call k_expand(k) 
 !
 ! ...then the pts
 !
 YAMBO_FREE(k%ptbz)
 YAMBO_ALLOC(k%ptbz,(k%nbz,3))
 !
 k%units=units
 !
 do i1=1,k%nbz
   !
   ! k_bz is in iku
   !
   k_bz=matmul(rl_sop(:,:,k%sstar(i1,2)),k%pt(k%sstar(i1,1),:)) 
   !
   ! Shift to the BZ
   !
   if (FORCE_BZ) call k2bz(k_bz)
   !
   if (units=='a') call c2a(v_in=k_bz,v_out=k%ptbz(i1,:),mode='ki2a')
   if (units=='i') k%ptbz(i1,:)=k_bz
   if (units=='c') call c2a(v_in=k_bz,v_out=k%ptbz(i1,:),mode='ki2c')
   !
 enddo
 !
end subroutine

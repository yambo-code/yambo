!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine k_expand(k)
 !
 ! Outputs:
 ! 
 !   k%star k%sstar k%nstar k%weights k%nbz
 !
 !Given the kpoints and the simmetry operations
 !this sub. calculates weights (and rangs of the point
 !groups) as well the simm. operations contained
 !in each star:
 !
 !k%star(ik,i) = is | R_is ik =!= ik  with i=1,k%nstar(ik)
 !
 !k%nstar(ik)   = # of k-points in the Star of ik
 !k%weights(ik) = k%nstar(ik) / (Sum_ik nk(ik) )  = Num. of symm / rank(ko) / Nk^BZ
 !
 !
 !2-dimension example:
 !
 !-------------------
 !|        |        |
 !|   X1   |   X2   |
 !|        |        |
 !-------------------
 !|        |        |
 !|   X3   |   ko   |
 !|        |        |
 !-------------------
 !
 !In this case we have 8 symmetries and we obtain
 !from the IRREDUCIBLE ko other 3 REDUCIBLE k-points i
 !(X1, X2, X3)
 !
 !rang(ko) = 2  (I plus inversion that sends X2 < - > X3)
 !k%nstar(ko) = Num. of symm / rank(ko) = 8/2 = 4 (points in the star)
 !
 ! N.B
 ! rang(ik) = # of symmetries | R_is ik = ik
 !
 use pars,           ONLY:SP
 use vec_operate,    ONLY:rlu_v_is_zero,c2a,k2bz
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:rl_sop,bz_samp
 use zeros,          ONLY:k_rlu_zero
#include<memory.h>
 !
 type(bz_samp)::k
 !
 ! Work Space
 !
 integer         :: i1,i2,is
 real(SP)        :: v(3),kstar(nsym,3)
 logical         :: k_found
 !
 if (allocated(k%sstar)) return
 !
 YAMBO_FREE(k%star)
 YAMBO_FREE(k%nstar)
 YAMBO_FREE(k%weights)
 !
 YAMBO_ALLOC(k%star,(k%nibz,nsym))
 YAMBO_ALLOC(k%nstar,(k%nibz))
 YAMBO_ALLOC(k%weights,(k%nibz))
 !
 k%nstar=0
 k%star=0
 do i1=1,k%nibz
   do is=1,nsym
     v=matmul(rl_sop(:,:,is),k%pt(i1,:))
     call k2bz(v_in=v)
     call c2a(v_in=v,v_out=kstar(is,:),mode='ki2a')
     k_found=.false.
     do i2=1,k%nstar(i1)
       if (rlu_v_is_zero(kstar(is,:)-kstar(k%star(i1,i2),:),zero_=k_rlu_zero)) k_found=.true.
       if (k_found) exit
     enddo
     if (.not.k_found) k%nstar(i1)=k%nstar(i1)+1
     if (.not.k_found) k%star(i1,k%nstar(i1))=is
   enddo
 enddo
 !
 k%weights(:)=real(k%nstar(:))/real(sum(k%nstar))
 k%weights(:)=k%weights(:)/sum(k%weights)
 k%nbz=sum(k%nstar(:))
 !
 YAMBO_ALLOC(k%sstar,(k%nbz,2))
 i2=0
 do i1=1,k%nibz
   do is=1,k%nstar(i1)
     i2=i2+1
     k%sstar(i2,:)=(/i1,k%star(i1,is)/)
   enddo
 enddo
 !
end subroutine

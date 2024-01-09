!
! License-Identifier: GPL
!
! Copyright (C) 2012 The Yambo Team
!
! Authors (see AUTHORS file for details): AM FP
!
subroutine ELPH_databases_IO_grids_check(k)
 !
 ! ELPH <-> YAMBO k table correspondance
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use vec_operate,    ONLY:v_is_zero,rlu_v_is_zero,c2a
 use zeros,          ONLY:k_rlu_zero
 use YPP_ELPH,       ONLY:DB_K_map,DB_ph_kpt,YAMBO_K_found,DB_nk
 !
 implicit none
 !
 type(bz_samp) :: k
 !
 ! Work Space
 !
 integer  :: ik,ik_elph
 real(SP) :: v(3)
 !
 DB_K_map=0
 YAMBO_K_found=.FALSE.
 do ik_elph=1,DB_nk
   !
   ! For non zero phonon q GS grid is composed of (k_1,k_1+q,k_2,k_2+q ...).
   ! I table the  k1,k2 ...
   !
   do ik=1,k%nbz
     if (YAMBO_K_found(ik)) cycle
     v=k%ptbz(ik,:)-DB_ph_kpt(:,ik_elph)
     call c2a(v_in=v,mode="ki2a")
     if (rlu_v_is_zero(v,zero_=k_rlu_zero)) then
       DB_K_map(ik_elph)=ik
       YAMBO_K_found(ik)=.TRUE.
       exit
     endif
   enddo
   !
   if (DB_K_map(ik_elph)==0) call error("Wrong K-grid correspondance")
   !
 enddo
 !
end subroutine

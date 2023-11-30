!
! License-Identifier: GPL
!
! Copyright (C) 2012 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine ELPH_databases_grids_map(k,q)
 !
 use drivers,        ONLY:l_phel_corr
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use ELPH,           ONLY:PH_qpt,PH_kpt_bz,elph_nQ,elph_nk_bz,K_io_map,Q_io_map,&
&                         l_GKKP_expanded_DB_exists,l_GKKP_DB_exists
 use vec_operate,    ONLY:v_is_zero,rlu_v_is_zero,c2a
 use zeros,          ONLY:k_rlu_zero
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 !
 ! Work Space
 !
 integer       ::i1,i2,nQ
 real(SP)      ::v(3)
 !
 if (l_GKKP_expanded_DB_exists) then
   if (elph_nQ   /=q%nbz) call error('Number of q-points does not match')
   if (elph_nk_bz/=k%nbz) call error('Number of k-points does not match')
   nQ=q%nbz
 else if (l_GKKP_DB_exists) then
   nQ=q%nibz
 endif
 !
 if (.not.allocated(Q_io_map)) then
   YAMBO_ALLOC(Q_io_map,(nQ))
   YAMBO_ALLOC(K_io_map,(k%nbz))
   Q_io_map=0
   K_io_map=0
 endif
 !
 if (l_GKKP_DB_exists.and.l_phel_corr) then
   do i1=1,elph_nQ
     Q_io_map(i1)=i1
   enddo
   do i1=1,k%nbz
     K_io_map(i1)=i1
   enddo
   return
 endif
 !
 ! Q-points map (BUILT-IN -> PH)
 !
 call k_ibz2bz(q,'i',.false.)
 do i1=1,nQ
   do i2=1,elph_nQ
     ! For abinit I may need a -1 ?!
     !call c2a(v_in=q%ptbz(i1,:)+PH_qpt(i2,:),v_out=v,mode="ki2a")
     ! For pwscf no need for -1 (see comment below)
     call c2a(v_in=q%ptbz(i1,:)-PH_qpt(i2,:),v_out=v,mode="ki2a")
     ! DS comment 2022/04/23:
     ! - these if are useless, if l_GKKP_DB_exists the code exits before (see return)
     ! - why the -q here? This is already fixed by DB_Q_map. The Q_io_map should use +q. See again standard case above.
     !if (l_GKKP_expanded_DB_exists) call c2a(v_in=q%ptbz(i1,:)+PH_qpt(i2,:),v_out=v,mode="ki2a")
     !if (l_GKKP_DB_exists)          call c2a(v_in=q%pt(i1,:)+PH_qpt(i2,:),v_out=v,mode="ki2a")
     if (rlu_v_is_zero(v,zero_=k_rlu_zero)) Q_io_map(i1)=i2
   enddo
 enddo
 call k_ibz2bz(q,'d',.false.)
 if (any(Q_io_map==0)) call error('Q-points do not match')
 !
 ! K-points map
 !
 K_io_map=0
 call k_ibz2bz(k,'i',.false.)
 do i1=1,k%nbz
   do i2=1,k%nbz
     call c2a(v_in=k%ptbz(i1,:)-PH_kpt_bz(i2,:),v_out=v,mode="ki2a")
     if (rlu_v_is_zero(v,zero_=k_rlu_zero)) K_io_map(i1)=i2
   enddo
 enddo
 call k_ibz2bz(k,'d',.false.)
 if (any(K_io_map==0)) call error('K-points do not match')
 !
end subroutine

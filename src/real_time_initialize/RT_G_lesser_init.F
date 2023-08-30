!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_G_lesser_init( )
 !
 use pars,           ONLY:SP,rZERO
 use RT_occupations, ONLY:RT_el_occ
 use parallel_m,     ONLY:PAR_IND_Xk_ibz
 use real_time,      ONLY:dG_lesser,G_lesser_reference,RTibz,RT_bands,G_lesser
 use R_lattice,      ONLY:nkibz
 !
 implicit none
 !
 ! Work Space
 !
 integer :: ik,ik_RT,ib
 !
 do ib=RT_bands(1),RT_bands(2)
   do ik=1,nkibz
     do ik_RT=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
       !
       G_lesser_reference(ib,ib,ik_RT) = cmplx(rZERO,RT_el_occ%N_ref(ib,ik_RT),SP)
       !
       if (.not.PAR_IND_Xk_ibz%element_1D(ik) ) cycle
       !
       G_lesser(ib,ib,ik_RT,1)  = cmplx(rZERO,RT_el_occ%N(ib,ik_RT),SP)
       dG_lesser(ib,ib,ik_RT,1) = cmplx(rZERO,RT_el_occ%N(ib,ik_RT)-RT_el_occ%N_ref(ib,ik_RT),SP)
       !
     enddo
   enddo
 enddo
 !
end subroutine RT_G_lesser_init

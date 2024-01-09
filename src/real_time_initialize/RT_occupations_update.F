!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_occupations_update( )
 !
 use parallel_m,     ONLY:PAR_COM_Xk_ibz_INDEX,PAR_IND_Xk_ibz
 use parallel_int,   ONLY:PP_redux_wait
 use real_time,      ONLY:dG_lesser,G_lesser_reference,RT_bands,RT_is_dynamically_dephased,RTibz
 use RT_occupations, ONLY:RT_el_occ,RT_ho_occ,RT_life_occ,RT_occupation_clean
 use electrons,      ONLY:spin_occ
 use R_lattice,      ONLY:nkibz
 !
 implicit none
 !
 ! Work Space
 !
 integer :: ib,ik,ik_RT
 !
 ! El/Ho Occupations are defined from G_lesser and G_lesser_reference
 !===================
 call RT_occupation_clean(RT_el_occ)
 call RT_occupation_clean(RT_ho_occ)
 call RT_occupation_clean(RT_life_occ)
 !
 do ib=RT_bands(1),RT_bands(2)
   do ik=1,nkibz
     do ik_RT=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
       !
       RT_el_occ%N_ref(ib,ik_RT) =          aimag(G_lesser_reference(ib,ib,ik_RT))
       RT_ho_occ%N_ref(ib,ik_RT) = spin_occ-aimag(G_lesser_reference(ib,ib,ik_RT))
       !
       if (.not.PAR_IND_Xk_ibz%element_1D(ik) ) cycle
       !
       RT_el_occ%dN(ib,ik_RT)= aimag(dG_lesser(ib,ib,ik_RT,1))
       !
       RT_el_occ%N(ib,ik_RT) = RT_el_occ%N_ref(ib,ik_RT)+RT_el_occ%dN(ib,ik_RT)
       RT_ho_occ%N(ib,ik_RT) = RT_ho_occ%N_ref(ib,ik_RT)-RT_el_occ%dN(ib,ik_RT)
       !
       if (.not.RT_is_dynamically_dephased) cycle
       !
       RT_life_occ%dN(ib,ik_RT) = RT_el_occ%dN(ib,ik_RT)
       !
     enddo
   enddo
 enddo
 !
 call PP_redux_wait(RT_el_occ%N,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_ho_occ%N,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_el_occ%dN,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 if (.not.RT_is_dynamically_dephased) return
 !
 call PP_redux_wait(RT_life_occ%dN,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
end subroutine RT_occupations_update

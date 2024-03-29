!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_occupations_eval(dG)
 !
 use pars,           ONLY:SP
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PAR_G_k_range
 use parallel_int,   ONLY:PP_redux_wait
 use RT_occupations, ONLY:RT_el_occ,RT_ho_occ
 use real_time,      ONLY:RTibz,RT_bands,RT_nk,RT_nbands,G_lesser_reference,l_RT_rotate_DM
 use hamiltonian,    ONLY:H_rotation
 use electrons,      ONLY:spin_occ
 !
 implicit none
 !
 complex(SP), intent(in) :: dG(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 !
 ! Work Space
 !
 integer                 :: ik,ib
 real(SP)                :: df_H
 complex(SP)             :: G_rot(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2)),&
&                           G_ref(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 logical, external       :: RT_do_it
 !
 ! Update the occupations:
 !  RT_delta_occupations are always sensitive to small changes 
 !  for Gref~spin_occ RT_H_occ is also sensitive
 !  for Gref~0        RT_E_occ is also sensitive
 !
 RT_el_occ%N=0._SP
 RT_ho_occ%N=0._SP
 RT_el_occ%dN=0._SP
 !
 do ik=1,RT_nk
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
   !
   if (l_RT_rotate_DM.and.(RT_do_it('OUT').or.RT_do_it('CARR'))) then
     G_rot=G_lesser_reference(:,:,ik)+dG(:,:,ik)
     call OBS_rotate(H_rotation(:,:,ik,1),G_rot,RT_nbands,1)
     G_ref=G_lesser_reference(:,:,ik)
     call OBS_rotate(H_rotation(:,:,ik,1),G_ref,RT_nbands,1)
   endif
   !
   if (l_RT_rotate_DM.and.(RT_do_it('OUT').or.RT_do_it('CARR'))) then
     do ib=RT_bands(1),RT_bands(2)
       RT_el_occ%N(ib,ik)= aimag(G_rot(ib,ib))
       RT_ho_occ%N(ib,ik)=-aimag(G_rot(ib,ib))+spin_occ
       ! This would be the correct definition
       ! RT_el_occ%dN(ib,ik)=aimag(G_rot(ib,ib)-G_ref(ib,ib))
       ! but I use this definition to consistently reconstruct f from df in ypp
       RT_el_occ%dN(ib,ik)=aimag(G_rot(ib,ib)-G_lesser_reference(ib,ib,ik))
     enddo
   else
     do ib=RT_bands(1),RT_bands(2)
       !
       RT_el_occ%N(ib,ik)= aimag(dG(ib,ib,ik))+aimag(G_lesser_reference(ib,ib,ik))
       !
       ! AM, July 2015. Intel compilers nest in ordered way the mathematical parenthesis. By using 
       ! df_H this problem is solved.
       !
       df_H                   =spin_occ-aimag(G_lesser_reference(ib,ib,ik))
       RT_ho_occ%N(ib,ik)=-aimag(dG(ib,ib,ik))+ df_H
       !
       RT_el_occ%dN(ib,ik)=aimag(dG(ib,ib,ik))
     enddo
   endif
   ! 
 enddo
 !
 call PP_redux_wait(RT_el_occ%N,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_ho_occ%N,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_el_occ%dN,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
end subroutine RT_occupations_eval

!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
!> @brief Construct time-dependent valence bands V(i_G,i_v,i_k,i_spin)
!     
subroutine NL_build_valence_bands(en,V_input,VAL_BANDS,lowest_band) 
 !
 use pars,        ONLY:SP,cZERO
 use electrons,   ONLY:levels,n_sp_pol,n_spinor
 use nl_optics,   ONLY:NL_bands
 use FFT_m,       ONLY:fft_size
 use QP_m,        ONLY:QP_nk
 use wave_func,   ONLY:WF
 use parallel_m,  ONLY:PAR_IND_Xk_ibz,PAR_Xk_ibz_index,PAR_Xk_nibz
 !
 implicit none
 !
 type(levels), intent(in)  :: en
 integer,      intent(in)  :: lowest_band
 complex(SP),  intent(in)  :: V_input(NL_bands(2),maxval(en%nbf),QP_nk,n_sp_pol)
 complex(SP),  intent(out) :: VAL_BANDS(fft_size,maxval(en%nbf),PAR_Xk_nibz,n_sp_pol)
 !
 ! Work Space
 !
 integer :: ib,is,ik,i_wf,i_v,ik_mem,i_spinor
 !
 Val_bands=cZERO
 !
 do ik=1,QP_nk
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   ik_mem=PAR_Xk_ibz_index(ik)
   !
   do is=1,n_sp_pol
     do ib=lowest_band,NL_bands(2)
       !
       i_wf=WF%index(ib,ik,is)
!$omp parallel do default(shared), private(i_v,i_spinor)
       do i_v=1,en%nbf(is)
         do i_spinor=1,n_spinor
           Val_bands(:,i_v,ik_mem,is)=Val_bands(:,i_v,ik_mem,is)+V_input(ib,i_v,ik,is)*WF%c(:,i_spinor,i_wf)
         enddo
       enddo
!$omp end parallel do
       !
     enddo
   enddo
   !
 enddo
 !
end subroutine NL_build_valence_bands

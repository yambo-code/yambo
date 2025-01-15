!
! License-Identifier: GPL
!
! Copyright (C) 2011 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine V_mean_potential(ik,i_sp_pol,WFo)
 !
 ! Here I implement Eq. 5 of PRL 80, 4265 (1998).
 !
 ! H_nl_sc(n,m) = <n_o| \Sigma |m_o>
 !
 ! SC_R(m,n) =  <m_o|n>
 !
 ! |n_o> is a bare (unpertirbed) state
 !
 use pars,           ONLY:SP,cZERO
 use wave_func,      ONLY:WAVEs
 use FFT_m,          ONLY:fft_size
 use hamiltonian,    ONLY:H_nl_sc,H_rotation
 use SC,             ONLY:SC_bands,V_mean
 use wrapper,        ONLY:M_by_M
 implicit none
 !
 integer     :: ik,i_sp_pol
 type(WAVEs) :: WFo
 !
 ! Work Space
 !
 integer    :: ib,ifft,ir,ibp
 complex(SP):: Sigma_times_R(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 !
 Sigma_Times_R=cZERO
 !
 ! Sigma_times_R(n,m) = \sum _I <n|\Sigma|I> <I|m>
 !                               |        |     |
 !                               bare     bare  SC
 !
 call M_by_M('n','n',SC_bands(2)-SC_bands(1)+1,H_nl_sc,H_rotation(:,:,ik,i_sp_pol),Sigma_times_R)
 !
 ! V_m(r) = \sum_n <r|n> Sigma_times_R(n,m) /<r|m> * |<r|m>|²
 !
 ! in the PRL they say that 
 !
 ! "As the potential is not defined by Eq. (5) at the nodes of
 !  the QP state, we take a weighted average of the resulting
 !  Vloc, according to |<r|QP>|² , over a few states in a
 !  small energy range."
 !
 ! V_m(r) = \sum_n <r|n> Sigma_times_R(n,m) /<r|m> * |<r|m>|²
 !
 do ib=SC_bands(1),SC_bands(2)
   !
   V_mean(:,ib)=cZERO
   !
   do ibp=SC_bands(1),SC_bands(2)
     !
     ifft=WFo%index(ibp,ik,1)
     !
     V_mean(:,ib)=V_mean(:,ib)+WFo%c(:,1,ifft)*Sigma_times_R(ibp,ib)
     !
   enddo
   !
   ifft=WFo%index(ib,ik,1)
   !
   do ir=1,fft_size
     V_mean(ir,ib)=V_mean(ir,ib)/WFo%c(ir,1,ifft)
   enddo
   !
 enddo
 !
end subroutine 

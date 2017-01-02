!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
!
! This file is distributed under the terms of the GNU
! General Public License. You can redistribute it and/or
! modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation;
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will
! be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE.  See the GNU General Public License
! for more details.
!
! You should have received a copy of the GNU General Public
! License along with this program; if not, write to the Free
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
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
 use hamiltonian,    ONLY:H_nl_sc
 use SC,             ONLY:SC_R,SC_bands,V_mean
 use wrapper,        ONLY:M_by_M
 implicit none
 !
 integer     :: ik,i_sp_pol
 type(WAVEs) :: WFo
 !
 ! Work Space
 !
 integer    :: ib,ifft,ir,ibp
 complex(SP):: Sigma_times_R(SC_bands(2),SC_bands(2))
 !
 Sigma_Times_R=cZERO
 !
 ! Sigma_times_R(n,m) = \sum _I <n|\Sigma|I> <I|m>
 !                               |        |     |
 !                               bare     bare  SC
 !
 call M_by_M('n','n',SC_bands(2),H_nl_sc,SC_R(:,:,ik,i_sp_pol),Sigma_times_R)
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
 do ib=1,SC_bands(2)
   !
   V_mean(:,ib)=cZERO
   !
   do ibp=1,SC_bands(2)
     !
     ifft=WFo%index(ibp,ik,1)
     !
     V_mean(:,ib)=V_mean(:,ib)+WFo%c(:,ifft)*Sigma_times_R(ibp,ib)
     !
   enddo
   !
   ifft=WFo%index(ib,ik,1)
   !
   do ir=1,fft_size
     V_mean(ir,ib)=V_mean(ir,ib)/WFo%c(ir,ifft)
   enddo
   !
 enddo
 !
end subroutine 

!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG
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
subroutine OEP_app_inversion(E,k,Vkli,Vslt,U,rho)
 !
 !  Exchange KLI/CEDA
 !
 !  Found in reciprocal space as:
 !  vkli_RL(G) = Ko^{-1}(G,G')rho_{xc}(G')
 !  
 !  where: 
 !  Ko is the approximated Xo:
 !  \rho(G)\delta_{G,G'} - 2*int_k sum_{v} rho_{v,v}(k,G) [rho_{v,v}(k,G')]^* +c.c.
 !  and 
 !  rho_{xc}(G)= W(G) - 2*int_k sum_{v} rho_{v,v}(k,G) [SigmaXC_{v,v}(k)]^* +c.c.  
 !  where W(G) is the Hole/Slater part of the potential multiplied by the density
 !
 use pars,            ONLY:SP,DP,schlen,cZERO
 use D_lattice,       ONLY:i_time_rev,nsym
 use electrons,       ONLY:levels,n_met_bands
 use R_lattice,       ONLY:bz_samp,nkibz,nkbz
 use FFT_m,           ONLY:fft_size,fft_g_table,fft_dim,fftw_plan,fft_rot_r
 use linear_algebra,  ONLY:INV
 use collision_el,    ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use QP_m,            ONLY:QP_ng_Sx
 use SC,              ONLY:it_now,l_oep_EWeight
 use drivers,         ONLY:l_oep_kli,l_oep_ceda
 use parallel_m,      ONLY:PP_indexes,myid,PP_indexes_reset
 use interfaces,      ONLY:LINEAR_ALGEBRA_driver 
 use parallel_int,    ONLY:PP_redux_wait,PARALLEL_index 
 use wave_func,       ONLY:WF
 use LIVE_t,          ONLY:live_timing
 use stderr,          ONLY:intc
 !
 ! I/O
 !
#include<memory.h>
 real(SP),    intent(out):: Vkli(fft_size)
 real(SP),    intent(in) :: Vslt(fft_size),rho(fft_size)
 complex(SP), intent(in) :: U(nkibz,n_met_bands,fft_size)
 type(levels), intent(in):: E
 type(bz_samp),intent(in):: k
 !
 ! Work Space
 !
 integer                  :: i1,ik,iv1,iv2,is,ig1,ig2,ifft,offt
 real(SP)                 :: EWeight, Prefactor
 complex(SP), allocatable :: rhotw_sum(:),rhotw_sum_TR(:),rho_xc(:),&
&                            Vkli_RL(:),Ko(:,:),rhotw_prd(:,:),rhotw_rs(:)
 complex(SP)              :: SigmaXC
 complex(DP), allocatable :: Vx_dp(:),W_tmp(:),rhotw_ds(:)
 character(schlen)        :: ch
 type(elemental_collision):: isc
 type(PP_indexes)         :: px
 logical                  :: l_is_TR
 !
 ! allocation
 ! 
 YAMBO_ALLOC(W_tmp,(fft_size))
 YAMBO_ALLOC(rho_xc,(QP_ng_Sx-1))
 YAMBO_ALLOC(Ko,(QP_ng_Sx-1,QP_ng_Sx-1))
 !
 !FT the Hole potential 
 !
 W_tmp = Vslt/rho
#if defined _FFTW
 fftw_plan = 0
 call fft_3d(W_tmp,fft_dim,-1,fftw_plan)
#else
 call fft_3d(W_tmp,fft_dim,-1)
#endif
 rho_xc = (0._SP,0._SP)
 rho_xc(1:QP_ng_Sx-1) = W_tmp(fft_g_table(2:QP_ng_Sx,1))/real(fft_size,SP)
 YAMBO_FREE(W_tmp)
 ! 
 ! Calculate Ko(G,G') and rho_xc
 !     =>1 - initialize vars/dim
 !       for each k in IBZ:
 !       for each v: 
 !       2 - calculate  rhotw_sum (G) = sum{R} Rho^*_{v}(k,R^-1 G)
 !       3 - accumulate Ko(G,G') = Ko(G,G') - occ* sum{R} Rho^*_{v}(k,R^-1 G)*rho_{v}(k,R^-1 G')
 !       4 - accumulate rho_xc
 ! -----------------------------------------------
 ! NB:   not tested for metallic systems, might not work
 ! -----------------------------------------------
 call elemental_collision_free(isc)
 call PP_indexes_reset(px)
 !
 call elemental_collision_alloc(isc,TITLE="OEP_SigmaCSX",NG=QP_ng_Sx)
 YAMBO_ALLOC(rhotw_sum,(QP_ng_Sx-1))
 YAMBO_ALLOC(rhotw_sum_TR,(QP_ng_Sx-1))
 YAMBO_ALLOC(rhotw_prd,(QP_ng_Sx,QP_ng_Sx))
 YAMBO_ALLOC(rhotw_ds,(fft_size))
 YAMBO_ALLOC(rhotw_rs,(QP_ng_Sx))
 !
 call PARALLEL_index(px,(/nkibz,E%nbf/))
 call PP_redux_wait
 if (l_oep_kli) ch="KLI@it"//trim(intc(it_now))
 if (l_oep_ceda) ch="CED@it"//trim(intc(it_now))
 call live_timing(trim(ch),px%n_of_elements(myid+1))
 EWeight = 1._SP
 rhotw_prd = (0._SP,0._SP)
 do ik = 1, nkibz
   do iv1 = 1, E%nbf
     if (l_oep_EWeight) EWeight = E%E(iv1,ik,1)+ E%E_Fermi 
     do iv2 = 1, iv1!E%nbf
       if (l_oep_kli.and.(iv2.ne.iv1)) cycle
       ifft=WF%index(iv1,ik,1)
       offt=WF%index(iv2,ik,1)
       rhotw_sum = (0._SP,0._SP)
       rhotw_sum_TR = (0._SP,0._SP)
       Prefactor = 1.0_SP
       if (iv1.ne.iv2) Prefactor = 2.0_SP
       SigmaXC = dot_product(WF%c(:,1,ifft),U(ik,iv2,:)) 
       do i1 = 1,k%nstar(ik)
         is  = k%star(ik,i1)
         l_is_TR = is>nsym/(i_time_rev+1)
         isc%is = (/iv1,ik,is,1/)
         isc%os = (/iv2,ik,is,1/)
         isc%qs = (/1,1,1/)
         call scatter_Bamp(isc)
         !
         !rhotw_ds(:)=conjg(WF%c(fft_rot_r(:,is),1,ifft))*WF%c(fft_rot_r(:,is),1,offt)/EWeight
         rhotw_ds(:)=WF%c(fft_rot_r(:,is),1,ifft)*conjg(WF%c(fft_rot_r(:,is),1,offt))/EWeight
         !
         rhotw_ds = rhotw_ds/(rho*real(fft_size,SP)) 
         rhotw_rs = cZERO
         !
         ! r -> G transform; 
         ! In order to be compliant with FFTQE -1 should be used in calls to FFT
         ! This implies that the above conjg of rhotw_ds has been reversed, and
         ! that rhotw_ds will be used as conjugaed in the following.
         !
#if defined _FFTW
         fftw_plan = 0
         !call fft_3d(rhotw_ds,fft_dim,+1,fftw_plan)
         call fft_3d(rhotw_ds,fft_dim,-1,fftw_plan)
#else
         !call fft_3d(rhotw_ds,fft_dim,+1)
         call fft_3d(rhotw_ds,fft_dim,-1)
#endif
         rhotw_rs(1:QP_ng_Sx) = rhotw_ds(fft_g_table(1:QP_ng_Sx,1))
         !forall(ig1=2:QP_ng_Sx,ig2=2:QP_ng_Sx) rhotw_prd(ig1,ig2) = rhotw_prd(ig1,ig2) + &
         !  & Prefactor*E%f(iv1,ik,1)*conjg(rhotw_rs(ig1))*isc%rhotw(ig2)
         forall(ig1=2:QP_ng_Sx,ig2=2:QP_ng_Sx) rhotw_prd(ig1,ig2) = rhotw_prd(ig1,ig2) + &
           & Prefactor*E%f(iv1,ik,1)*rhotw_rs(ig1)*isc%rhotw(ig2)
         !
         if (l_is_TR) then
           !rhotw_sum_TR(1:QP_ng_Sx-1)=rhotw_sum_TR(1:QP_ng_Sx-1)+rhotw_rs(2:QP_ng_Sx)
           rhotw_sum_TR(1:QP_ng_Sx-1)=rhotw_sum_TR(1:QP_ng_Sx-1)+conjg(rhotw_rs(2:QP_ng_Sx))
         else
           !rhotw_sum(1:QP_ng_Sx-1)=rhotw_sum(1:QP_ng_Sx-1)+conjg(rhotw_rs(2:QP_ng_Sx))
           rhotw_sum(1:QP_ng_Sx-1)=rhotw_sum(1:QP_ng_Sx-1)+rhotw_rs(2:QP_ng_Sx)
         end if         
       end do
       rho_xc(:) =  rho_xc(:) + Prefactor*E%f(iv1,ik,1)*& 
            &(rhotw_sum(:)*SigmaXC+rhotw_sum_TR*conjg(SigmaXC))/(real(nkbz,SP)) 
       call live_timing(steps=1)
     end do
   end do
 end do
 !
 call live_timing()
 !
 Ko = (0._SP,0._SP)
 do ig1 = 1,QP_ng_Sx-1
   Ko(ig1,ig1) = (1._SP,0._SP)  
   Ko(ig1,1:QP_ng_Sx-1) = Ko(ig1,1:QP_ng_Sx-1) - rhotw_prd(ig1+1,2:QP_ng_Sx)/float(nkbz)
 end do
 !
 YAMBO_FREE(rhotw_sum)
 YAMBO_FREE(rhotw_sum_TR)
 YAMBO_FREE(rhotw_prd)
 YAMBO_FREE(rhotw_ds)
 YAMBO_FREE(rhotw_rs)
 !
 ! Calculate Vkli (RL) 
 !
 YAMBO_ALLOC(Vkli_RL,(QP_ng_Sx-1))
 call LINEAR_ALGEBRA_driver(INV,M=Ko)
 ! 
 Vkli_RL=matmul(Ko,rho_xc)
 !
 YAMBO_FREE(rho_xc)
 YAMBO_FREE(Ko)
 !
 ! Calculate Vkli (DL)
 !
 YAMBO_ALLOC(Vx_dp,(fft_size))
 Vx_dp = (0._DP,0._DP)
 Vx_dp(fft_g_table(2:QP_ng_Sx,1))= Vkli_RL(1:QP_ng_Sx-1)
#if defined _FFTW
 fftw_plan = 0
 call fft_3d(Vx_dp,fft_dim,1,fftw_plan)
#else
 call fft_3d(Vx_dp,fft_dim,1)
#endif
 !
 Vkli = real(Vx_dp)
 !
 ! CLEAN
 !
 YAMBO_FREE(Vkli_RL)
 YAMBO_FREE(Vx_dp)
 call elemental_collision_free(isc)
 call PP_indexes_reset(px)
 !
end subroutine OEP_app_inversion

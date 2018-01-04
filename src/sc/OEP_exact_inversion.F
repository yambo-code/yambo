!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine OEP_exact_inversion(X,E,k,Voep,W_x)
 !
 !  Exact exchange/static RPA potential (voep in direct space)
 !
 !  Found in reciprocal space as:
 !  voep_RL(G) = Xo^{-1}(G,G')rho_{xc}(G)
 !  
 !  where: 
 !  rho_xc(G)= int_k sum_{c,v} ( rho_{v,c}(k,G)   SigmaXC_{v,c}(k)   +  
 !                            + rho^*_{v,c}(k,G) SigmaXC^*_{v,c}(k))\
 !                            (E_{vk} - E_{ck})
 !
 use pars,            ONLY:SP,DP
 use D_lattice,       ONLY:DL_vol,i_time_rev,sop_inv,nsym,i_space_inv
 use DIPOLES,         ONLY:DIP_alloc
 use X_m,             ONLY:X_t, X_alloc,X_mat,X_poles_tab,current_iq 
 use electrons,       ONLY:levels,n_met_bands
 use R_lattice,       ONLY:bz_samp,g_rot,nkibz,nkbz
 use FFT_m,           ONLY:fft_size,fft_g_table,fft_dim,fftw_plan
 use frequency,       ONLY:w_samp,W_reset,bare_grid_N,coarse_grid_Pt,coarse_grid_index,ordered_grid_index
 use interfaces,      ONLY:LINEAR_ALGEBRA_driver
 use linear_algebra,  ONLY:INV
 use collision_el,    ONLY:elemental_collision,elemental_collision_free
 use wave_func,       ONLY:WF
 !
 ! I/O
 !
#include<memory.h>
 real(SP),intent(out)    :: Voep(fft_size)
 complex(SP),intent(in)  :: W_x(nkibz,n_met_bands,fft_size)
 type(X_t)    :: X
 type(levels),intent(in) :: E
 type(bz_samp),intent(in):: k
 !
 ! Work Space
 !
 integer                  :: i1,ik,ic,iv,is,ig1,ifft,offt
 logical                  :: l_is_TR
 complex(SP), allocatable :: Xo_inverse(:,:),&
&                            rhotw_sum(:),rhotw_sum_TR(:),rho_xc(:),Voep_RL(:)
 complex(DP), allocatable :: Vx_dp(:),rho_tw_rs(:)
 complex(SP)              :: SigmaXC
 type(w_samp)             :: Xw
 type(elemental_collision):: isc
 !
 ! Calculate Xo^{-1}(G,G') => 1 - initialize vars/dim
 !                            2 - calculate Xo, 
 !                            3 - delete wings,   
 !                            4 - invert
 ! -----------------------------------------------
 ! 1:
 call W_reset(Xw)
 Xw%n_freqs = 1
 call FREQUENCIES_setup(Xw)
 Xw%p(1)=real(Xw%p(1))+(0.,1.)*epsilon(1._SP)
 call X_alloc('X',(/X%ng,X%ng,Xw%n_freqs/))
 X%ordering='c'
 !
 ! 2:
 call IO_and_Messaging_switch("-report") 
 call X_irredux(X_mat,1,E,k,Xw,X)
 call IO_and_Messaging_switch("+report")
 ! 3:
 YAMBO_ALLOC(Xo_inverse,(X%ng-1,X%ng-1))
 Xo_inverse(:X%ng-1,:X%ng-1) =  X_mat(2:X%ng,2:X%ng,1) 
 call X_alloc('X')
 ! 4:
 call LINEAR_ALGEBRA_driver(INV,M=Xo_inverse)
 !
 !
 ! Calculate rho_xc(G) =>  1 - initialize vars/dim
 !                        for each k in IBZ:
 !                        2 - retabulate SigmaXC elements  ! This has now changed
 !                        for each c,v,k: 
 !                        3 - calculate  rhotw_sum (G) = sum{R} rho^*_{v,c}(k,R^-1 G)
 !                                       rhotw_sum_TR (G) = sum{I R} rho_{v,c}(k,(IR)^-1 G)
 !                        4 - accumulate rho_xc
 ! -----------------------------------------------
 ! NB: a. it does not work for metallic systems
 ! -----------------------------------------------
 ! 1:
 call elemental_collision_free(isc)
 isc%ngrho=X%ng
 YAMBO_ALLOC(rhotw_sum,(X%ng))
 YAMBO_ALLOC(rhotw_sum_TR,(X%ng))
 YAMBO_ALLOC(rho_xc,(X%ng))
 YAMBO_ALLOC(rho_tw_rs,(fft_size))
 !
 rho_xc = (0.,0.)
 !
 ! 3:
 do ik = 1, nkibz 
   do iv = 1 ,E%nbf
     do ic = E%nbf+1, X%ib(2)
       ifft=WF%index(iv,ik,1)
       offt=WF%index(ic,ik,1)
       SigmaXC = dot_product(WF%c(:,1,offt),W_x(ik,iv,:)) !<ic|Sigma_xc|iv>
       !
       !rho_tw_rs(:) = conjg(WF%c(:,ifft))*WF%c(:,offt)
       rho_tw_rs(:) = WF%c(:,1,ifft)*conjg(WF%c(:,1,offt))
       !
       ! r -> G transform; 
       ! In order to be compliant with FFTQE -1 should be used in calls to FFT
       ! This implies that the above conjg of rho_tw_rs has been reversed, and
       ! that rho_tw_rs will be used as conjugaed in the following.
       !
#if defined _FFTW
       !call fft_3d(rho_tw_rs(:),fft_dim,+1,fftw_plan)
       call fft_3d(rho_tw_rs(:),fft_dim,-1,fftw_plan)
#else
       !call fft_3d(rho_tw_rs(:),fft_dim,+1)
       call fft_3d(rho_tw_rs(:),fft_dim,-1)
#endif
       rhotw_sum    = (0.,0.)
       rhotw_sum_TR = (0.,0.)
       !
       do i1 = 1,k%nstar(ik)
         !
         is      = sop_inv(k%star(ik,i1))
         l_is_TR = is>nsym/(i_time_rev+1)
         !
         rho_tw_rs(fft_g_table(g_rot(1,1),1)) = (0.,0.)
         if (l_is_TR) then
           !rhotw_sum_TR(:X%ng)=rhotw_sum_TR(:X%ng)+rho_tw_rs(fft_g_table(g_rot(is,:X%ng),1))
           rhotw_sum_TR(:X%ng)=rhotw_sum_TR(:X%ng)+conjg(rho_tw_rs(fft_g_table(g_rot(is,:X%ng),1)))
         else
           !rhotw_sum(:X%ng)=rhotw_sum(:X%ng)+conjg(rho_tw_rs(fft_g_table(g_rot(is,:X%ng),1)))
           rhotw_sum(:X%ng)=rhotw_sum(:X%ng)+rho_tw_rs(fft_g_table(g_rot(is,:X%ng),1))
         endif
         !
       end do
       ! 4:
       rho_xc(:) =  rho_xc(:) + E%f(iv,ik,1)*(rhotw_sum(:)*conjg(SigmaXC)+rhotw_sum_TR(:)*SigmaXC)/&
&                  (E%E(iv,ik,1)-E%E(ic,ik,1))
       !
     end do
   end do
 end do 
 YAMBO_FREE(rhotw_sum)
 YAMBO_FREE(rhotw_sum_TR)
 YAMBO_FREE(rho_tw_rs)
 call DIP_alloc('DIP_projected')
 !
 ! Add c.c. contribution
 !
 if (i_time_rev/=0) then
   rho_xc=2.*rho_xc
 else if (i_space_inv/=0) then
   rho_xc=rho_xc+conjg(rho_xc)
 endif
 YAMBO_ALLOC(Voep_RL,(X%ng-1))
 !
 ! Calculate Voep_RL
 !
 Voep_RL=matmul(Xo_inverse,rho_xc(2:))
 YAMBO_FREE(rho_xc)
 YAMBO_FREE(Xo_inverse)
 !
 ! Calculate Voep (DL)
 !
 YAMBO_ALLOC(Vx_dp,(fft_size))
 Vx_dp = (0.,0.)
 do ig1=2,X%ng
   Vx_dp(fft_g_table(ig1,1))= Voep_RL(ig1-1)/(DL_vol*float(nkbz)) 
 enddo
#if defined _FFTW
 fftw_plan = 0
 call fft_3d(Vx_dp,fft_dim,1,fftw_plan)
 fftw_plan = 0
#else
 call fft_3d(Vx_dp,fft_dim,1)
#endif
 !
 Voep = real(Vx_dp)
 !
 ! CLEAN
 !
 YAMBO_FREE(Voep_RL)
 YAMBO_FREE(Vx_dp)
 call elemental_collision_free(isc)
 call W_reset(Xw)
 !
 ! Reset coarse-grid arrays used in X_os
 !
 YAMBO_FREE(ordered_grid_index)
 YAMBO_FREE(X_poles_tab)
 YAMBO_FREE(bare_grid_N)
 YAMBO_FREE(coarse_grid_Pt)
 YAMBO_FREE(coarse_grid_index)
 current_iq=0
 !
end subroutine OEP_exact_inversion


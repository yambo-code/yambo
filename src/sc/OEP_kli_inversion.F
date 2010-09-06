!
! Copyright (C) 2000-2010 M. Gruening and the YAMBO team
!              http://www.yambo-code.org
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
subroutine OEP_kli_inversion(E,k,Vkli,Vslt,U,rho)
 !
 !  Exchange KLI
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
 use pars,            ONLY:SP,DP
 use D_lattice,       ONLY:DL_vol,sop_inv,i_time_rev,nsym
 use electrons,       ONLY:levels,n_met_bands
 use R_lattice,       ONLY:bz_samp,g_rot,nkibz,nkbz
 use FFT_m,           ONLY:fft_size,fft_g_table,fft_dim,fftw_plan
 use frequency,       ONLY:w_samp,W_reset,bg_npts,cg_pt,cg_index_bg,rg_index_bg
 use matrix_operate,  ONLY:mat_dia_inv,INV,USE_LK
 use collision,       ONLY:ggwinfo,collision_reset
 use QP_m,            ONLY:QP_Vnl_xc,QP_n_states,QP_table,QP_ng_Sx
 use memory_m,        ONLY:mem_est
 use SC,              ONLY:SC_bands,SC_R,H_rotate
 use drivers,         ONLY:l_collisions_IO
 use memory_m,        ONLY:mem_est
 use par_proc_m,      ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,   ONLY:par_indexes
 use wave_func,       ONLY:wf_state,wf
 !
 ! I/O
 !
 implicit none
 real(SP),    intent(out):: Vkli(fft_size)
 real(SP),    intent(in) :: Vslt(fft_size),rho(fft_size)
 complex(SP), intent(in) :: U(nkibz,n_met_bands,fft_size)
 type(levels), intent(in):: E
 type(bz_samp),intent(in):: k
 !
 ! Work Space
 !
 integer                  :: i1,ik,iv1,iv2,is,ig1,ig2,ifft,ir
 complex(SP), allocatable :: SigmaXC(:),rhotw_sum(:),rho_xc(:),Vkli_RL(:),&
&                            Ko(:,:),rhotw_prd(:,:),rhotw_rs(:)
 complex(DP), allocatable :: Vx_dp(:),W_tmp(:),rhotw_ds(:)
 type(ggwinfo)            :: isc
 type(pp_indexes)         :: px
 !
 ! allocation
 ! 
 allocate(W_tmp(fft_size))
 allocate(rho_xc(QP_ng_Sx-1),Ko(QP_ng_Sx-1,QP_ng_Sx-1))
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
 deallocate(W_tmp)
 ! 
 ! Calculate Ko(G,G') and rho_xc
 !     =>1 - initialize vars/dim
 !       for each k in IBZ:
 !       2 - retabulate SigmaXC elements
 !       for each v,k: 
 !       3 - calculate  rhotw_sum (G) = sum{R} Rho^*_{v}(k,R^-1 G)
 !       4 - accumulate Ko(G,G') = Ko(G,G') - occ* sum{R} Rho^*_{v}(k,R^-1 G)*rho_{v}(k,R^-1 G')
 !       5 - accumulate rho_xc
 ! -----------------------------------------------
 ! NB:   it does not work for metallic systems
 ! -----------------------------------------------
 !1:
 Ko = (0._SP,0._SP)
 forall(ig1=1:QP_ng_Sx-1) Ko(ig1,ig1) = (1._SP,0._SP)  
 call collision_reset(isc)
 call pp_indexes_reset(px)
 isc%ngrho=QP_ng_Sx
 allocate(isc%rhotw(QP_ng_Sx),rhotw_sum(QP_ng_Sx-1),rhotw_prd(QP_ng_Sx,QP_ng_Sx))
 allocate(rhotw_ds(fft_size),rhotw_rs(QP_ng_Sx),SigmaXC(E%nbf))
 ! 2:
 call par_indexes(px,(/nkibz,E%nbf/))
 call pp_redux_wait
 do ik = 1, nkibz
   rhotw_prd = (0._SP,0._SP)
   SigmaXC = (0._SP,0._SP)
   ! 3:
   do iv1 = 1 ,E%nbf
     isc%is = (/iv1,ik,1,1/)
     isc%os = (/iv1,ik,1,1/)
     isc%qs = (/1,1,1/)
     !
     do ir = 1,fft_size
       SigmaXC(iv1) = SigmaXC(iv1) + U(ik,iv1,ir)
     end do
     !
     call scatterBamp(isc)
     ifft=wf_state(iv1,ik,1)
     rhotw_ds(:)=conjg(wf(:,ifft))*wf(:,ifft)/rho(:)*real(fft_size,DP) 
#if defined _FFTW
     fftw_plan = 0
     call fft_3d(rhotw_ds,fft_dim,+1,fftw_plan)
#else
     call fft_3d(rhotw_ds,fft_dim,+1)
#endif
     rhotw_rs(1:QP_ng_Sx) = rhotw_ds(fft_g_table(1:QP_ng_Sx,1))/real(fft_size,SP) 
     forall(ig1=2:QP_ng_Sx,ig2=2:QP_ng_Sx) rhotw_prd(ig1,ig2) = E%f(iv1,ik,1)*conjg(rhotw_rs(ig1))*isc%rhotw(ig2)
     !
     rhotw_sum    = (0.,0.)
     !
     do i1 = 1,k%nstar(ik)
       is  = k%star(ik,i1)
       if (is>nsym/(i_time_rev+1)) is=is-nsym/(i_time_rev+1)
       !
       forall(ig1 = 2:QP_ng_Sx)       Ko(ig1-1,1:QP_ng_Sx-1) = Ko(ig1-1,1:QP_ng_Sx-1) -&
           & rhotw_prd(g_rot(sop_inv(is),ig1),g_rot(sop_inv(is),2:QP_ng_Sx)) +&
           & conjg(rhotw_prd(g_rot(sop_inv(is),ig1),g_rot(sop_inv(is),2:QP_ng_Sx)))
       !
       forall(ig1=2:QP_ng_Sx) rhotw_sum(ig1-1)=rhotw_sum(ig1-1)+rhotw_rs(g_rot(sop_inv(is),ig1))
       !
     end do
     ! 4:
     rho_xc(1:QP_ng_Sx-1) =  rho_xc(1:QP_ng_Sx-1) - E%f(iv1,ik,1)*(conjg(rhotw_sum(2:QP_ng_Sx))*SigmaXC(iv1)&
          &+rhotw_sum(2:QP_ng_Sx)*conjg(SigmaXC(iv1)))
     !
   end do
 end do
 !
 deallocate(isc%rhotw,rhotw_sum,rhotw_prd,SigmaXC,rhotw_ds,rhotw_rs)!,rho)
 if (allocated(QP_Vnl_xc))  deallocate(QP_Vnl_xc)
 !
 ! Calculate Vkli (RL) 
 !
 allocate(Vkli_RL(QP_ng_Sx-1))!DEBUG
 call mat_dia_inv(INV,USE_LK,Ko)
 !
 Vkli_RL=matmul(Ko,rho_xc)
  !
 deallocate(rho_xc)
 !
 ! Calculate Vkli (DL)
 !
 allocate(Vx_dp(fft_size))
 Vx_dp = (0._DP,0._DP)
 Vx_dp(fft_g_table(1:QP_ng_Sx-1,1))= Vkli_RL(1:QP_ng_Sx-1)/(DL_vol*float(nkbz))
#if defined _FFTW
 fftw_plan = 0
 call fft_3d(Vx_dp,fft_dim,1,fftw_plan)
 fftw_plan = 0
#else
 call fft_3d(Vx_dp,fft_dim,1)
#endif
 !
 Vkli = real(Vx_dp)
  !
  ! CLEAN
  !
 deallocate(Vkli_RL,Vx_dp)
 call collision_reset(isc)
 call pp_indexes_reset(px)
  !
end subroutine OEP_kli_inversion

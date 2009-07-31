!
! Copyright (C) 2000-2008 M. Gruening and the YAMBO team
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
subroutine V_exx(X,E,k,q,Vexx)
 !
 !  Exact exchange potential (vexx in direct space)
 !
 !  Found in reciprocal space as:
 !  vexx_RL(G) = Xo^{-1}(G,G')rho_{xc}(G)
 !  
 !  where: 
 !  rho_x(G) = int_k sum_{c,v} ( rho_{v,c}(k,G)   SigmaX_{v,c}(k)   +  
 !                            + rho^*_{v,c}(k,G) SigmaX^*_{v,c}(k))\
 !                            (E_{vk} - E_{ck})
 !
 use pars,            ONLY:SP,DP
 use D_lattice,       ONLY:DL_vol,i_time_rev,sop_inv,nsym,i_space_inv
 use X_m,             ONLY:X_t, X_alloc,X_mat,X_poles_tab,current_iq 
 use electrons,       ONLY:levels
 use R_lattice,       ONLY:bz_samp,g_rot,nkibz,nkbz
 use FFT_m,           ONLY:fft_size,fft_g_table,fft_dim,fftw_plan
 use frequency,       ONLY:w_samp,W_reset,bg_npts,cg_pt,cg_index_bg,rg_index_bg
 use matrix_operate,  ONLY:mat_dia_inv,INV,USE_LK
 use collision,       ONLY:ggwinfo,collision_reset
 use QP_m,            ONLY:QP_Sx,QP_n_states,QP_table
 use memory_m,        ONLY:mem_est
 !
 ! I/O
 !
 implicit none
 real(SP) :: Vexx(fft_size)
 type(X_t)    :: X
 type(levels) :: E
 type(bz_samp):: k,q
 !
 ! Work Space
 !
 integer                  :: i1,ik,ic,iv,is,ig1,ig2,ikbz
 logical                  :: l_is_TR
 complex(SP), allocatable :: Xo_inverse(:,:), SigmaX(:,:),rhotw_save(:),&
&                            rhotw_sum(:),rhotw_sum_TR(:),rho_x(:),Vexx_RL(:)
 complex(DP), allocatable :: Vx_dp(:)
 type(w_samp)             :: Xw
 type(ggwinfo)            :: isc
 !
 ! Calculate Xo^{-1}(G,G') => 1 - initialize vars/dim
 !                            2 - calculate Xo, 
 !                            3 - delete wings,   
 !                            4 - invert
 ! -----------------------------------------------
 ! 1:
 call W_reset(Xw)
 Xw%n = 1
 call FREQUENCIES_setup(Xw)
 Xw%p(1)=real(Xw%p(1))+(0.,1.)*epsilon(1._SP)
 !
 ! 2:
 call X_alloc('X',(/X%ng,X%ng,Xw%n(2)/))
 call IO_mute("report") 
 call X_os(X_mat,1,(/1,Xw%n(1)/),E,k,Xw,X)
 call IO_mute("") 
 ! 3:
 allocate(Xo_inverse(X%ng-1,X%ng-1))
 Xo_inverse(:X%ng-1,:X%ng-1) =  X_mat(2:X%ng,2:X%ng,1) 
 call X_alloc('X')
 ! 4:
 call mat_dia_inv(INV,USE_LK,Xo_inverse)
 !
 ! Calculate rho_x(G) =>  1 - calculate SigmaX_{v,c}(k)
 !                        2 - initialize vars/dim
 !                        for each k in IBZ:
 !                        3 - retabulate SigmaX elements
 !                        for each c,v,k: 
 !                        4 - calculate  rhotw_sum (G) = sum{R} rho^*_{v,c}(k,R^-1 G)
 !                                       rhotw_sum_TR (G) = sum{I R} rho_{v,c}(k,(IR)^-1 G)
 !                        5 - accumulate rho_x
 ! -----------------------------------------------
 ! NB: a. it does not work for metallic systems
 ! -----------------------------------------------
 ! 1:
 allocate(QP_Sx(QP_n_states))
 call mem_est("QP_Sx",(/QP_n_states/))
 call IO_mute("report io_in io_out") 
 call QP_XX_Vxc(E,k,k,q)
 call IO_mute(" ")
 ! 2:
 call collision_reset(isc)
 isc%ngrho=X%ng
 allocate(isc%rhotw(X%ng),rhotw_save(X%ng),rhotw_sum(X%ng-1),rhotw_sum_TR(X%ng-1),&
&         SigmaX(E%nbf,X%ib(2)),rho_x(X%ng-1))
 ! 3:
 rho_x = (0.,0.)
 !
 do ik = 1, nkibz
   !
   ! Accumulate QP_Sx in a local SigmaX array
   !
   SigmaX = (0.,0.)
   do i1 = 1,QP_n_states
     iv = QP_table(i1,1)
     ic = QP_table(i1,2)
     if (QP_table(i1,3)/=ik.or.iv>E%nbf.or.ic<=E%nbf) cycle
     SigmaX(iv,ic) = QP_Sx(i1)
   enddo
   ! 4:
   do ic = E%nbf+1, X%ib(2)
     do iv = 1 ,E%nbf
       isc%is = (/iv,ik,1,1/)
       isc%os = (/ic,ik,1,1/)
       isc%qs = (/1,1,1/)
       !
       call scatterBamp(isc)
       rhotw_save = isc%rhotw
       rhotw_sum    = (0.,0.)
       rhotw_sum_TR = (0.,0.)
       !
       do i1 = 1,k%nstar(ik)
         !
         is      = k%star(ik,i1)
         l_is_TR = is>nsym/(i_time_rev+1)
         ikbz    = sum(k%nstar(:ik-1))+i1
         !
         do ig1=1,X%ng
           ig2 = g_rot(sop_inv(is),ig1)
           isc%rhotw(ig1) = rhotw_save(ig2)
         enddo
         if (l_is_TR) then
           forall(ig1=2:X%ng) rhotw_sum_TR(ig1-1)=rhotw_sum_TR(ig1-1)+isc%rhotw(ig1)
         else
           forall(ig1=2:X%ng) rhotw_sum(ig1-1)=rhotw_sum(ig1-1)+conjg(isc%rhotw(ig1))
         endif
         !
       end do
       ! 5:
       rho_x(:) =  rho_x(:) + E%f(iv,ik,1)*(2.-E%f(ic,ik,1))/2.*&
&                  (rhotw_sum(:)*SigmaX(iv,ic)+rhotw_sum_TR(:)*conjg(SigmaX(iv,ic)))/&
&                  (E%E(iv,ik,1)-E%E(ic,ik,1)) 
       !
     end do
   end do
 end do 
 deallocate(isc%rhotw,rhotw_save,rhotw_sum,rhotw_sum_TR,SigmaX)
 call X_alloc('OptOsc')
 if (allocated(QP_Sx))  deallocate(QP_Sx)
 !
 ! Add c.c. contribution
 !
 if (i_time_rev/=0) then
   rho_x=2.*rho_x
 else if (i_space_inv/=0) then
   rho_x=rho_x+conjg(rho_x)
 endif
 allocate(Vexx_RL(X%ng-1))
 !
 ! Calculate Vexx_RL
 !
 Vexx_RL=matmul(Xo_inverse,rho_x)
 deallocate(rho_x)
 !
 ! Calculate Vexx (DL)
 !
 allocate(Vx_dp(fft_size))
 Vx_dp = (0.,0.)
 do ig1=2,X%ng
   Vx_dp(fft_g_table(ig1,1))= Vexx_RL(ig1-1)/(DL_vol*float(nkbz)) 
 enddo
#if defined _FFTW
 fftw_plan = 0
 call fft_3d(Vx_dp,fft_dim,1,fftw_plan)
 fftw_plan = 0
#else
 call fft_3d(Vx_dp,fft_dim,1)
#endif
 !
 Vexx = real(Vx_dp)
 !
 ! CLEAN
 !
 deallocate(Vexx_RL,Vx_dp)
 call collision_reset(isc)
 call W_reset(Xw)
 !
 ! Reset coarse-grid arrays used in X_os
 !
 if (allocated(rg_index_bg)) deallocate(rg_index_bg)
 deallocate(X_poles_tab,bg_npts,cg_pt,cg_index_bg)
 call mem_est("X_poles_tab RGi BGn CGp CGi")
 current_iq=0
 !
end subroutine V_exx

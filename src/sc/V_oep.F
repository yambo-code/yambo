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
subroutine V_oep(X,E,k,q,Voep)
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
 use X_m,             ONLY:X_t, X_alloc,X_mat,X_poles_tab,current_iq 
 use electrons,       ONLY:levels
 use R_lattice,       ONLY:bz_samp,g_rot,nkibz,nkbz
 use FFT_m,           ONLY:fft_size,fft_g_table,fft_dim,fftw_plan
 use frequency,       ONLY:w_samp,W_reset,bg_npts,cg_pt,cg_index_bg,rg_index_bg
 use matrix_operate,  ONLY:mat_dia_inv,INV,USE_LK
 use collision,       ONLY:ggwinfo,collision_reset
 use QP_m,            ONLY:QP_Vnl_xc,QP_n_states,QP_table,QP_Sc
 use memory_m,        ONLY:mem_est
 use SC,              ONLY:SC_bands,SC_R,H_rotate
 use drivers,         ONLY:l_collisions_IO,l_sc_srpa
 !
 ! I/O
 !
 implicit none
 real(SP) :: Voep(fft_size)
 type(X_t)    :: X
 type(levels) :: E
 type(bz_samp):: k,q
 !
 ! Work Space
 !
 integer                  :: i1,ik,ic,iv,is,ig1,ig2,ikbz
 logical                  :: l_is_TR
 complex(SP), allocatable :: Xo_inverse(:,:), SigmaXC(:,:,:),rhotw_save(:,:),&
&                            rhotw_sum(:),rhotw_sum_TR(:),rho_xc(:),Voep_RL(:)
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
 call IO_and_Messaging_switch("-report") 
 call X_os(X_mat,1,(/1,Xw%n(1)/),E,k,Xw,X)
 call IO_and_Messaging_switch("+report") 
 ! 3:
 allocate(Xo_inverse(X%ng-1,X%ng-1))
 Xo_inverse(:X%ng-1,:X%ng-1) =  X_mat(2:X%ng,2:X%ng,1) 
 call X_alloc('X')
 ! 4:
 call mat_dia_inv(INV,USE_LK,Xo_inverse)
 !
 ! Calculate rho_xc(G) =>  1 - initialize vars/dim
 !                        for each k in IBZ:
 !                        2 - retabulate SigmaXC elements
 !                        for each c,v,k: 
 !                        3 - calculate  rhotw_sum (G) = sum{R} rho^*_{v,c}(k,R^-1 G)
 !                                       rhotw_sum_TR (G) = sum{I R} rho_{v,c}(k,(IR)^-1 G)
 !                        4 - accumulate rho_xc
 ! -----------------------------------------------
 ! NB: a. it does not work for metallic systems
 ! -----------------------------------------------
 ! 1:
 call collision_reset(isc)
 isc%ngrho=X%ng
 allocate(isc%rhotw(X%ng),rhotw_sum(X%ng-1),rhotw_sum_TR(X%ng-1),&
&         SigmaXC(X%ib(2),X%ib(2),1),rho_xc(X%ng-1))
 if (i_time_rev/=0.or.i_space_inv/=0) then
   allocate(rhotw_save(X%ng,1))
 else
   allocate(rhotw_save(X%ng,2))
 endif
 ! 2:
 rho_xc = (0.,0.)
 !
 do ik = 1, nkibz 
   !
   ! Accumulate QP_Vnl_xc in a local SigmaXC array
   ! If COLLISIONS_IO_compose was used, I need also to rotate the matrix elements
   !
   SigmaXC = (0.,0.)
    if (l_collisions_IO)  then
      call V_qp_basis_to_H(ik,QP_Vnl_xc,SigmaXC)
      if (l_sc_srpa) call V_qp_basis_to_H(ik,QP_Sc,SigmaXC)
      call H_rotate(SC_R(:,:,ik,1),SigmaXC,SC_bands,1)
      forall (iv=E%nbf+1:X%ib(2),ic=1:E%nbf) SigmaXC(iv,ic,1)= (0.,0.)
    else
     do i1 = 1,QP_n_states
       iv = QP_table(i1,1)
       ic = QP_table(i1,2)
       if (QP_table(i1,3)/=ik.or.iv>E%nbf.or.ic<=E%nbf) cycle
       SigmaXC(iv,ic,1) = QP_Vnl_xc(i1)
       if (l_sc_srpa) SigmaXC(iv,ic,1) = SigmaXC(iv,ic,1)+ QP_Sc(i1,1) 
     enddo
   endif
   ! 3:
   do ic = E%nbf+1, X%ib(2)
     do iv = 1 ,E%nbf
       isc%is = (/iv,ik,1,1/)
       isc%os = (/ic,ik,1,1/)
       isc%qs = (/1,1,1/)
       !
       call scatterBamp(isc)
       rhotw_save(:,1) = isc%rhotw(:)
       if (i_time_rev==0.and.i_space_inv==0) then
         isc%is = (/ic,ik,1,1/)
         isc%os = (/iv,ik,1,1/)
         isc%qs = (/1,1,1/)
         call scatterBamp(isc)
         rhotw_save(:,2) = isc%rhotw(:)
       endif
       !
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
           isc%rhotw(ig1) = rhotw_save(ig2,1)
         enddo
         if (l_is_TR) then
           forall(ig1=2:X%ng) rhotw_sum_TR(ig1-1)=rhotw_sum_TR(ig1-1)+isc%rhotw(ig1)
         else
           forall(ig1=2:X%ng) rhotw_sum(ig1-1)=rhotw_sum(ig1-1)+conjg(isc%rhotw(ig1))
         endif
         !
         if (i_time_rev==0.and.i_space_inv==0) then
           !
           do ig1=1,X%ng
             ig2 = g_rot(sop_inv(is),ig1)
             isc%rhotw(ig1) = rhotw_save(ig2,2)
           enddo
           forall(ig1=2:X%ng) rhotw_sum_TR(ig1-1)=rhotw_sum_TR(ig1-1)+conjg(isc%rhotw(ig1))
         endif
         !
       end do
       ! 4:
       rho_xc(:) =  rho_xc(:) + E%f(iv,ik,1)*(2.-E%f(ic,ik,1))/2.*&
&                  (rhotw_sum(:)*SigmaXC(iv,ic,1)+rhotw_sum_TR(:)*conjg(SigmaXC(iv,ic,1)))/&
&                  (E%E(iv,ik,1)-E%E(ic,ik,1)) 
       !
     end do
   end do
 end do 
 deallocate(isc%rhotw,rhotw_save,rhotw_sum,rhotw_sum_TR,SigmaXC)
 call X_alloc('OptOsc')
 if (allocated(QP_Vnl_xc))  deallocate(QP_Vnl_xc)
 !
 ! Add c.c. contribution
 !
 if (i_time_rev/=0) then
   rho_xc=2.*rho_xc
 else if (i_space_inv/=0) then
   rho_xc=rho_xc+conjg(rho_xc)
 endif
 allocate(Voep_RL(X%ng-1))
 !
 ! Calculate Voep_RL
 !
 Voep_RL=matmul(Xo_inverse,rho_xc)
 deallocate(rho_xc)
 !
 ! Calculate Voep (DL)
 !
 allocate(Vx_dp(fft_size))
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
 deallocate(Voep_RL,Vx_dp)
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
end subroutine V_oep

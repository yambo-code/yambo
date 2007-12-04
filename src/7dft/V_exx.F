!
! Copyright (C) 2000-2005 M. Gruening and the SELF team
!         http://www.fisica.uniroma2.it/~self
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
  use pars,            ONLY:pi,SP,DP,HARTREE
  use D_lattice,       ONLY:DL_vol,i_time_rev,sop_inv,nsym
  use X_m,             ONLY:X_t, X_reset,X_alloc,X_mat,X_rhoq0
  use electrons,       ONLY:levels
  use R_lattice,       ONLY:bz_samp,g_rot,nkibz,nkbz,ng_vec
  use FFT_m,           ONLY:fft_size,fft_g_table,fft_dim,fftw_plan
  use frequency,       ONLY:w_samp,W_reset
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
  ! Workspace:
  !
  integer                  :: i1,ik,ik1,ic,iv,isym,g1,g2
  complex(SP), allocatable :: Xo_inverse(:,:), SigmaX(:,:),rhotw_save(:),&
&                             rho_w(:,:),rho_x(:),Vexx_RL(:)
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
  call freqs_setup(Xw)
  ! 2:
  call X_alloc('X',(/X%ng,X%ng,Xw%n(2)/))
  call X_os(X_mat,1,(/1,Xw%n(1)/),E,k,Xw,X)
  ! 3:
  allocate(Xo_inverse(X%ng-1,X%ng-1))
  forall(i1 = 2:X%ng)  Xo_inverse(i1-1,1:X%ng-1) =&
&                      X_mat(i1,2:X%ng,1)
  call X_alloc('X')
  ! 4:
  call mat_dia_inv(INV,USE_LK,Xo_inverse)
  !
  ! Calculate rho_x(G) =>  1 - calculate SigmaX_{v,c}(k)
  !                        2 - initialize vars/dim
  !                        for each k in IBZ:
  !                        3 - retabulate SigmaX elements
  !                        for each c,v: 
  !                        4 - calculate  rho_w (G) = sum{R_k} rho_{c,v}(R_k,G)
  !                        5 - accumulate rho_x (I use TR!)
  ! -----------------------------------------------
  ! NB: a. it does not work for metallic systems
  !     b. it does not work for systems without TR (check out what happen with SI!)
  ! -----------------------------------------------
  ! 1:
  allocate(QP_Sx(QP_n_states))
  call mem_est("QP_Sx",(/QP_n_states/))
  call QP_XX_Vxc(E,k,k,q,.FALSE.,.TRUE.) 
  ! 2:
  call collision_reset(isc)
  isc%ngrho=X%ng
  allocate(isc%rhotw(X%ng),rhotw_save(X%ng),rho_w(X%ng,2),rho_x(X%ng))
  allocate(SigmaX(E%nbf,X%ib(2)))
  ! 3:
  rho_x = (0.,0.)
  do ik = 1, nkibz
    SigmaX = (0.,0.)
    do i1 = 1,QP_n_states
      iv = QP_table(i1,1)
      ic = QP_table(i1,2)
      if (QP_table(i1,3).ne.ik) cycle
      if (iv >  E%nbf) cycle 
      if (ic <= E%nbf) cycle
      SigmaX(iv,ic) = QP_Sx(i1)
      print*, iv, ic, ik, SigmaX(iv,ic)*hartree 
    end do
    ! 4:
    do ic = E%nbf+1, X%ib(2)
      do iv = 1 ,E%nbf
        isc%is = (/iv,ik,1,1/)
        isc%os = (/ic,ik,1,1/)
        isc%qs = (/1,1,1/)
        call scatterBamp(isc)
        rhotw_save = isc%rhotw
        rho_w = (0.,0.)
        do ik1 = 1,k%nstar(ik)
          isym = k%star(ik,ik1)
          do g1=1,X%ng
            g2 = g_rot(sop_inv(isym),g1)
            isc%rhotw(g1) = rhotw_save(g2)
          enddo
          isc%rhotw(1) = X_rhoq0(ic,iv,ik1,1)
          if (isym>nsym/(i_time_rev+1)) then
            rho_w(2:,2)= rho_w(2:,2) + isc%rhotw(2:)
            rho_w(1,2) = rho_w(1,2) + conjg(isc%rhotw(1)) 
          else
            rho_w(:,1)= rho_w(:,1)+ conjg(isc%rhotw(:))
          end if
        end do
        ! 5:
        rho_x(:) =  rho_x(:) + E%f(iv,ik,1)*(2.-E%f(ic,ik,1))*&
               & (rho_w(:,1)* SigmaX(iv,ic)+rho_w(:,2)* conjg(SigmaX(iv,ic)))&
               &/(E%E(iv,ik,1)-E%E(ic,ik,1))    
      end do
    end do
  end do 
  deallocate(isc%rhotw,rhotw_save,rho_w,SigmaX)
  call X_alloc('OptOsc')
  if (allocated(QP_Sx))  deallocate(QP_Sx)
  !
  ! Calculate Vexx_RL
  !
  allocate(Vexx_RL(X%ng))
   Vexx_RL = (0.,0.)
  do g1= 2, X%ng
    Vexx_RL(2:X%ng)=Vexx_RL(2:X%ng)+Xo_inverse(1:X%ng-1,g1-1)*rho_x(g1)
  end do
  !
  deallocate(rho_x)
  !
  ! Calculate Vexx (DL)
  !
  allocate(Vx_dp(fft_size))
  Vx_dp = (0.,0.)
  Vx_dp(fft_g_table(1:X%ng,1))= Vexx_RL(1:X%ng)/(DL_vol*float(nkbz)) 
#if defined FFTW
  fftw_plan = 0
  call fft_3d(Vx_dp,fft_dim,1,fftw_plan)
  fftw_plan = 0
#else
  call fft_3d(Vx_dp,fft_dim,1)
#endif
  !
  Vexx = real(Vx_dp)
  !
  deallocate(Vexx_RL,Vx_dp)
  !
end subroutine V_exx


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
subroutine V_exx(X,E,k,Vexx)
  !
  !  Exact exchange potential (vexx in direct space)
  !
  !  Found in reciprocal space as:
  !  vexx_RL(G) = Xo^{-1}(G,G')rho_{xc}(G)
  !  
  !  where: 
  !  rho_xc(G) = int_k sum_{c,v} ( rho_{v,c}(k,G)   SigmaX_{v,c}(k)   +  
  !                           + rho^*_{v,c}(k,G) SigmaX^*_{v,c}(k))\
  !                           (E_{vk} - E_{ck})
  !
  use pars,            ONLY: pi,SP,DP
  use D_lattice,       ONLY: DL_vol
  use X_m,             ONLY: X_t, X_reset, X_alloc, X_mat
  use electrons,       ONLY:levels
  use R_lattice,       ONLY:bz_samp
  use FFT_m,           ONLY:fft_size
  use frequency,       ONLY: w_samp,W_reset
  use matrix_operate,  ONLY:mat_dia_inv,INV,USE_LK
  !
  ! I/O
  !
  implicit none
  real(SP) :: Vexx(fft_size)
  type(X_t)    :: X
  type(levels) :: E
  type(bz_samp):: k
  !
  ! Workspace:
  !
  integer                  :: i1,i2 
  complex(SP), allocatable :: Xo_inverse(:,:)
  type(w_samp)             :: Xw
  !
  ! Calculate Xo^{-1}(G,G'): 1- initialize vars/dim
  !                          2- calculate Xo, 
  !                          3- delete wings,   
  !                          4- invert
  ! 1:
  call W_reset(Xw)
  Xw%n = 1
  call freqs_setup(Xw)
  ! 2:
  call X_alloc('X',(/X%ng,X%ng,Xw%n(2)/))
  call X_os(X_mat,1,(/1,Xw%n(1)/),E,k,Xw,X)
  !print*, k%sstar
  print*,fft_size
  print*, X_mat
  stop
  ! 3:
  allocate(Xo_inverse(X%ng-1,X%ng-1))
  forall(i1 = 2:X%ng)  Xo_inverse(i1-1,1:X%ng-1) =&
&                      X_mat(i1,2:X%ng,1)
  !
  call X_alloc('X')
  ! 4:
  call mat_dia_inv(INV,USE_LK,Xo_inverse)
  !
  ! Calculate rho_xc(G)
  !
  
  !
  ! Calculate vexx_RL
  !

  !
  ! Calculate vexx_DL
  !
end subroutine V_exx


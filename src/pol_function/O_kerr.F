!
! Copyright (C) 2000-2012 D. Sangalli and the YAMBO team 
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
subroutine O_kerr(iq,i_G_shift,Q_plus_G_sq_modulus,frequencies_range,Xen,Xk,wv,X)
 !
 ! This subroutine compute X_xx, X_xy and X_yy. I do NOT need X_yx 
 ! From these the kerr parameters are computed. 
 ! To these end the following assumptions are used:
 ! 1 - the system has cubic symmetry
 ! 2 - the magnetization is along the z axis
 !
 ! From these assumptions the epsilon has the following structure:
 !          (  eps_xx   eps_xy    0     )
 !   eps =  ( -eps_xy   eps_xx    0     )
 !          (    0        0     eps_zz  )
 !
 ! Then in G space we have that X_yy(G,G') = X_xx(G_rot, G_rot' ) where G_rot is respect to 
 ! the symmetry (0. -1. 0. ; 1. 0. 0. ; 0. 0. 1.), x -> -y ; y -> x ; z -> z
 !
 !
 use pars,            ONLY:pi,SP
 use units,           ONLY:DEG2RAD
 use com,             ONLY:msg,error
 use matrix_operate,  ONLY:m3inv_c
 use X_m,             ONLY:X_t,X_mat,X_epsilon,X_alloc
 use KERR,            ONLY:X_head,Xo_head,Xo_wing_R,Xo_wing_L
 use frequency,       ONLY:w_samp
 use electrons,       ONLY:levels
 use R_lattice,       ONLY:bz_samp,bare_qpg,q_norm
 use TDDFT,           ONLY:FXC_n_g_corr,FXC,FXC_LRC_alpha,FXC_LRC_beta
 use drivers,         ONLY:l_alda_fxc,l_lrc_fxc
 use timing,          ONLY:live_timing
 use D_lattice,       ONLY:nsym,sop_inv
 use R_lattice,       ONLY:g_rot,rl_sop
 !
 implicit none
 !
 type(levels)         :: Xen
 type(bz_samp)        :: Xk
 type(w_samp)         :: wv
 type(X_t)            :: X
 !
 integer              :: frequencies_range(2)
 integer              :: i_G_shift
 real(SP)             :: Q_plus_G_sq_modulus
 !
 ! Dummies and local
 integer              :: i1,ig,igp,ig_rot,igp_rot,i_sym,iq,iw
 integer              :: kerr_sym_xy_index,kerr_sop_xy(3,3)
 !
 complex(SP)          :: eps_tens(3,3,wv%n(2)),eps_m1_tens(3,3,wv%n(2))
 !
 if(i_G_shift /= 1) call error('i_G_shift different from 1 not implemented')
 !
 ! (1) COMPUTE \eps^-1
 !======================================================
 !
 ! Xo
 !
 call X_os(X_mat,iq,frequencies_range,Xen,Xk,wv,X)
 !
 Xo_head(1,:)  = X_mat(1,1,:)   ! X_xx
 !                              ! X_xy computed in X_os.F
 Xo_head(3,:)  = cmplx(0.,0.)   ! X_xz
 Xo_head(4,:)  =-Xo_head(2,:)   ! X_yx
 Xo_head(5,:)  = X_mat(1,1,:)   ! X_yy
 Xo_head(6:8,:)= cmplx(0.,0.)   ! X_yz, X_zx, X_zy
 Xo_head(9,:)  = X_mat(1,1,:)   ! X_zz
 !
 if(X%ng>1) then
   Xo_wing_R(1,:,:) = X_mat(1 ,2:,:)  ! XWRx
   Xo_wing_L(1,:,:) = X_mat(2:,1 ,:)  ! XWLx
   !                                  ! XWRy  }  
   !                                  ! XWLy  }  computed in X_os.F
   Xo_wing_R(3,:,:) = cmplx(0.,0.)    ! XWRx = 0.
   Xo_wing_L(3,:,:) = cmplx(0.,0.)    ! XWRx = 0.
 endif
 !
 ! X Dyson solver
 !
 call X_s(iq,frequencies_range,X,wv)
 !
 call msg('nrs','Computation of the Kerr parameters')
 !
 ! eps without LF
 !
 X_epsilon(1,frequencies_range(1):frequencies_range(2))=&
&                       1.0_SP-Xo_head(1,:)*4.0_SP*pi/Q_plus_G_sq_modulus
 !
 X_epsilon(3,frequencies_range(1):frequencies_range(2))=&
&                             -Xo_head(2,:)*4.0_SP*pi/Q_plus_G_sq_modulus
 !
 !
 ! (2) Invert the dielectric tensor
 !======================================================
 !
 !         ( em1_xx   em1_xy    0    )
 !  em1 =  (-em1_xy   em1_xx    0    )
 !         (   0        0     em1_xx )
 !
 eps_tens=(0.,0.)
 eps_m1_tens=(0.,0.)
 !
 eps_m1_tens(1,1,:)=1.0_SP+X_head(1,:)*4.*pi/Q_plus_G_sq_modulus
 eps_m1_tens(1,2,:)=       X_head(2,:)*4.*pi/Q_plus_G_sq_modulus
 !
 eps_m1_tens(2,2,:)= eps_m1_tens(1,1,:)
 eps_m1_tens(3,3,:)= eps_m1_tens(1,1,:)
 eps_m1_tens(2,1,:)=-eps_m1_tens(1,2,:)
 !
 do iw=1,wv%n(2)
   call m3inv_c(eps_m1_tens(:,:,iw),eps_tens(:,:,iw))
 enddo
 !
 ! eps with LF
 !
 X_epsilon(2,frequencies_range(1):frequencies_range(2))=eps_tens(1,1,:)           ! xx
 !
 X_epsilon(4,frequencies_range(1):frequencies_range(2))=eps_tens(1,2,:)           ! xy
 !
 ! KERR parameters
 !
 do i1=1,2
   X_epsilon(i1+4,frequencies_range(1):frequencies_range(2))=                                &
&         -X_epsilon(i1+2,frequencies_range(1):frequencies_range(2))/                        &
&              ( (X_epsilon(i1,frequencies_range(1):frequencies_range(2))-1.0_SP)*           &
&                 sqrt(X_epsilon(i1,frequencies_range(1):frequencies_range(2))) ) /DEG2RAD
 enddo
 !
end subroutine 

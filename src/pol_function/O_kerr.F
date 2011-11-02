!
! Copyright (C) 2000-2011 D. Sangalli and the YAMBO team 
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
 use pars,            ONLY:pi,SP
 use units,           ONLY:DEG2RAD
 use com,             ONLY:msg,error
 use matrix_operate,  ONLY:m3inv_c
 use X_m,             ONLY:X_t,X_mat,X_epsilon,X_alloc
 use KERR,            ONLY:X_mat_head,X_mat_wing_right,X_mat_wing_left
 use frequency,       ONLY:w_samp
 use electrons,       ONLY:levels
 use R_lattice,       ONLY:bz_samp,bare_qpg
 !
 implicit none
 !
 type(levels)         :: Xen
 type(bz_samp)        :: Xk
 type(w_samp)         :: wv
 type(X_t)            :: X
 !
 integer              :: i1,i2,i3,iq,iw,frequencies_range(2)
 integer              :: i_G_shift
 real(SP)             :: Q_plus_G_sq_modulus
 !
 complex(SP)          :: Xo_tmp_w(X%ng,X%ng,wv%n(2))
 complex(SP)          :: X_mat_tmp_w(X%ng,X%ng,wv%n(2))
 complex(SP)          :: Xo_tmp(X%ng,X%ng)
 complex(SP)          :: X_mat_tmp(X%ng,X%ng)
 !
 complex(SP)          :: eps_tens(3,3,wv%n(2)),eps_m1_tens(3,3,wv%n(2))
 !
 ! DEBUG <
 complex(SP)          :: eps0_tens(3,3,wv%n(2)),eps0_m1_tens(3,3,wv%n(2))
 real(SP)             :: M_tmp(3,3)
 ! DEBUG >
 !
 if(i_G_shift /= 1) call error('i_G_shift different from 1 not implemented')
 !
 ! (1) DIAGONAL COMPONENT OF THE DIELECTRIC TENSOR
 !======================================================
 !
 call X_os(X_mat,iq,frequencies_range,Xen,Xk,wv,X)
 !
 Xo_tmp_w(:,:,:)=X_mat(:,:,:)                                           ! Xo_xx
 !
 ! Without LF eps/alpha
 !
 X_epsilon(1,frequencies_range(1):frequencies_range(2))=&
&                       1.0_SP-X_mat(i_G_shift,i_G_shift,:)*4.0_SP*pi/Q_plus_G_sq_modulus
 !
 ! X Dyson equation solver
 !
 ! X_xx = Xo_xx + Xo_xx*v*X_xx + Xo_xy*v*X_yx 
 !        the term Xo_xy*v*X_yx is neglected in X_s
 ! X_xx = (1-Xo_xx*v)^-1 * Xo_xx
 ! X_s computes the term (1-Xo_xx*v)^-1
 !
 call msg('nrs','Computation of the Kerr parameters')
 call msg('nrs','(1) Dyson equation for the diagonal component of epsilon')
 call X_s(iq,frequencies_range,X,wv)
 !
 X_mat_tmp_w=X_mat                                                      ! (1-Xo_xx*v)^-1
 do iw=1,wv%n(2)
   X_mat(:,:,iw)=matmul(X_mat_tmp_w(:,:,iw),Xo_tmp_w(:,:,iw))
   forall(i1=1:X%ng,i2=1:X%ng) X_mat(i1,i2,iw)=X_mat(i1,i2,iw)*4.*pi/bare_qpg(iq,i1)/bare_qpg(iq,i2)
 enddo
 !
 ! Use X_epsilon(2) as tmp vector for eps_m1_xx
 X_epsilon(2,frequencies_range(1):frequencies_range(2))=&
&          (X_mat(i_G_shift,i_G_shift,:)*bare_qpg(iq,1)**2/Q_plus_G_sq_modulus+1.)
 !
 !
 ! (2) OFF-DIAGONAL COMPONENT OF THE DIELECTRIC TENSOR
 !======================================================
 !
 X_epsilon(3,frequencies_range(1):frequencies_range(2))=&
&                        -X_mat_head(:)*4.0_SP*pi/Q_plus_G_sq_modulus
 !
 ! X Dyson equation solver
 !
 ! X_xy = (1-Xo_xx*v)^-1 * Xo_xy * (1. + v*X_xx )
 ! X_s computes the term (1-Xo_xx*v)^-1
 !
 call msg('nrs','(2) Dyson equation for the off-diagonal component of epsilon')
 !
 forall(i1=1:X%ng,i2=1:X%ng,iw=1:wv%n(2)) X_mat(i1,i2,iw)=&
&                X_mat(i1,i2,iw)/4./pi*bare_qpg(iq,i1)*bare_qpg(iq,i2)  ! X_xx
 !
 Xo_tmp_w(1,1,:)=X_mat_head(:)                                          ! Xo_xy (head)
 !
 do iw=1,wv%n(2)
   !
   if(X%ng>1) then                                                      ! Xo_xy (wings too)
     Xo_tmp_w(1,2:,iw)=X_mat_wing_right(:,iw)
     Xo_tmp_w(2:,1,iw)=X_mat_wing_left(:,iw)
   endif
   !
   Xo_tmp(:,:)=Xo_tmp_w(:,:,iw)                                         ! Xo_xy
   !
   X_mat_tmp(:,:)=matmul(X_mat_tmp_w(:,:,iw),Xo_tmp)                    ! (1-Xo_xx*v)^-1 * Xo_xy
   !
   do i1=1,X%ng 
     X_mat(:,i1,iw)=4.*pi/bare_qpg(iq,i1)**2*X_mat(i1,:,iw)             ! v * X_xx
   enddo
   forall(i1=1:X%ng)  X_mat_tmp_w(i1,i1,iw)=X_mat(i1,i1,iw)+1.          ! 1. + v * X_xx
   !
   X_mat(:,:,iw)=matmul(X_mat_tmp(:,:),X_mat_tmp_w(:,:,iw))             ! ( (1-Xo_xx*v)^-1* Xo_xy) *  (1. + X_xx )
   !
   ! This last step is duplicated from X_s.F
   forall(i1=1:X%ng,i2=1:X%ng) X_mat(i1,i2,iw)=X_mat(i1,i2,iw)*4.*pi/bare_qpg(iq,i1)/bare_qpg(iq,i2)
   !
 enddo
 !
 ! Use X_epsilon(4) as tmp vector for eps_m1_xy
 X_epsilon(4,frequencies_range(1):frequencies_range(2))=&
&          X_mat(1,1,:)*bare_qpg(iq,1)**2/Q_plus_G_sq_modulus
 !
 !
 ! (3) CONSTRUCT AND INVERT THE DIELECTRIC TENSOR
 !======================================================
 !
 !         ( em1_xx   em1_xy    0    )
 !  em1 =  (-em1_xy   em1_xx    0    )
 !         (   0        0     em1_xx )
 !
 eps_tens=(0.,0.)
 eps_m1_tens=(0.,0.)
 !
 eps_m1_tens(1,1,:)=X_epsilon(2,frequencies_range(1):frequencies_range(2))
 eps_m1_tens(1,2,:)=X_epsilon(4,frequencies_range(1):frequencies_range(2))
 !
 eps_m1_tens(2,2,:)= eps_m1_tens(1,1,:)
 eps_m1_tens(3,3,:)= eps_m1_tens(1,1,:)
 eps_m1_tens(2,1,:)=-eps_m1_tens(1,2,:)            !conjg(eps_m1_tens(1,2,:))
 !
 do iw=1,wv%n(2)
   call m3inv_c(eps_m1_tens(:,:,iw),eps_tens(:,:,iw))
 enddo
 !
 ! DEBUG <
 eps0_tens=(0.,0.)
 eps0_tens=(0.,0.)
 !
 eps0_tens(1,1,:)=X_epsilon(1,frequencies_range(1):frequencies_range(2))
 eps0_tens(1,2,:)=X_epsilon(3,frequencies_range(1):frequencies_range(2))
 !
 eps0_tens(2,2,:)= eps0_tens(1,1,:)
 eps0_tens(3,3,:)= eps0_tens(1,1,:)
 eps0_tens(2,1,:)=-eps0_tens(1,2,:)                !conjg(eps0_tens(1,2,:))
 !
 do iw=1,wv%n(2)
   call m3inv_c(eps0_tens(:,:,iw),eps0_m1_tens(:,:,iw))
 enddo
 !
 write(*,*) 'eps0= '
 do i1=1,3
   write(*,*) eps0_tens(i1,:,1)
 enddo
 !
 write(*,*) 'eps0_m1= '
 do i1=1,3
   write(*,*) eps0_m1_tens(i1,:,1)
 enddo
 !
 write(*,*) 'eps_m1= '
 do i1=1,3
   write(*,*) eps_m1_tens(i1,:,1)
 enddo
 !
 write(*,*) 'eps= '
 do i1=1,3
   write(*,*) eps_tens(i1,:,1)
 enddo
 ! DEBUG >
 !
 X_epsilon(1,frequencies_range(1):frequencies_range(2))=eps0_tens(1,1,:)          ! xx
 X_epsilon(3,frequencies_range(1):frequencies_range(2))=eps0_tens(1,2,:)          ! xy
 !
 X_epsilon(2,frequencies_range(1):frequencies_range(2))=eps_tens(1,1,:)           ! xx
 X_epsilon(4,frequencies_range(1):frequencies_range(2))=eps_tens(1,2,:)           ! xy
 !
 do i1=1,2
   X_epsilon(i1+4,frequencies_range(1):frequencies_range(2))=-X_epsilon(i1+2,frequencies_range(1):frequencies_range(2))/ &
&              ( (X_epsilon(i1,frequencies_range(1):frequencies_range(2))-1.0_SP)*                                       &
&                 sqrt(X_epsilon(i1,frequencies_range(1):frequencies_range(2))) ) /DEG2RAD
 enddo
 !
end subroutine 

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
 use com,             ONLY:msg
 use matrix_operate,  ONLY:m3inv
 use X_m,             ONLY:X_t,X_mat,X_epsilon,X_alloc
 use KERR,            ONLY:X_mat_wing_right,X_mat_wing_left
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
 integer              :: i1,iq,iw,frequencies_range(2)
 integer              :: i_G_shift
 real(SP)             :: Q_plus_G_sq_modulus
 !
 complex(SP)          :: Xo_tmp(X%ng,X%ng,wv%n(2))
 !
 complex(SP)          :: eps_tens(3,3,wv%n(2)),eps_m1_tens(3,3,wv%n(2))
 real(SP)             :: M1(3,3),M2(3,3)
 !
 eps_tens=(0.,0.)
 eps_m1_tens=(0.,0.)
 !
 call X_os(X_mat,iq,frequencies_range,Xen,Xk,wv,X)
 !
 Xo_tmp(:,:,:)=X_mat(:,:,:)
 !
 ! Without LF eps/alpha
 !
 X_epsilon(1,frequencies_range(1):frequencies_range(2))=&
&                       1.0_SP-X_mat(i_G_shift,i_G_shift,:)*4.0_SP*pi/Q_plus_G_sq_modulus
 !
 ! X Dyson equation solver
 !
 call msg('nrs','Computation of the Kerr parameters')
 call msg('nrs','(1) Dyson equation for the diagonal component of epsilon')
 call X_s(iq,frequencies_range,X,wv)
 !
 eps_m1_tens(1,1,1:wv%n(2))=(X_mat(i_G_shift,i_G_shift,:)*&
&                              bare_qpg(iq,1)**2/Q_plus_G_sq_modulus+1.)
 !
 !   Then the off-diagonal component
 !
 X_mat(:,:,:)=Xo_tmp(:,:,:)
 !
! do iw=1,wv%n(2)
!   X_mat(1,:,iw)=X_mat_wing_right(:,iw)
!   X_mat(:,1,iw)=X_mat_wing_left(:,iw)
! enddo
! X_mat(1,1,:)=X_mat_wing_right(1,:)
 X_mat(1,1,:)=X_mat_wing_right(1,:)
 !
 X_epsilon(3,frequencies_range(1):frequencies_range(2))=&
&                        -X_mat(i_G_shift,i_G_shift,:)*4.0_SP*pi/Q_plus_G_sq_modulus
 !
 ! X Dyson equation solver
 !
 call msg('nrs','(2) Dyson equation for the off-diagonal component of epsilon')
 call X_s(iq,frequencies_range,X,wv)
 !
 eps_m1_tens(1,2,1:wv%n(2))=(X_mat(i_G_shift,i_G_shift,:)*&
&                              bare_qpg(iq,1)**2/Q_plus_G_sq_modulus)
 !
 !
 eps_m1_tens(1,1,:)=eps_m1_tens(1,1,:)
 eps_m1_tens(2,2,:)=eps_m1_tens(1,1,:)
 eps_m1_tens(3,3,:)=eps_m1_tens(1,1,:)
 eps_m1_tens(2,1,:)=conjg(eps_m1_tens(1,2,:))
 !
 do iw=1,wv%n(2)
   M1=10.**6*real(eps_m1_tens(:,:,iw))
   M2=10.**6*aimag(eps_m1_tens(:,:,iw))
   ! DEBUG <
   !write(*,*) ' '
   !write(*,*) 'M1'
   !do i1=1,3
   !  write(*,*) M1(i1,:)
   !enddo
   !write(*,*) 'M2'
   !do i1=1,3
   !  write(*,*) M2(i1,:)
   !enddo
   ! DEBUG >
   call m3inv(M1)
   call m3inv(M2)
   ! DEBUG <
   !write(*,*) ' '
   !write(*,*) 'M1'
   !do i1=1,3
   !  write(*,*) M1(i1,:)
   !enddo
   !write(*,*) 'M2_inv'
   !do i1=1,3
   !  write(*,*) M2(i1,:)
   !enddo
   ! DEBUG >
   eps_tens(:,:,iw)=(M1(:,:)+(0.,1.)*M2(:,:))/10.**6
 enddo
 !
 X_epsilon(2,frequencies_range(1):frequencies_range(2))=1./eps_m1_tens(1,1,:)   ! xx
! X_epsilon(2,frequencies_range(1):frequencies_range(2))=eps_tens(1,1,:)        ! xx
 X_epsilon(4,frequencies_range(1):frequencies_range(2))=eps_tens(1,2,:)         ! xy
 !
 do i1=1,2
   X_epsilon(i1+4,frequencies_range(1):frequencies_range(2))=-X_epsilon(i1+2,frequencies_range(1):frequencies_range(2))/ &
&              ( (X_epsilon(i1,frequencies_range(1):frequencies_range(2))-1.0_SP)*                                       &
&                 sqrt(X_epsilon(i1,frequencies_range(1):frequencies_range(2))) ) /DEG2RAD
 enddo
 !
end subroutine 

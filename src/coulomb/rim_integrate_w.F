!
! License-Identifier: GPL
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Copyright (C) 2006 The Yambo Team
!
#include<dev_defs.h>
!
! Authors (see AUTHORS file for details): AM AG GS AF NS
!
! This work has been developed by Giacomo Sesti et al.
! Please cite this work arXiv:2508.06930:https://arxiv.org/abs/2508.06930
!
! AF: This file needs cleanup
!     - proper indentation
!     - explicit handling of precision (e.g. 2.0_SP instead of 2.0)
!     - too long, splitting ??
!
subroutine rim_integrate_w(iq,qr,qr_d,N_out,em1_anis,Xw)
 !
 use pars,          ONLY:SP,DP,pi,zero_dfl
 use com,           ONLY:msg
 use vec_operate,   ONLY:c2a,iku_v_norm
 use R_lattice,     ONLY:g_vec,RIM_n_rand_pts,k_grid_b,k_grid_uc_vol,q0_def_norm,&
&                        RIM_W_ng,q_pt,RIM_W_is_diagonal,idir,RIM_W,RIM_W_d,&
&                        aux_RIM_W,f_coeff,DEV_VAR(f_coeff),cut_is_slab,RIM_id_epsm1_reference,rimw_type
 use D_lattice,     ONLY:alat
 use frequency,     ONLY:w_samp
 use gpu_m,         ONLY:have_gpu
 use devxlib,       ONLY:devxlib_memcpy_d2h,devxlib_memcpy_h2d
 !
#include<y_memory.h>
 !
 integer      :: iq,N_out
 real(SP)     :: em1_anis(3),qtmp(3),qr_rlu(3)
 real(SP)     :: qr(3,RIM_n_rand_pts)
 real(SP) DEV_ATTR :: qr_d(3,RIM_n_rand_pts)
 real(SP)     :: v1(3),q_grid_b_rlu(3,3),q_grid_b_iku(3,3)
 type(w_samp) :: Xw
 !
 ! Work Space
 !
 integer    :: i1,i1min,i2,i2min,i2max,i3,iw,j,s1,s2
 real(DP), allocatable :: vslab(:,:)
 real(DP), allocatable DEV_ATTR :: vslab_d(:,:)
 real(DP)   :: r1,rfac
 complex(DP):: RIM_acc,RIM_acc_anis,func
 real(SP)   :: slab_vz1,slab_vplane1,lcut,pre_factor,anis_fact,vslab2
 !
 ! define pointers to enable CUF kernels
 ! when compiling using CUDA-Fortran
 !
 complex(DP), pointer DEV_ATTR :: f_coeff_p(:,:,:,:,:)
 !
 ! AF: f_coeff_p needs to be present on GPU memory
 f_coeff_p => DEV_VAR(f_coeff)
 !
 call msg("r","start rimw integration")
 call msg("r", trim(rimw_type))
 !
 rfac=8._SP*k_grid_uc_vol/real(N_out)/(2._SP*pi)**4
 !
 if (cut_is_slab) then
   lcut=alat(idir(1))/2._SP
 ! else
 !  call error('RIM-W without cutoff slab has not been implemented')
 end if
 !
 !Load  q_grid_b
 do j=1,3
   v1 = k_grid_b(j,:)
   call c2a(v_in=v1,mode='kc2a')
   !
   if ((abs(v1(2))<zero_dfl).and.(abs(v1(3))<zero_dfl)) then
     !
     q_grid_b_rlu(:,1) = abs(v1)
     !
   elseif((abs(v1(1))< zero_dfl).and.(abs(v1(3))< zero_dfl)) then
     !
     q_grid_b_rlu(:,2) = abs(v1)
     !
   elseif((abs(v1(1))< zero_dfl).and.(abs(v1(2))< zero_dfl)) then
     !
     q_grid_b_rlu(:,3) = abs(v1)
     !
   else
     call error('Error in loading q_min')
   endif
   !
 enddo
 !
 ! get q_grid_b in iku
 do j=1,3
   call c2a(v_in=q_grid_b_rlu(:,j),v_out=q_grid_b_iku(:,j),mode='ka2i')
 enddo
 !
 ! Evaluate V (we take into account whether truncation applied or not)
 !
 i1min=1
 YAMBO_ALLOC(vslab,(RIM_n_rand_pts,RIM_W_ng))
 !
 if (have_gpu) then
  !
  YAMBO_ALLOC_GPU(DEV_VAR(vslab),(RIM_n_rand_pts,RIM_W_ng))
  !
 endif
 !
 !
 !$omp parallel do default(shared), private(i1,i2,slab_vz1,slab_vplane1,pre_factor), &
 !$omp &           collapse(2)
 !
 do i1=i1min,RIM_W_ng
   !
   do i2=1,RIM_n_rand_pts
     !
     if (cut_is_slab) then
       !
       ! System 2D, prefactor of slab truncation
       !
       !r1=iku_v_norm(qpGr+q_sampl)*iku_v_norm(qpGc+q_sampl)
       !kz
       slab_vz1=(qr(idir(1),i2)+q_pt(iq,idir(1))+g_vec(i1,idir(1)))*2._SP*pi/alat(idir(1))
       !kxy
       slab_vplane1=sqrt(((qr(idir(2),i2)+q_pt(iq,idir(2))+g_vec(i1,idir(2)))*2._SP*pi/ &
&                         alat(idir(2)))**2 + ((qr(idir(3),i2)+q_pt(iq,idir(3))      &
&                        +g_vec(i1,idir(3)))*2._SP*pi/alat(idir(3)))**2)
       !
       if (slab_vplane1 < q0_def_norm) slab_vplane1 = q0_def_norm
       pre_factor = 1.-exp(-slab_vplane1*lcut)*cos(slab_vz1*lcut)
       !
       vslab(i2,i1) =sqrt(4._SP*pi*pre_factor/(slab_vplane1**2+slab_vz1**2))
       !
     else
       !
       ! System 3D, standard bare coulomb
       !
       pre_factor=1._SP
       !
       slab_vplane1=sqrt(((qr(1,i2)+q_pt(iq,1)+g_vec(i1,1))*2._SP*pi/alat(1))**2 + &
&                        ((qr(2,i2)+q_pt(iq,2)+g_vec(i1,2))*2._SP*pi/alat(2))**2 + &
&                        ((qr(3,i2)+q_pt(iq,3)+g_vec(i1,3))*2._SP*pi/alat(3))**2)
       !
       vslab(i2,i1) = sqrt(4._SP*pi*pre_factor/slab_vplane1**2)
       !
     end if
   enddo
   !
 enddo
 !$omp end parallel do
 !
 if (have_gpu) call devxlib_memcpy_h2d(DEV_VAR(vslab),vslab)
 !
 if (RIM_W_is_diagonal) then
  i2max=i1
  i2min=i1
 else
  i2min=i1min
  i2max=RIM_W_ng
 end if
 !
 !
 do iw=1,Xw%n_freqs
   !
   !DEV_OMP parallel do default(shared), private(i1,i2,i3,func,RIM_acc), &
   !DEV_OMP & collapse(2)
   !
   do i1=i1min,RIM_W_ng
     do i2=i2min,i2max
       !
       RIM_acc=0.0_DP
       !
       !DEV_CUF kernel do(1) <<<*,*>>>
       do i3=1,RIM_n_rand_pts
         !
         func = f_coeff_p(1,i1,i2,iq,iw)                                                                 &
&             + DEV_VAR(qr)(1,i3)*(f_coeff_p(2,i1,i2,iq,iw)+DEV_VAR(qr)(1,i3)*f_coeff_p(5,i1,i2,iq,iw)   &
&             + 2._DP*DEV_VAR(qr)(2,i3)*f_coeff_p(6,i1,i2,iq,iw))                                        &
&             + DEV_VAR(qr)(2,i3)*(f_coeff_p(3,i1,i2,iq,iw)+DEV_VAR(qr)(2,i3)*f_coeff_p(8,i1,i2,iq,iw)   &
&             + 2._DP*DEV_VAR(qr)(3,i3)*f_coeff_p(9,i1,i2,iq,iw))                                        &
&             + DEV_VAR(qr)(3,i3)*(f_coeff_p(4,i1,i2,iq,iw)+DEV_VAR(qr)(3,i3)*f_coeff_p(10,i1,i2,iq,iw)  &
&             + 2._DP*DEV_VAR(qr)(1,i3)*f_coeff_p(7,i1,i2,iq,iw))                                        &
&             +DEV_VAR(qr)(1,i3)**2*(DEV_VAR(qr)(1,i3)*f_coeff_p(11,i1,i2,iq,iw)+3_DP*DEV_VAR(qr)(2,i3)* &
&              f_coeff_p(12,i1,i2,iq,iw)+3_DP*DEV_VAR(qr)(3,i3)*f_coeff_p(13,i1,i2,iq,iw))               &
&             +DEV_VAR(qr)(2,i3)**2*(DEV_VAR(qr)(2,i3)*f_coeff_p(15,i1,i2,iq,iw)+3_DP*DEV_VAR(qr)(3,i3)* &
&              f_coeff_p(16,i1,i2,iq,iw)+3_DP*DEV_VAR(qr)(1,i3)*f_coeff_p(14,i1,i2,iq,iw))               &
&             +DEV_VAR(qr)(3,i3)**2*(DEV_VAR(qr)(3,i3)*f_coeff_p(19,i1,i2,iq,iw)+3_DP*DEV_VAR(qr)(1,i3)* &
&              f_coeff_p(17,i1,i2,iq,iw)+3_DP*DEV_VAR(qr)(2,i3)*f_coeff_p(18,i1,i2,iq,iw))               &
&             +6*DEV_VAR(qr)(1,i3)*DEV_VAR(qr)(2,i3)*DEV_VAR(qr)(3,i3)*f_coeff_p(20,i1,i2,iq,iw) 
         !
         ! Accumulate Wc
         !
         RIM_acc = RIM_acc + &
&                  rfac*DEV_VAR(vslab)(i3,i1)**2*func*DEV_VAR(vslab)(i3,i2)**2 / &
&                  (1._DP- DEV_VAR(vslab)(i3,i1)*func*DEV_VAR(vslab)(i3,i2) )
         !
       enddo
       !
       aux_RIM_W(iw,i1,i2)=cmplx(RIM_acc,KIND=SP)
       !
     enddo
   enddo
   !
   !DEV_OMP end parallel do
   !
 enddo
 !
 !if (have_gpu) call devxlib_memcpy_d2h(aux_RIM_W,DEV_VAR(aux_RIM_W))
 !
 RIM_W(:,iq,:,:)=aux_RIM_W(:,:,:)
 !
#ifdef _GPU
 YAMBO_FREE_GPU(DEV_VAR(vslab))
#endif
 !
 if (iq>1) return
 !
 ! head q == 0
 !
 do iw=1,Xw%n_freqs
   !
   RIM_acc=0._DP
   RIM_acc_anis=0._DP
   !
   if (iw==1) then
     select case (trim(rimw_type))
     !
     !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     !
     case("semiconductor")
     do i1=1,RIM_n_rand_pts
       !
       call c2a(v_in=qr(:,i1),v_out=qr_rlu(:),mode='ki2a')
       !
       slab_vplane1=sqrt((qr(idir(2),i1)*2._SP*pi/alat(idir(2)))**2 + &
&                        (qr(idir(3),i1)*2._SP*pi/alat(idir(3)))**2)
       !kxy
       r1=sqrt((qr(1,i1)*2._SP*pi/alat(1))**2+(qr(2,i1)*2._SP*pi/alat(2))**2 + &
&              (qr(3,i1)*2._SP*pi/alat(3))**2)
       !
       !Regularization
       if (r1 < 1.e-5) then
         !
         if (cut_is_slab) then
           !
           ! System 2D, prefactor of slab truncation
           !
           vslab(i1,1) = sqrt(4._DP*pi*(1.-exp(-q0_def_norm*lcut))/q0_def_norm**2)
           RIM_acc= RIM_acc + rfac*f_coeff(1,1,1,1,iw)*(4*pi*lcut)**2
           RIM_acc_anis= RIM_acc_anis + rfac*f_coeff(1,1,1,1,iw)*(4.0_DP*pi*lcut)**2 &
&                        *0.5_DP*(em1_anis(idir(2))+em1_anis(idir(3)))
           !
         else
           !
           ! System 3D, standard bare coulomb
           !
           vslab(i1,1) = sqrt(4._DP*pi/q0_def_norm**2)
           RIM_acc= RIM_acc +rfac*f_coeff(1,1,1,1,iw)/(1.0-(4._DP*pi)*f_coeff(1,1,1,1,iw)) &
&                   *(4._DP*pi)**2/q0_def_norm**2
           RIM_acc_anis= RIM_acc_anis + rfac*f_coeff(1,1,1,1,iw)/(1.0_DP-(4._DP*pi) &
                         *f_coeff(1,1,1,1,iw))*(4._DP*pi)**2/q0_def_norm**2      &
&                        *(em1_anis(1)+em1_anis(2)+em1_anis(3))/3.0_SP
           !
         endif
         !
         cycle
       endif
       !
       !Evaluate v_slab and interpolation function
       !
       if (cut_is_slab) then
         !
         vslab2 = 4._DP*pi*(1.-exp(-slab_vplane1*lcut))
         func = f_coeff(1,1,1,1,iw)*exp(sign(1._DP,real(f_coeff(idir(2)+1,1,1,1,iw),DP)) &
&               *sqrt((f_coeff(idir(2)+1,1,1,1,iw)*qr_rlu(idir(2)))**2                   &
&               +(f_coeff(idir(3)+1,1,1,1,iw)*qr_rlu(idir(3)))**2))
         !
       else
         !
         vslab2 = 4._DP*pi
         func = f_coeff(1,1,1,1,iw)*exp(sign(1._DP,real(f_coeff(2,1,1,1,iw),DP))  &
&               *sqrt((f_coeff(2,1,1,1,iw)*qr_rlu(1))**2 + (f_coeff(3,1,1,1,iw)   &
&               *qr_rlu(2))**2 + (f_coeff(4,1,1,1,iw)*qr_rlu(3))**2))
         !
       end if
       !
       !Evaluate W
       RIM_acc = RIM_acc + rfac*vslab2*func*vslab2/(1-vslab2*func)/r1**2
       !
       !Anisotropy contribution
       anis_fact=dot_product(em1_anis,(2._SP*pi*qr(:,i1)/(alat(:)*r1))**2)
       func = func*anis_fact
       RIM_acc_anis = RIM_acc_anis + rfac*vslab2*func*vslab2/(r1**2*(1-vslab2*func))
       !
       !Store the square root of vslab
       vslab(i1,1) = sqrt(vslab2)/r1
       !
     enddo
     !
     !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     !
     case("Dirac")
     do i1=1,RIM_n_rand_pts
       !
       call c2a(v_in=qr(:,i1),v_out=qr_rlu(:),mode='ki2a')
       !
       slab_vplane1=sqrt((qr(idir(2),i1)*2._SP*pi/alat(idir(2)))**2 + &
&                        (qr(idir(3),i1)*2._SP*pi/alat(idir(3)))**2)
       !kxy
       r1=sqrt((qr(1,i1)*2._SP*pi/alat(1))**2+(qr(2,i1)*2._SP*pi/alat(2))**2 + &
&              (qr(3,i1)*2._SP*pi/alat(3))**2)
       !
       if (r1 < 1.e-5) then
         vslab2 = 4._DP*pi/q0_def_norm**2
         !
         ! System 2D, prefactor of slab truncation
         !
         if (cut_is_slab) vslab2 = vslab2*sqrt(1.-exp(-q0_def_norm*lcut))
         !
         func = f_coeff(1,1,1,1,1)*q0_def_norm
         RIM_acc = RIM_acc + rfac*vslab(i1,1)**2*func*vslab(i1,1)**2    &
&                            /(1._DP-vslab(i1,1)**2*func)
         cycle
         !
       endif
       !
       !Evaluate v_slab
       vslab2 = 4._DP*pi/r1**2
       !
       ! System 2D, prefactor of slab truncation
       !
       if (cut_is_slab) vslab2 = vslab2*(1.-exp(-slab_vplane1*lcut))
       !
       !Evaluate func
       !
       func = f_coeff(1,1,1,1,iw)*r1*exp(sign(1._DP,real(f_coeff(idir(2)+1,1,1,1,iw),DP)) &
&               *sqrt((f_coeff(idir(2)+1,1,1,1,iw)*qr_rlu(idir(2)))**2                    &
&               +(f_coeff(idir(3)+1,1,1,1,iw)*qr_rlu(idir(3)))**2))
       !
       RIM_acc = RIM_acc+rfac*vslab2*func*vslab2/(1._DP-vslab2*func)
       !
       !Store the square root of vslab
       vslab(i1,1) = sqrt(vslab2)
       !
     enddo
     !
     !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     !
     case("metal")
     do i1=1,RIM_n_rand_pts
       !
       call c2a(v_in=qr(:,i1),v_out=qr_rlu(:),mode='ki2a')
       !
       func = f_coeff(1,1,1,1,1) + f_coeff(2,1,1,1,1)*abs(qr_rlu(1)) + f_coeff(3,1,1,1,1)  &
&            *abs(qr_rlu(2)) + f_coeff(4,1,1,1,1)*abs(qr_rlu(3)) + f_coeff(5,1,1,1,1)      &
&            *qr_rlu(1)**2 + f_coeff(6,1,1,1,1)*qr_rlu(2)**2 + f_coeff(7,1,1,1,1)*qr_rlu(3)**2
       !
       slab_vplane1=sqrt((qr(idir(2),i1)*2._SP*pi/alat(idir(2)))**2+&
&                          (qr(idir(3),i1)*2._SP*pi/alat(idir(3)))**2)
       !
       r1=sqrt((qr(1,i1)*2._SP*pi/alat(1))**2+(qr(2,i1)*2._SP*pi/alat(2))**2 + &
&             (qr(3,i1)*2._SP*pi/alat(3))**2)
       !
       if (r1 < 1.e-5) then
          !
          vslab(i1,1) = sqrt(4._DP*pi/q0_def_norm**2)
          !
          if (cut_is_slab)  vslab(i1,1) = vslab(i1,1)*sqrt(1.-exp(-q0_def_norm*lcut))
          !
       else
          !
          vslab(i1,1) =sqrt(4._DP*pi/r1**2)
          !
          if (cut_is_slab) vslab(i1,1) = vslab(i1,1)*sqrt(1.-exp(-slab_vplane1*lcut))
          !
       endif
       !
       RIM_acc = RIM_acc + rfac*func*vslab(i1,1)**4/(1._DP-func*vslab(i1,1)**2)
       !
     enddo
     !
     end select
     !
   else
     !
     ! Compute at finite frequency
     !
     select case (trim(rimw_type))
     !
     !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     !
     case("semiconductor","Dirac")
       ! GS: Identical to static case except for anistropy here missing,
       !     merge of the two cases?
     do i1=1,RIM_n_rand_pts
       !
       call c2a(v_in=qr(:,i1),v_out=qr_rlu(:),mode='ki2a')
       !
       slab_vplane1=sqrt((qr(idir(2),i1)*2._SP*pi/alat(idir(2)))**2 + &
&                        (qr(idir(3),i1)*2._SP*pi/alat(idir(3)))**2)
       !kxy
       r1=sqrt((qr(1,i1)*2._SP*pi/alat(1))**2+(qr(2,i1)*2._SP*pi/alat(2))**2 + &
&              (qr(3,i1)*2._SP*pi/alat(3))**2)
       !
       !Regularization
       if (r1 < 1.e-5) then
         if (cut_is_slab) then
           !
           ! System 2D, prefactor of slab truncation
           !
           vslab(i1,1) = sqrt(4._DP*pi*(1.-exp(-q0_def_norm*lcut))/q0_def_norm**2)
           RIM_acc = RIM_acc + rfac*f_coeff(1,1,1,1,iw)*(4*pi*lcut)**2
           !
         else
           !
           ! System 3D, standard bare coulomb
           !
           vslab(i1,1) = sqrt(4._DP*pi/q0_def_norm**2)
           RIM_acc = RIM_acc + rfac*f_coeff(1,1,1,1,iw)/(1.0-(4._DP*pi)* &
&                  f_coeff(1,1,1,1,iw))*(4*pi)**2/q0_def_norm**2
           !
         endif
         !
         cycle
         !
       endif
       !
       !Evaluate v_slab and interpolation function
       !
       if (cut_is_slab) then
         !
         vslab2 = 4._DP*pi*(1.-exp(-slab_vplane1*lcut))
         func = f_coeff(1,1,1,1,iw)*exp(sign(1._DP,real(f_coeff(idir(2)+1,1,1,1,iw),DP)) &
&               *sqrt((f_coeff(idir(2)+1,1,1,1,iw)*qr_rlu(idir(2)))**2                   &
&               +(f_coeff(idir(3)+1,1,1,1,iw)*qr_rlu(idir(3)))**2))
         !
       else
         !
         vslab2 = 4._DP*pi
         func = f_coeff(1,1,1,1,iw)*exp(sign(1._DP,real(f_coeff(2,1,1,1,iw),DP))  &
&               *sqrt((f_coeff(2,1,1,1,iw)*qr_rlu(1))**2 + (f_coeff(3,1,1,1,iw)   &
&               *qr_rlu(2))**2 + (f_coeff(4,1,1,1,iw)*qr_rlu(3))**2))
         !
       endif
       !
       !Evaluate W
       RIM_acc = RIM_acc + rfac*vslab2*func*vslab2/(1-vslab2*func)/r1**2
       !
       !Store the square root of vslab
       vslab(i1,1) = sqrt(vslab2)/r1
       !
     enddo
     !
     !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     !
     case("metal")
     !
     do i1=1,RIM_n_rand_pts
       call c2a(v_in=qr(:,i1),v_out=qr_rlu(:),mode='ki2a')
       !
       r1=sqrt((qr(1,i1)*2._SP*pi/alat(1))**2+(qr(2,i1)*2._SP*pi/alat(2))**2 + &
&              (qr(3,i1)*2._SP*pi/alat(3))**2)
       !
       slab_vplane1=sqrt((qr(idir(2),i1)*2._SP*pi/alat(idir(2)))**2 + &
&                        (qr(idir(3),i1)*2._SP*pi/alat(idir(3)))**2)
       !
       func =  f_coeff(1,1,1,1,iw)
       !
       if (r1 < 1.e-5) then
         !
         if (cut_is_slab) then
           !
           ! System 2D, prefactor of slab truncation
           !
           vslab(i1,1) = sqrt(4._DP*pi*(1.0_DP-exp(-q0_def_norm*lcut))/q0_def_norm**2)
           RIM_acc = RIM_acc + rfac*func*(4.0_DP*pi*lcut)**2
           !
         else
           !
           ! System 3D, standard bare coulomb
           !
           vslab(i1,1) = sqrt(4._DP*pi/q0_def_norm**2)
           RIM_acc = RIM_acc + rfac*func/(1.0_DP-(4._DP*pi)*func)*(4*pi)**2/q0_def_norm**2
           !
         endif
         !
         cycle
         !
       endif
       !
       vslab2 = 4._DP*pi
       !
       if (cut_is_slab) then
         vslab2 = vslab2*(1.-exp(-slab_vplane1*lcut))
         func = (func+f_coeff(idir(2)+4,1,1,1,iw)*abs(qr_rlu(idir(2)))+f_coeff(idir(3)+4,1,1,1,iw)    &
&               *abs(qr_rlu(idir(3))))*exp(sign(1._DP,real(f_coeff(idir(2)+1,1,1,1,iw),DP))           &
&               *sqrt((f_coeff(idir(2)+1,1,1,1,iw)*qr_rlu(idir(2)))**2+(f_coeff(idir(3)+1,1,1,1,iw)   &
&               *qr_rlu(idir(3)))**2))
         !
       else
         !
         func = (func+f_coeff(5,1,1,1,iw)*abs(qr_rlu(1)) + f_coeff(6,1,1,1,iw)*abs(qr_rlu(2))      &
&               + f_coeff(7,1,1,1,iw)*abs(qr_rlu(3)))*exp(sign(1._DP,real(f_coeff(3,1,1,1,iw),DP)) &
&               * sqrt((f_coeff(2,1,1,1,iw)*qr_rlu(1))**2 + (f_coeff(3,1,1,1,iw)*qr_rlu(2))**2     &
&               + (f_coeff(4,1,1,1,iw)*qr_rlu(3))**2))
         !
       endif
       !
       !Evaluate W
       RIM_acc = RIM_acc + rfac*vslab2*func*vslab2/(1-vslab2*func)/r1**2
       !
       !Store the square root of vslab
       vslab(i1,1) = sqrt(vslab2)/r1
       !
     enddo
     !
     end select
     !
   endif
   !
   if (RIM_id_epsm1_reference == 0) then
     RIM_W(iw,1,1,1)=RIM_acc
   else
     RIM_W(iw,1,1,1)=RIM_acc_anis
   endif
   !
 enddo
 !
 !if (RIM_W_is_diagonal) return
 !
 ! wings q == 0
 !
 do iw=1,Xw%n_freqs
   do i2=2,RIM_W_ng
    do i1=0,1 ! loop on both wings
     !
     s1 = i1 - i2 * (i1 - 1)
     s2 = i2 * i1 + (1 - i1)
     !
     RIM_acc=0._DP
     !
     do i3=1,RIM_n_rand_pts
        !
        call c2a(v_in=qr(:,i3),v_out=qr_rlu(:),mode='ki2a')
        !
        r1=sqrt((qr(1,i3)*2._SP*pi/alat(1))**2+(qr(2,i3)*2._SP*pi/alat(2))**2 + &
&              (qr(3,i3)*2._SP*pi/alat(3))**2)
        !
        if ( r1 < 1e-5) then
          !
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ! Small q behaviour differs for metals and semiconductors
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          !
          select case (trim(rimw_type))
          !
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          !
          case("metal")
          !
          if (cut_is_slab) then
            slab_vz1=g_vec(i2,idir(1))*2._SP*pi/alat(idir(1))
            slab_vplane1=sqrt((g_vec(i2,idir(2))*2._SP*pi/alat(idir(2)))**2 + &
&                         (g_vec(i2,idir(3))*2._SP*pi/alat(idir(3)))**2)
            if (slab_vplane1 < q0_def_norm) slab_vplane1 = q0_def_norm
            vslab2 = 4._DP*pi*(1.-exp(-slab_vplane1*lcut)*cos(slab_vz1*lcut))**0.5
            !
            do j=1,2
              ! Load q_grid_b in iku
              qtmp(:)=q_grid_b_iku(idir(1+j),:)
              !
              if (iw ==1) then
                ! System 2D, real part of f func behaves as fcoeff*q, if imag present as vec(q)
                func = (real(f_coeff(2,s1,s2,iq,iw))*abs(q_grid_b_rlu(idir(1+j),1))  + &
&                       real(f_coeff(3,s1,s2,iq,iw))*abs(q_grid_b_rlu(idir(1+j),2))  + &
&                       real(f_coeff(4,s1,s2,iq,iw))*abs(q_grid_b_rlu(idir(1+j),3))  + &
&                       aimag(f_coeff(2,s1,s2,iq,iw))*q_grid_b_rlu(idir(1+j),1)  + &
&                       aimag(f_coeff(3,s1,s2,iq,iw))*q_grid_b_rlu(idir(1+j),2)  + &
&                       aimag(f_coeff(4,s1,s2,iq,iw))*q_grid_b_rlu(idir(1+j),3))   &
&                      /iku_v_norm(qtmp(:))
                !
                RIM_acc = RIM_acc + rfac*vslab2**2*func/(slab_vplane1**2+slab_vz1**2)/2.0_SP/q0_def_norm
                !
              else
                if ((g_vec(i2,idir(2))==g_vec(i2,idir(3))) .and. (g_vec(i2,idir(2))==0)) then
                  func = ((f_coeff(2,s1,s2,iq,iw)-f_coeff(5,s1,s2,iq,iw))*q_grid_b_rlu(idir(1+j),1)**2 + &
&                        (f_coeff(3,s1,s2,iq,iw)-f_coeff(6,s1,s2,iq,iw))*q_grid_b_rlu(idir(1+j),2)**2  + &
&                        (f_coeff(4,s1,s2,iq,iw)-f_coeff(7,s1,s2,iq,iw))*q_grid_b_rlu(idir(1+j),3)**2 )  &
&                        /iku_v_norm(qtmp(:))**2/2.0_SP
                  !
                  ! AF: is this correct ??
                  ! GS: yes, the out of plane wings do not contribute. In hindsight, this part is superfluous
                  !
                  RIM_acc = RIM_acc
                  !
                else
                  func = ((f_coeff(2,s1,s2,iq,iw)-f_coeff(5,s1,s2,iq,iw))*abs(q_grid_b_rlu(idir(1+j),1))  + &
&                        (f_coeff(3,s1,s2,iq,iw)-f_coeff(6,s1,s2,iq,iw))*abs(q_grid_b_rlu(idir(1+j),2))   + &
&                        (f_coeff(4,s1,s2,iq,iw)-f_coeff(7,s1,s2,iq,iw))*abs(q_grid_b_rlu(idir(1+j),3)) )   &
&                        /iku_v_norm(qtmp(:))/2.0_SP
                  !
                  RIM_acc = RIM_acc + rfac*vslab2**2*func/(slab_vplane1**2+slab_vz1**2) &
&                          /2.0_SP/q0_def_norm
                endif
              endif
              !
            enddo
            !
          else
            !
            ! System 3D, f func behaves as f = fcoeff*q^2
            ! we loop on 3D directions as fcoeff may be anisotropic
            !
            do j=1,3
              ! Load q_grid_b in iku
              qtmp(:)=q_grid_b_iku(j,:)
              !
              func = (qtmp(1)*(qtmp(1)*f_coeff(5,s1,s2,iq,iw) + 2._SP*qtmp(2)*           &
&                     f_coeff(6,s1,s2,iq,iw)) + qtmp(2)*(qtmp(2)*f_coeff(8,s1,s2,iq,iw) &
&                     + 2._SP*qtmp(3)*f_coeff(9,s1,s2,iq,iw)) +qtmp(3)*(qtmp(3)          &
&                     *f_coeff(10,s1,s2,iq,iw) + 2._SP*qtmp(1)*f_coeff(7,s1,s2,iq,iw)))  &
&                     /iku_v_norm(qtmp(:))**2
              !
              slab_vplane1=sqrt((g_vec(i2,1)*2.0_SP*pi/alat(1))**2 + (g_vec(i2,2)*2.0_SP*pi &
&                               /alat(2))**2 + (g_vec(i2,3)*2.*pi/alat(3))**2)
              !
              RIM_acc = RIM_acc + rfac*(4._SP*pi)**2*func/slab_vplane1**2/3.0
              !
            enddo
            !
          endif
          !
          cycle
          !
          end select
          !
        endif
        !
        !Evaluate func
        !
        func = f_coeff(1,s1,s2,iq,iw)                                             &
&             + qr(1,i3)*(f_coeff(2,s1,s2,iq,iw)+qr(1,i3)*f_coeff(5,s1,s2,iq,iw)  &
&             + 2._DP*qr(2,i3)*f_coeff(6,s1,s2,iq,iw))                            &
&             + qr(2,i3)*(f_coeff(3,s1,s2,iq,iw)+qr(2,i3)*f_coeff(8,s1,s2,iq,iw)  &
&             + 2._DP*qr(3,i3)*f_coeff(9,s1,s2,iq,iw))                            &
&             + qr(3,i3)*(f_coeff(4,s1,s2,iq,iw)+qr(3,i3)*f_coeff(10,s1,s2,iq,iw) &
&             + 2._DP*qr(1,i3)*f_coeff(7,s1,s2,iq,iw))                            &
&             +qr(1,i3)**2*(qr(1,i3)*f_coeff(11,s1,s2,iq,iw)+3_DP*qr(2,i3)*       &
&              f_coeff(12,s1,s2,iq,iw)+3_DP*qr(3,i3)*f_coeff(13,s1,s2,iq,iw))     &
&             +qr(2,i3)**2*(qr(2,i3)*f_coeff(15,s1,s2,iq,iw)+3_DP*qr(3,i3)*       &
&              f_coeff(16,s1,s2,iq,iw)+3_DP*qr(1,i3)*f_coeff(14,s1,s2,iq,iw))     &
&             +qr(3,i3)**2*(qr(3,i3)*f_coeff(19,s1,s2,iq,iw)+3_DP*qr(1,i3)*       &
&              f_coeff(17,s1,s2,iq,iw)+3_DP*qr(2,i3)*f_coeff(18,s1,s2,iq,iw))     &
&             +6*qr(1,i3)*qr(2,i3)*qr(3,i3)*f_coeff(20,s1,s2,iq,iw)
        !
        select case (trim(rimw_type))
        !
        case("metal")
        !
        if (cut_is_slab) then
          !
          if (iw ==1) then
            func = real(f_coeff(2,s1,s2,iq,iw))*abs(qr_rlu(1)) + real(f_coeff(3,s1,s2,iq,iw))*abs(qr_rlu(2)) &
&                + real(f_coeff(4,s1,s2,iq,iw))*abs(qr_rlu(3)) + aimag(f_coeff(2,s1,s2,iq,iw))*qr_rlu(1)     &
&                + aimag(f_coeff(3,s1,s2,iq,iw))*qr_rlu(2) + aimag(f_coeff(4,s1,s2,iq,iw))*qr_rlu(3)         &
&                + f_coeff(5,i2,1,iq,iw)*(qr_rlu(1))**2 + f_coeff(6,i2,1,iq,iw)*(qr_rlu(2))**2               &
&                + f_coeff(7,i2,1,iq,iw)*(qr_rlu(3))**2 + f_coeff(8,i2,1,iq,iw)                              &
&                * sign(qr_rlu(1)**2,qr_rlu(1)) + f_coeff(9,i2,1,iq,iw)*sign(qr_rlu(2)**2,qr_rlu(2))         &
&                + f_coeff(10,i2,1,iq,iw)*sign(qr_rlu(3)**2,qr_rlu(3))
          else
            !
            if ( (g_vec(i2,idir(2))==g_vec(i2,idir(3))) .and. (g_vec(i2,idir(2))==0)) then
              func = ((f_coeff(2,s1,s2,iq,iw)+f_coeff(5,s1,s2,iq,iw)+ (f_coeff(2,s1,s2,iq,iw)    &
&                    -f_coeff(5,s1,s2,iq,iw))*sign(1._SP,qr_rlu(1)))*qr_rlu(1)**2    +           &
&                     (f_coeff(3,s1,s2,iq,iw)+f_coeff(6,s1,s2,iq,iw)+ (f_coeff(3,s1,s2,iq,iw)    &
&                    -f_coeff(6,s1,s2,iq,iw))*sign(1._SP,qr_rlu(2)))*qr_rlu(2)**2    +           &
&                     (f_coeff(4,s1,s2,iq,iw)+f_coeff(7,s1,s2,iq,iw)+ (f_coeff(4,s1,s2,iq,iw)    &
&                    -f_coeff(7,s1,s2,iq,iw))*sign(1._SP,qr_rlu(3)))*qr_rlu(3)**2    +           &
&                     (f_coeff(8,s1,s2,iq,iw)+f_coeff(11,s1,s2,iq,iw)+ (f_coeff(8,s1,s2,iq,iw)   &
&                    -f_coeff(11,s1,s2,iq,iw))*sign(1._SP,qr_rlu(1)))*qr_rlu(1)**3   +           &
&                     (f_coeff(9,s1,s2,iq,iw)+f_coeff(12,s1,s2,iq,iw)+ (f_coeff(9,s1,s2,iq,iw)   &
&                    -f_coeff(12,s1,s2,iq,iw))*sign(1._SP,qr_rlu(2)))*qr_rlu(2)**3   +           &
&                     (f_coeff(10,s1,s2,iq,iw)+f_coeff(13,s1,s2,iq,iw)+ (f_coeff(10,s1,s2,iq,iw) &
&                    -f_coeff(13,s1,s2,iq,iw))*sign(1._SP,qr_rlu(3)))*qr_rlu(3)**3)/2.0_SP
            else
              !
              func = ((f_coeff(2,s1,s2,iq,iw)+f_coeff(5,s1,s2,iq,iw)+ (f_coeff(2,s1,s2,iq,iw)    &
&                    -f_coeff(5,s1,s2,iq,iw))*sign(1._SP,qr_rlu(1)))*qr_rlu(1)       +           &
&                     (f_coeff(3,s1,s2,iq,iw)+f_coeff(6,s1,s2,iq,iw)+ (f_coeff(3,s1,s2,iq,iw)    &
&                    -f_coeff(6,s1,s2,iq,iw))*sign(1._SP,qr_rlu(2)))*qr_rlu(2)       +           &
&                     (f_coeff(4,s1,s2,iq,iw)+f_coeff(7,s1,s2,iq,iw)+ (f_coeff(4,s1,s2,iq,iw)    &
&                    -f_coeff(7,s1,s2,iq,iw))*sign(1._SP,qr_rlu(3)))*qr_rlu(3)       +           &
&                     (f_coeff(8,s1,s2,iq,iw)+f_coeff(11,s1,s2,iq,iw)+ (f_coeff(8,s1,s2,iq,iw)   &
&                    -f_coeff(11,s1,s2,iq,iw))*sign(1._SP,qr_rlu(1)))*qr_rlu(1)**2   +           &
&                     (f_coeff(9,s1,s2,iq,iw)+f_coeff(12,s1,s2,iq,iw)+ (f_coeff(9,s1,s2,iq,iw)   &
&                    -f_coeff(12,s1,s2,iq,iw))*sign(1._SP,qr_rlu(2)))*qr_rlu(2)**2   +           &
&                     (f_coeff(10,s1,s2,iq,iw)+f_coeff(13,s1,s2,iq,iw)+ (f_coeff(10,s1,s2,iq,iw) &
&                    -f_coeff(13,s1,s2,iq,iw))*sign(1._SP,qr_rlu(3)))*qr_rlu(3)**2)/2.0_SP
            endif
            !
          endif
          !
        endif
        !
        end select
        !
        r1=sqrt((qr(1,i3)*2.*pi/alat(1))**2+(qr(2,i3)*2.*pi/alat(2))**2 + &
&              (qr(3,i3)*2.*pi/alat(3))**2)
        slab_vplane1=sqrt((qr(idir(2),i3)*2.*pi/alat(idir(2)))**2 + &
&                          (qr(idir(3),i3)*2.*pi/alat(idir(3)))**2)
        !
        if (r1 < 1.e-5) then
          !
          vslab(i3,1) = sqrt(4._DP*pi/q0_def_norm**2)
          !
          if (cut_is_slab)  vslab(i3,1) = vslab(i3,1)*sqrt(1.-exp(-q0_def_norm*lcut))
          !
        else
          !
          vslab(i3,1) =sqrt(4._DP*pi/r1**2)
          !
          if (cut_is_slab) vslab(i3,1) = vslab(i3,1)*sqrt(1.-exp(-slab_vplane1*lcut))
          !
        endif
        !
        ! Accumulate Wc
        !
        RIM_acc = RIM_acc + rfac*vslab(i3,1)**2*func*vslab(i3,i2)**2/(1._DP-  &
&                  vslab(i3,1)*func*vslab(i3,i2))
        !
     enddo
     RIM_W(iw,iq,s1,s2)=RIM_acc
     ! old version symmetrization RIM_W(iw,iq,i2,1)=RIM_W(iw,iq,s1,s2)
    end do 
   enddo
   !
 enddo
 !
 YAMBO_FREE(vslab)
 !
end subroutine

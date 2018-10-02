!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine fft_setup(iG_max,iGo_max,ONLY_SIZE)
 !
 use pars,           ONLY:SP,pi
 use vec_operate,    ONLY:c2a
 use D_lattice,      ONLY:a,nsym,dl_sop,sop_inv,i_time_rev,alat
 use R_lattice,      ONLY:b,g_vec,ng_vec
 use matrix_operate, ONLY:m3inv
 use FFT_m,          ONLY:fft_dim,fft_size,fft_rot_r,fft_rot_r_inv,&
&                         fft_norm,fft_g_table,fft_multiplier,modx
#if defined _CUDA
 use FFT_m,          ONLY:fft_g_table_d,fft_rot_r_d,fft_rot_r_inv_d
#endif
#if defined _FFTW
 use FFT_m,          ONLY:fftw_plan
#endif
#if defined _FFTQE
 use fft_base,       ONLY:dffts
#endif
 use wave_func,      ONLY:wf_ng
 use timing_m,       ONLY:timing
#include<memory.h>
 !
 integer :: iG_max,iGo_max
 logical :: ONLY_SIZE
#if defined _FFTQE
 integer, external :: fft_setmap
#endif
 ! 
 ! Work Space
 !
 integer  :: i1,i2,i3,i4,iv(3),ln(3),is,space_inv(3,3)
 real(SP) :: v1(3),M1(3,3),M2(3,3),mat(3,3),scal(3)
 real(SP), allocatable :: g_vec_rot(:,:)

 call timing("FFT_setup",opr="start")
 !
 space_inv=reshape((/-1, 0, 0, 0,-1, 0, 0, 0,-1/),(/3,3/))
 call m3inv(transpose(b),mat)
 scal(:)=2._SP*pi/alat(:)
 ! 
 mat(:,1)=scal(1)*mat(:,1)
 mat(:,2)=scal(2)*mat(:,2)
 mat(:,3)=scal(3)*mat(:,3)
 !
 allocate(g_vec_rot,mold=g_vec)
 g_vec_rot=transpose(matmul(mat,transpose(g_vec)))
 !
 if (.not.ONLY_SIZE) then
   if (allocated(fft_g_table)) then
     YAMBO_FREE(fft_g_table)
   endif
   YAMBO_ALLOC(fft_g_table,(max(iG_max,wf_ng),iGo_max))
 endif
 !
 ! SIZE estimation
 !
 ln=-1
 i4=-1
 if (.not.ONLY_SIZE) fft_g_table=0
 do while(.true.)
   !
#if defined _FFTQE
   if (i4>0.and..not.ONLY_SIZE) call fft_desc_init(fft_dim,iGo_max,dffts)
#endif
   !
   do i1=1,min(max(iG_max,wf_ng),ng_vec)
     do i2=1,iGo_max
       !
       v1=(g_vec_rot(i1,:)-g_vec_rot(i2,:))
       iv=nint(v1)
       !
       if (i2==1.or.i1<=iG_max) forall(i3=1:3) ln(i3)=max(ln(i3),iv(i3))
#if defined _FFTQE
       if (i4>0.and..not.ONLY_SIZE) fft_g_table(i1,i2)=fft_setmap(iv,dffts)
#else
       if (i4>0.and..not.ONLY_SIZE) fft_g_table(i1,i2)=1+modx(iv(1),fft_dim(1))+&
&                                   modx(iv(2),fft_dim(2))*fft_dim(1)+&
&                                   modx(iv(3),fft_dim(3))*fft_dim(1)*fft_dim(2)
#endif
     enddo
   enddo
   !
#ifdef _CUDA
   if (i4>0.and..not.ONLY_SIZE) then
     if (allocated(fft_g_table_d)) deallocate(fft_g_table_d)
     allocate(fft_g_table_d,source=fft_g_table)
   endif
#endif
   !
   ln=ln*fft_multiplier
   !
   call fft_best_size(ln)
   if (i4>0) then
      call timing("FFT_setup",opr="stop")
      if (allocated(g_vec_rot)) deallocate(g_vec_rot)
      return
   endif
   fft_dim=ln
   fft_size=product(fft_dim)
   fft_norm=sqrt(1._SP/real(fft_size,SP))
   if (ONLY_SIZE) then
     i4=1
     cycle
   endif
   !
   if (allocated(fft_rot_r)) then
     YAMBO_FREE(fft_rot_r)
   endif
   YAMBO_ALLOC(fft_rot_r,(fft_size,nsym))
   !
   YAMBO_FREE(fft_rot_r_inv)
   YAMBO_ALLOC(fft_rot_r_inv,(fft_size))

   !
   !Remember
   !-------
   ! 
   ! r_j= (I_i-1)/Ni a(j,i) = at(j,i) (i-1)/Ni
   !
   ! at=transpose(a)
   !
   ! a(i,j)*b(k,j)=b(k,j)*at(j,i)=d_ik 2 pi 
   ! atm1=inverse(transpose(a))=b/2./pi
   !
   ! r(s)_j=at(i,l) (I_l-1)/Nl = (R_s r)_i = R_s(i,k) at(k,j) (j-1)/Nj 
   !
   ! => (I_l-1)/Nl = atm1(l,i) R_s(i,k) at(k,j) (j-1)/Nj
   ! 
   ! Now I want to rewrite wf_ks(r)= wf_k(r(s^-1))
   !
   do is=1,nsym+1
     if( is<=nsym/(1+i_time_rev) )                M1=matmul( dl_sop(:,:,sop_inv(is)),transpose(a))
     if( is> nsym/(1+i_time_rev) .and. is<=nsym ) M1=matmul(-dl_sop(:,:,sop_inv(is)),transpose(a))
     if( is==nsym+1 )                             M1=matmul( space_inv,transpose(a))
     M2=matmul(b,M1)/2.0_SP/pi
     forall (i1=1:3,i2=1:3) M2(i1,i2)=M2(i1,i2)*fft_dim(i1)/fft_dim(i2)
     !
     do i1=0,fft_dim(1)-1
       do i2=0,fft_dim(2)-1
         do i3=0,fft_dim(3)-1
           iv=nint(matmul(M2,(/i1,i2,i3/)))
           i4=1+i1+i2*fft_dim(1)+i3*fft_dim(1)*fft_dim(2)
           if( is==nsym+1) then
             fft_rot_r_inv(i4)=1+modx(iv(1),fft_dim(1))+&
&                            modx(iv(2),fft_dim(2))*fft_dim(1)+&
&                            modx(iv(3),fft_dim(3))*fft_dim(1)*fft_dim(2)
             cycle
           endif
           fft_rot_r(i4,is)=1+modx(iv(1),fft_dim(1))+&
&                          modx(iv(2),fft_dim(2))*fft_dim(1)+&
&                          modx(iv(3),fft_dim(3))*fft_dim(1)*fft_dim(2)
         enddo
       enddo
     enddo
   enddo
   i4=1
#if defined _CUDA
   if (allocated(fft_rot_r_d)) deallocate(fft_rot_r_d)
   if (allocated(fft_rot_r_inv_d)) deallocate(fft_rot_r_inv_d)
   allocate(fft_rot_r_d,source=fft_rot_r)
   allocate(fft_rot_r_inv_d,source=fft_rot_r_inv)
#endif
 enddo
 ! 
#if defined _FFTW
 fftw_plan=0
#endif
 !
 if (allocated(g_vec_rot)) deallocate(g_vec_rot)
 call timing("FFT_setup",opr="stop")
 !
 contains
   !
   subroutine fft_best_size(test_fft_size)
     !
#if defined _FFTQE
     use fft_scalar, ONLY:good_fft_order
#endif
     !
     implicit none
     integer :: test_fft_size(3)
     ! 
     ! Work Space
     !
     integer, parameter :: nn=82
     integer :: i1,i2,nallwd(nn)
     data nallwd/& ! taken from CTRIG
&      3,   4,   5,   6,   8,   9,  12,  15,  16,  18,&
&     20,  24,  25,  27,  30,  32,  36,  40,  45,  48,&
&     54,  60,  64,  72,  75,  80,  81,  90,  96, 100,&
&    108, 120, 125, 128, 135, 144, 150, 160, 162, 180,&
&    192, 200, 216, 225, 240, 243, 256, 270, 288, 300,&
&    320, 324, 360, 375, 384, 400, 405, 432, 450, 480,&
&    486, 500, 512, 540, 576, 600, 625, 640, 648, 675,&
&    720, 729, 750, 768, 800, 810, 864, 900, 960, 972,&
&    1000,1024/
     !
     ! The size is calculated on the components of the RL vectors
     ! that are positive and negative. Thus I need 2N+1 elements
     !
     test_fft_size=2*test_fft_size+1
     !
#if defined _FFTQE
     ! use the good_fft_order from the quantum-espresso suite
     do i1=1,3
       test_fft_size(i1)=good_fft_order(test_fft_size(i1))
     enddo
#endif
     !
#if !defined _FFTQE && defined _FFTW
     ! the standard FFTW distribution works most efficiently for arrays
     ! whose size can be factored into small primes (2, 3, 5, and 7),
     ! and otherwise it uses a slower general-purpose routine
     do i1=1,3
       if (any((/mod(test_fft_size(i1),2),mod(test_fft_size(i1),3),&
&                mod(test_fft_size(i1),5),mod(test_fft_size(i1),7)/)==0)) cycle
       test_fft_size(i1)=test_fft_size(i1)+mod(test_fft_size(i1),2)
     enddo
#endif
     !
#if !defined _FFTQE && defined _FFTSG
     do i1=1,3
       do i2=1,nn
         if (nallwd(i2)>=test_fft_size(i1)) then
           test_fft_size(i1)=nallwd(i2)
           exit
         endif
       enddo
      if (test_fft_size(i1)>nallwd(nn)) test_fft_size(i1)=nallwd(nn)
     enddo
#endif
     !
#if !defined _FFTQE && !defined _FFTSG && !defined _FFTW
#error "[FFT] inconsistent FFT lib config"
#endif
     !
   end subroutine
   !
end subroutine

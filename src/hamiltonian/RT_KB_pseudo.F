!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine RT_KB_pseudo(kbv,kbv_r0,kbv_r1,kbv_r2)
 !
 use pars,          ONLY:SP,DP,cZERO,cI
 use units,         ONLY:SPEED_OF_LIGHT
 use electrons,     ONLY:n_spinor
 use wave_func,     ONLY:wf_ng
 use FFT_m,         ONLY:fft_size,fft_dim,fft_g_table,fft_norm,fftw_plan
 use pseudo,        ONLY:pp_kbv_dim,pp_n_l_times_proj_max,pp_table
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species,n_atoms,atom_pos,x_cc
 use vec_operate,   ONLY:c2a
 use com,           ONLY:warning
 !
#include<memory.h>
 !
 complex(SP)             :: kbv(wf_ng,pp_kbv_dim,4,n_spinor),         &
&                           kbv_r0(fft_size,pp_kbv_dim,n_spinor),     &
&                           kbv_r1(fft_size,3,pp_kbv_dim,n_spinor),   &
&                           kbv_r2(fft_size,3,3,pp_kbv_dim,n_spinor)
 !
 ! Work Space
 !
 complex(DP)             :: kbv_DP(fft_size)
 complex(SP)             :: gauge_factor(fft_size)
 real(SP)                :: int_A_dl(fft_size),r_atom_pos(fft_size),r_x(fft_size)
 real(SP)                :: cos_delta_phi(fft_size),sin_delta_phi(fft_size)
 real(SP)                :: Phi_over_Phio,atom_pos_alat(3)
 !
 ! Dummies
 integer                 :: i1,is,ia,il,im,i_pp,pp_spin,i_spinor,i_dir,j_dir
 !
 if (n_spinor>1) call warning('new kb implementation to be tested for the case n_spinor>1')
 !
 kbv_r0=cZERO
 kbv_r1=cZERO
 kbv_r2=cZERO
 !
 i1=0
 !
 do is = 1,n_atomic_species
   do ia = 1,n_atoms_species(is)
     !
     call c2a(v_in=atom_pos(:,ia,is),v_out=atom_pos_alat,mode='rc2a')
     !
     !call load_cc(.false.,atom_pos_alat)
     call load_cc(.false.,(/0._SP,0._SP,0._SP/))
     !
     do i_spinor=1,n_spinor
       ! 
       do i_pp = 1, pp_n_l_times_proj_max
         il = pp_table(1,is,i_pp)
         if(il==0) cycle
         pp_spin = pp_table(3,is,i_pp)   ! neglect the so-term for abinit KB
         if(pp_spin==0) cycle
         if(pp_spin>1)  cycle
         !
         do im = 1,2*(il-1)+1
           !
           i1=i1+2-i_spinor
           !
           kbv_DP=(0._DP,0._DP)
           kbv_DP(fft_g_table(1:wf_ng,1))=cmplx(kbv(1:wf_ng,i1,1,i_spinor)*fft_norm,kind=DP)
           !
           ! Here I need to fourier tranform kbv(G,i1,1)
#if defined _FFTW
           fftw_plan = 0
           call fft_3d(kbv_DP,fft_dim,1,fftw_plan)
#else
           call fft_3d(kbv_DP,fft_dim,1)
#endif
           !
           kbv_r0(:,i1,i_spinor)=cmplx(kbv_DP(:),kind=SP)
           !
           do i_dir=1,3
             kbv_DP=(0._DP,0._DP)
             kbv_DP(fft_g_table(1:wf_ng,1))=cmplx(kbv(1:wf_ng,i1,1+i_dir,i_spinor)*fft_norm,kind=DP)
             !
             ! Here I need to fourier tranform kbv(G,i1,1+i_dir)
#if defined _FFTW
             fftw_plan = 0
             call fft_3d(kbv_DP,fft_dim,1,fftw_plan)
#else
             call fft_3d(kbv_DP,fft_dim,1)
#endif
             kbv_r1(:,i_dir,i1,i_spinor)=cmplx(kbv_DP(:),kind=SP)
             !kbv_r1(:,i_dir,i1,i_spinor)=-cI*kbv_DP(:)*x_cc(:,i_dir)
             do j_dir=1,3
               kbv_r2(:,j_dir,i_dir,i1,i_spinor)=kbv_DP(:)*x_cc(:,i_dir)*x_cc(:,j_dir)
             enddo
           enddo
           !       
         enddo   ! im
       enddo   ! i_pp
     enddo   ! i_spinor
     !
     YAMBO_FREE(x_cc)
     !
   enddo   ! ia
 enddo  ! is
 !
end subroutine RT_KB_pseudo

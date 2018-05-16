!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine Pseudo_KB_FFT(kbv,kbv_real_space)
 !
 use pars,          ONLY:SP,DP,cI
 use electrons,     ONLY:n_spinor
 use wave_func,     ONLY:wf_ng
 use FFT_m,         ONLY:fft_size,fft_dim,fft_g_table,fft_norm,fftw_plan
 use pseudo,        ONLY:pp_kbv_dim,pp_n_l_times_proj_max,pp_table
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species,n_atoms,atom_pos,x_cc,a
 use com,           ONLY:warning
 !
#include<memory.h>
 !
 complex(SP)             :: kbv(wf_ng,pp_kbv_dim,4,n_spinor),kbv_real_space(fft_size,n_spinor,pp_kbv_dim)
 !
 ! Work Space
 !
 complex(DP)             :: kbv_DP(fft_size)
 !
 ! Dummies
 !
 integer                 :: i1,is,ia,il,im,i_pp,pp_spin,i_spinor
 !
 if (n_spinor>1) call warning('new kb implementation to be tested for the case n_spinor>1')
 !
 i1=0
 !
 do is = 1,n_atomic_species
   do ia = 1,n_atoms_species(is)
     do i_pp = 1, pp_n_l_times_proj_max
       il = pp_table(1,is,i_pp)
       pp_spin = pp_table(3,is,i_pp)   ! neglect the so-term for abinit KB
       if(pp_spin>1) cycle
       !
       do im = 1,2*(il-1)+1
         !
         do i_spinor=1,n_spinor
           !
           i1=i1+2-i_spinor
           !
           kbv_DP=(0._DP,0._DP)
           kbv_DP(fft_g_table(1:wf_ng,1))=conjg(kbv(1:wf_ng,i1,1,i_spinor))*fft_norm
           !
           ! Here I need to fourier tranform kbv(G,i1,1)
#if defined _FFTW
           fftw_plan = 0
           call fft_3d(kbv_DP,fft_dim,1,fftw_plan)
#else
           call fft_3d(kbv_DP,fft_dim,1)
#endif
           !
           kbv_real_space(:,i_spinor,i1)=conjg(cmplx(kbv_DP(:),kind=SP))
           !
         enddo   ! i_spinor
       enddo   ! im
     enddo   ! i_pp
     !
   enddo   ! ia
 enddo  ! is
 !
end subroutine Pseudo_KB_FFT
